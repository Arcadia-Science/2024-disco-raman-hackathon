require(cowplot)
require(reshape2)
require(parallel)
require(glmnet)
require(pbapply)
require(tidyverse)
require(ggplot2)
require(dplyr)

# A function to get the median profile of the Raman spectra
get_median_profile <-
  function(spectral_data) {
    ## Function to get the median profile of the Raman spectra
    ## Parameters:
    ## spectral_data: data.frame containing three key columns:
    ## - "id" (sample ID - not replicate)
    ## - "Pixel" (i.e. wave number)
    ## - "Intensity" (spectral intensity)
    ## Returns:
    ## A matrix containing the median profile of the Raman spectra

    # Summarize across replicates to get the median pixel
    # (wave number) for each sample
    median_intensity <- spectral_data |>
    group_by(id, Pixel) |> # nolint
    summarize(
      Pixel = mean(Pixel),
      MedianIntensity = median(Intensity), # nolint
      .groups = "drop"
    )

    # Now prepare for analysis with Lasso regression
    median_profile <-
      dcast(median_intensity, id ~ Pixel, value.var = "MedianIntensity")
    rownames(median_profile) <- median_profile$id
    median_profile <- median_profile[, -1]

    # Return a matrix for compatibility with glmnet
    return(list(median_intensity = median_intensity,
                predictors = as.matrix(median_profile)))
  }

# A function to run the Lasso regression with bootstrapping
run_lasso_bootstrap <-
  function(predictors, response_var, n_bs_reps, sample_size, nfolds = 10) {
    # Conduct bootstrap replicates in parallel with a progress bar (pblapply)
    lasso_res <-
      pblapply(1:n_bs_reps, function(x) {
        # Initialize a flag to manage error-handling (in the case of imbalanced
        # training/test sets)
        success_flag <- FALSE

        # Loop until the code runs without error
        while (!success_flag) {
          tryCatch({
            # Split data into test and training sets
            train_indices <-
              sort(sample(seq_len(nrow(predictors)), size = sample_size))
            train_predictors <- predictors[train_indices, ]
            test_predictors <- predictors[-train_indices, ]
            train_response <- response_var[train_indices]
            test_response <- response_var[-train_indices]

            cv_lasso <- NULL
            suppressWarnings(
              cv_lasso <-
                cv.glmnet(x = train_predictors, y = train_response,
                          grouped = FALSE, nfolds = nfolds)
            )
            # If we don't hit an error error, exit the loop
            if (!is.null(cv_lasso)) {
              success_flag <- TRUE
            }
          }, error = function(e) {
            # If we do, print the error message and continue the loop
            print(paste("Error occurred:", e$message))
          })
        }
        # Now summarize, obtaining the best lambda from the cross validation,
        # optimal coefficients, and predictions, quantifying the model's
        # accuracy with mean squared error (MSE) and variance explained
        best_lambda <- cv_lasso$lambda.min  # Best lambda
        lasso <-
          glmnet(x = train_predictors, y = train_response, alpha = 1,
                 lambda = best_lambda)
        optimal_coefficients <- coef(lasso)
        # Determine if the intercept was the only coefficient estimated
        all_zero <- all(optimal_coefficients[-1, ] == 0)
        # If all do not equal zero, proceed, otherwise return NA
        if (!all_zero) {
          predictions <- predict(lasso, newx = test_predictors)
          mse <- mean(abs(test_response - predictions)^2)
          variance_explained <- lasso$dev.ratio * 100
        } else {
          mse <- NA
          variance_explained <- NA
        }
        # And return outputs
        return(list(optimal_coefficients = optimal_coefficients,
                    mse = mse,
                    cv_lasso = cv_lasso,
                    variance_explained = variance_explained))
      }, cl = 9L)

    return(lasso_res)
  }

# A function to summarize the results of the Lasso regression
summarize_lasso_bootstrap <-
  function(lasso_res, n_bs_reps) {
    # Pull out the coefficients
    bs_coefs <-
      do.call(cbind, lapply(lasso_res, function(x) x$optimal_coefficients[, 1]))
    bs_coefs <- as.data.frame(bs_coefs)[-1, ]
    # Reformat
    bs_coefs$Pixel <- rownames(bs_coefs)
    # Determine the number of times each intensity was sampled/retained in Lasso
    times_selected <-
      apply(bs_coefs[, -(n_bs_reps + 1)], 1, function(x) sum(x != 0))
    # Calculate mean, standard error, and confidence interval
    rep_cols <- colnames(bs_coefs)[-ncol(bs_coefs)]

    bs_coefs_summary <-
      mclapply(seq_len(nrow(bs_coefs)), function(x) {
        pixel <- bs_coefs[x, "Pixel"]
        x <- unlist(bs_coefs[rep_cols][x, ])
        mean <- mean(x, na.rm = TRUE)
        sd <- sd(x, na.rm = TRUE)
        se <- sd / sqrt(sum(!is.na(x)))
        lower_ci <- mean - qt(0.975, sum(!is.na(x)) - 1) * se
        upper_ci <- mean + qt(0.975, sum(!is.na(x)) - 1) * se
        return(data.frame(Mean = mean, SD = sd, SE = se,
                          LowerCI = lower_ci,
                          UpperCI = upper_ci,
                          Pixel = as.numeric(pixel)))
      }, mc.cores = 9L)
    bs_coefs_summary <- do.call(rbind, bs_coefs_summary)

    # Attribute significance to the points for which the CI for the confidence
    # intervals do not overlap with 0
    bs_coefs_summary$Significant <- "No"
    bs_coefs_summary$Significant[which(bs_coefs_summary$LowerCI > 0 &
                                         bs_coefs_summary$UpperCI > 0)] <- "Yes"
    bs_coefs_summary$Significant[which(bs_coefs_summary$LowerCI < 0 &
                                         bs_coefs_summary$UpperCI < 0)] <- "Yes"
    bs_coefs_summary$times_selected <- times_selected

    bs_mse <- unlist(lapply(lasso_res, function(x) x$mse))
    bs_mse <- data.frame(bs_rep = 1:n_bs_reps, mse = bs_mse)

    return(list(bs_coefs_summary = bs_coefs_summary,
                bs_mse = bs_mse))
  }

# A function to plot the results of the Lasso regression
plot_lasso_results <-
  function(lasso_res, lasso_summary, n_bs_reps) {
    # Produce a "ramanhattan" plot of the mean coefficients at each point along
    # the spectra, and the frequency with which they are retained across
    # bootstrap replicates
    ramanhattan_plt <-
      ggplot(lasso_summary$bs_coefs_summary, aes(x = Pixel, color = Significant)) + # nolint
      geom_hline(yintercept = 0, size = 0.5, alpha = 0.5, lty = 2) +
      geom_point(alpha = 0.75, aes(y = Mean, size = times_selected/n_bs_reps)) + # nolint
      scale_size(range = c(0.05, 5)) +
      geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI, group = "Pixel"), width = 0) + # nolint
      labs(x = "Pixel",
           y = "Coefficient") +
      scale_color_manual(values = c("black", "red")) +
      guides(size = guide_legend(title = "% Reps. Sampled")) +
      theme_classic(base_size = 14) +
      theme(legend.position = "bottom")

    # A histogram of the mean squared error (MSE) of predictions
    # for each bootstrap replicate
    suppressWarnings(
      mse_plt <-
        ggplot(lasso_summary$bs_mse, aes(mse)) + # nolint
        geom_histogram(fill = "grey", color = "black", bins = 50) +
        geom_vline(xintercept = mean(na.omit(lasso_summary$bs_mse$mse)),
                   alpha = 0.75, size = 0.75, lty = 2, color = "red") +
        theme_classic(base_size = 14) +
        theme(legend.position = "bottom") +
        ylab("Frequency in BS Replicates") +
        xlab("Mean Squared Error (MSE)")
    )
    # Calculate % variance explained for each bootstrap replicate and plot
    # using histogram of the mean squared error (MSE) of predictions
    var_exp <-
      unlist(lapply(lasso_res, function(x) x$variance_explained))
    var_exp <- data.frame(bs_rep = 1:n_bs_reps, variance_explained = var_exp)
    suppressWarnings(
      var_exp_plt <-
        ggplot(var_exp, aes(variance_explained)) + # nolint
        geom_histogram(fill = "grey", color = "black", bins = 50) +
        theme_classic(base_size = 14) +
        theme(legend.position = "bottom") +
        geom_vline(xintercept = mean(na.omit(var_exp$variance_explained)),
                   alpha = 0.75, size = 0.75, lty = 2, color = "red") +
        scale_y_log10() +
        annotation_logticks(side = "l") +
        ylab("Frequency in BS Replicates") +
        xlab("% Variance Explained")
    )
    # Combine the plots into a single grid
    suppressWarnings(
      final_ramanhattan_plt <-
        plot_grid(ramanhattan_plt,
                  plot_grid(mse_plt, var_exp_plt, ncol = 1, align = "v"),
                  ncol = 2, rel_widths = c(3, 1))
    )
    return(list(ramanhattan_plt = ramanhattan_plt,
                mse_plot = mse_plt,
                var_explained_plot = var_exp_plt,
                combined_final_plot = final_ramanhattan_plt))
  }

# A function to format the Raman spectra and predict the response
# variable using a Lasso regression with bootstrapping
predict_from_raman <-
  function(spectral_data, response_var,
           n_bs_reps = 5000, training_prop = 0.75,
           plot_results = TRUE) {
    ## Function to predict some quantitative response variable from Raman
    ## spectra using lasso regression and a bootstrapping procedure to
    ## estimate the stability and CI of the coefficients.
    ## The function also plots the results.

    ## Parameters:
    ## spectral_data: data.frame containing three key columns:
    ## - "id" (sample ID - not replicate)
    ## - "Pixel" (i.e. wave number)
    ## - "Intensity" (spectral intensity)
    ## n_bs_reps: # of lasso regression bootstrap replicates
    ## response_var: vector of response variables to predict, ordered by "id"

    # Convert the spectra to a matrix of median profiles for each sample
    median_profile <- get_median_profile(spectral_data)

    # use a 75/25 split for training/testing
    sample_size <- floor(training_prop * nrow(median_profile$predictors))

    message("Conducting Lasso (L1) regression bootstrap procedure...")
    lasso_res <-
      run_lasso_bootstrap(median_profile$predictors, response_var,
                          n_bs_reps, sample_size)

    message("Summarizing results...")
    lasso_summary <- summarize_lasso_bootstrap(lasso_res, n_bs_reps)

    # If not plotting the results, return the results as a list,
    # otherwise plot and proceed and include the plots in the output
    if (plot_results == FALSE) {
      message("Done!")
      return(list(
        median_sample_intensity = median_profile$median_intensity,
        median_profile = median_profile$predictors,
        lasso_results = lasso_res,
        bs_coefficient_summary = lasso_summary
      ))
    } else {
      message("Plotting...")
      lasso_plots <-
        plot_lasso_results(lasso_res, lasso_summary, n_bs_reps)

      message("Done!")
      # And return the results, converting the median profile to a data.frame
      # with an "id" column
      return(list(
        median_sample_intensity = median_profile$median_intensity,
        median_profile = median_profile$predictors,
        lasso_results = lasso_res,
        bs_coefficient_summary = lasso_summary,
        ramanhattan_plot = lasso_plots$ramanhattan_plt,
        var_explained_plot = lasso_plots$var_explained_plot,
        mse_plot = lasso_plots$mse_plot,
        combined_final_plot = lasso_plots$combined_final_plot
      ))
    }
  }
