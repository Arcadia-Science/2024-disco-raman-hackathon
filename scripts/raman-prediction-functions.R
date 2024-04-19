require(cowplot)
require(reshape2)
require(parallel)
require(glmnet)
require(pbapply)
require(tidyverse)
require(ggplot2)
require(dplyr)

# A function to do all remaining analyses using these prepared data
predict_from_raman <-
  function(spectral_data, response_var, n_bs_reps = 5000) {
    # Summarize across replicates within pepper to get the median wave number
    # for each pepper
    suppressMessages(suppressWarnings(
      median_intensity <-
        spectral_data |>
        group_by(id, WaveNumber) |>
        summarize(WaveNumber = mean(WaveNumber),
                  MedianIntensity = median(Intensity)) |>
        ungroup()
    ))

    # Now prepare for analysis with Lasso regression
    median_profile <-
      dcast(median_intensity, id ~ WaveNumber, value.var = "MedianIntensity")
    rownames(median_profile) <- median_profile$id
    median_profile <- median_profile[, -1]

    # To use sliding windows
    # windows <- window_spectra(median_profile, window_size = 30, overlap = 10)

    # Assuming flesh_pcs is your dataframe
    predictors <- as.matrix(median_profile)
    # predictors <- as.matrix(windows)

    message("Conducting Lasso (L1) regression...")
    lasso_res <-
      pblapply(1:n_bs_reps, function(x) {
        sample_size <- floor(0.75 * nrow(predictors))

        # Initialize a flag to manage error-handling
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
                cv.glmnet(x = train_predictors, y = train_response, alpha = 1,
                          grouped = FALSE, nfolds = 10)
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
        # Now summarize
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

        return(list(optimal_coefficients = optimal_coefficients,
                    mse = mse,
                    cv_lasso = cv_lasso,
                    variance_explained = variance_explained))
      }, cl = 9L)

    message("Summarizing results...")
    # Pull out the coefficients
    bs_coefs <-
      do.call(cbind, lapply(lasso_res, function(x) x$optimal_coefficients[, 1]))
    bs_coefs <- as.data.frame(bs_coefs)[-1, ]
    # Reformat
    bs_coefs$WaveNumber <- rownames(bs_coefs)
    # Determine the number of times each intensity was sampled/retained in Lasso
    times_selected <- apply(bs_coefs[, -(n_bs_reps+1)], 1, function(x) sum(x != 0))
    # Calculate mean, standard error, and confidence interval
    rep_cols <- colnames(bs_coefs)[-ncol(bs_coefs)]

    bs_coefs_summary <-
      mclapply(seq_len(nrow(bs_coefs)), function(x) {
        wavenumber <- bs_coefs[x, "WaveNumber"]
        x <- unlist(bs_coefs[rep_cols][x, ])
        mean <- mean(x, na.rm = TRUE)
        sd <- sd(x, na.rm = TRUE)
        se <- sd / sqrt(sum(!is.na(x)))
        lower_ci <- mean - qt(0.975, sum(!is.na(x)) - 1) * se
        upper_ci <- mean + qt(0.975, sum(!is.na(x)) - 1) * se
        return(data.frame(Mean = mean, SD = sd, SE = se,
                          LowerCI = lower_ci,
                          UpperCI = upper_ci,
                          WaveNumber = as.numeric(wavenumber)))
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

    message("Plotting...")
    ramanhattan_plt <-
      ggplot(bs_coefs_summary, aes(x = WaveNumber, color = Significant)) +
      geom_hline(yintercept = 0, size = 0.5, alpha = 0.5, lty = 2) +
      geom_point(alpha = 0.75, aes(y = Mean, size = times_selected/n_bs_reps)) +
      scale_size(range = c(0.05, 5)) +
      geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI, group = "WaveNumber"), width = 0) +
      labs(x = "Wave Number (Pixel)",
           y = "Coefficient") +
      scale_color_manual(values = c("black", "red")) +
      guides(size = guide_legend(title = "% Reps. Sampled")) +
      theme_classic(base_size = 14) +
      theme(legend.position = "bottom")

    bs_mse <- unlist(lapply(lasso_res, function(x) x$mse))
    bs_mse <- data.frame(bs_rep = 1:n_bs_reps, mse = bs_mse)
    suppressWarnings(
      mse_plt <-
        ggplot(bs_mse, aes(mse)) +
        geom_histogram(fill = "grey", color = "black", bins = 50) +
        geom_vline(xintercept = mean(na.omit(bs_mse$mse)),
                   alpha = 0.75, size = 0.75, lty = 2, color = "red") +
        theme_classic(base_size = 14) +
        theme(legend.position = "bottom") +
        ylab("Frequency in BS Replicates") +
        xlab("Mean Squared Error (MSE)")
    )
    var_exp <-
      unlist(lapply(lasso_res, function(x) x$variance_explained))
    var_exp <- data.frame(bs_rep = 1:n_bs_reps, variance_explained = var_exp)

    suppressWarnings(
      var_exp_plt <-
        ggplot(var_exp, aes(variance_explained)) +
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
    suppressWarnings(
      final_ramanhattan_plt <-
        plot_grid(ramanhattan_plt,
                  plot_grid(mse_plt, var_exp_plt, ncol = 1, align = "v"),
                  ncol = 2, rel_widths = c(3, 1))
    )
    message("Done!")
    return(list(
      median_sample_intensity = median_intensity,
      lasso_results = lasso_res,
      bs_coefficient_summary = bs_coefs_summary,
      ramanhattan_plot = ramanhattan_plt,
      var_explained_plot = var_exp_plt,
      mse_plot = mse_plt,
      combined_final_plot = final_ramanhattan_plt
    ))
  }

# Function to create overlapping windows and calculate mean for each window
window_spectra <- function(spectra, window_size = 3, overlap = 1) {
  windows <- list()
  i <- 1
  end <- i + window_size - 1
  window_mindpoints <- c()
  while (end < ncol(spectra)) {
    start <- i
    end <- i + window_size - 1
    end <-
      if (end > ncol(spectra)) {
        ncol(spectra)
      } else {
        end
      }
    window_data <- spectra[, start:end]
    window_mean <- rowMeans(window_data)
    window_mindpoints <- c(window_mindpoints, mean(start, end))
    if (i == 1) {
      windows <- data.frame(window_mean)
    } else {
      windows <- cbind(windows, window_mean)
    }
    i <- i + overlap
  }
  colnames(windows) <- window_mindpoints
  return(windows)
}
