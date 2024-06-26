library(MASS)
library(ArcadiaColorBrewer)
## Function: 'load_spectra'
## Expects path to a master directory with
## sub-directories containing two input files:
## - .csv files of Raman spectra corresponding to a specific sample type
## The .csv files are expected to be two columns:
## 1. pixel/wavenumber
## 2. col2: ntensity
load_spectra <- function(path) {
  # List directories
  files <- list.files(path)

  # Create empty list to load into
  dat <- list()

  # Loop over directories and load .csvs
  for (i in seq_along(files)) {
    # List files
    to_load <- list.files(paste(path, files[i], sep = ""))

    # Load and add to list
    for (j in seq_along(to_load)) {
      dat[[paste(files[i], j, sep = "_")]] <- read.csv(paste(path,
        files[i], "/",
        to_load[j],
        sep = ""
      ))[, 2]
    }
  }

  # Combine
  dat <- do.call(cbind, dat)

  # Return
  return(dat)
}

## 'calculate_and_plot_PCA'
## Given a data matrix and sample list, perform PCA and plot the
## first two PCAs using Arcadia's color scheme/formatting
plot_pca <- function(data,
                     samples,
                     return = FALSE,
                     ...) {
  # PCA
  pca <- prcomp(data)

  # Set up expanded Arcadia palette
  all_colors <- c(
    "#5088C5", "#F28360", "#F7B846", "#97CD78",
    "#7A77AB", "#f898AE", "#3B9886", "#c85152",
    "#73B5E3", "#BAB0A8", "#8A99AD", "#FFB984",
    "#C6E7F4", "#F8C5C1", "#F5E4BE", "#B5BEA4",
    "#DCBFFC", "#B6C8D4", "#DAD3C7", "#DA9085"
  )

  # Get colors for each sample
  unique_samples <- unique(samples)
  cols <- all_colors[seq_len(length(unique_samples))]
  sample_colors <- setNames(cols, unique_samples)
  unique_colors <- sample_colors[samples]

  # Plot
  # Adjust the plotting area dimensions to make the plot longer and less wide
  par(mfrow = c(1, 1), mar = c(5, 5, 2, 1), pin = c(5, 7))

  # Plot
  layout(matrix(1:2, nrow = 1), widths = c(0.5, 0.5))
  par(mar = c(5, 5, 2, 1))
  plot(pca$x[, 1:2],
    pch = 21,
    col = ArcadiaColorBrewer::darken_color(unique_colors),
    bg = unique_colors,
    cex.axis = 1.5,
    cex.lab = 1.5,
    xlab = "PC1",
    ylab = "PC2",
    ...
  )

  # Add legend
  par(mar = c(5, 1, 2, 1))
  plot.new()
  legend("center",
    legend = names(sample_colors), fill = sample_colors,
    title = "Samples", cex = 0.8, xpd = NA
  )

  # Reset layout
  layout(1)

  # Return (if return == TRUE)
  if (return == TRUE) {
    return(pca)
  }
}


## 'calculate_and_plot_LDA'
## Given a data matrix and sample list, perform LDA and plot the
## first two LDs using Arcadia's color scheme/formatting
plot_lda <- function(data, samples, return = FALSE, ...) {
  # LDA
  mod <- lda(samples ~ as.matrix(data))

  # Predict outcomes
  p <- as.data.frame(predict(mod, as.data.frame(data)))

  # Set up expanded Arcadia palette
  all_colors <- c(
    "#5088C5", "#F28360", "#F7B846", "#97CD78",
    "#7A77AB", "#f898AE", "#3B9886", "#c85152",
    "#73B5E3", "#BAB0A8", "#8A99AD", "#FFB984",
    "#C6E7F4", "#F8C5C1", "#F5E4BE", "#B5BEA4",
    "#DCBFFC", "#B6C8D4", "#DAD3C7", "#DA9085"
  )

  # Get colors for each sample
  unique_samples <- unique(samples)
  cols <- all_colors[seq_len(length(unique_samples))]
  sample_colors <- setNames(cols, unique_samples)
  unique_colors <- sample_colors[samples]

  # Create layout to leave space for the legend
  # Adjust the plotting area dimensions to make the plot longer and less wide
  par(mfrow = c(1, 1), mar = c(5, 5, 2, 1), pin = c(5, 7))

  # Plot
  layout(matrix(1:2, nrow = 1), widths = c(0.5, 0.5))
  par(mar = c(5, 5, 2, 1))
  plot(p$x.LD1,
    p$x.LD2,
    pch = 21,
    col = ArcadiaColorBrewer::darken_color(unique_colors),
    bg = unique_colors,
    cex.axis = 1.5,
    cex.lab = 1.5,
    xlab = "LD1",
    ylab = "LD2",
    ...
  )

  # Add legend
  par(mar = c(2, 2, 2, 2)) # Adjust margins for the legend
  plot.new() # Create a new plot for the legend
  legend("center",
    legend = names(sample_colors), fill = sample_colors,
    title = "Samples", cex = 0.8, xpd = NA
  )

  # Reset layout
  layout(1)

  # Return (if return == TRUE)
  if (return == TRUE) {
    return(mod)
  }
}
