## 'load_spectra'
## Expects path to a master directory with sub-directories containing
## .csv files of Raman spectra corresponding to a specific sample type
## .csv files are expected to be two columns:
## col1: pixel/wavenumber 
## col2: intensity
load_spectra <- function(path) {
  # List directories
  files <- list.files(path)

  # Create empty list to load into
  dat <- list()

  # Loop over directories and load .csvs
  for (i in 1:length(files)) {
    # List files
    toLoad <- list.files(paste(path, files[i], sep = ""))

    # Load and add to list
    for (j in 1:length(toLoad)) {
      dat[[paste(files[i], j, sep = "_")]] <- read.csv(paste(path,
        files[i], "/",
        toLoad[j],
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
calculate_and_plot_PCA <- function(data,
                                   samples,
                                   return_PCA = FALSE,
                                   add_labels = FALSE,
                                   ...) {
  # PCA
  pca <- prcomp(data)

  # Calculate %variance explained
  eigs <- round(pca$sdev^2 / sum(pca$sdev^2), 4) * 100

  # Set up expanded Arcadia palette
  all_colors <- c(
    "#5088C5", "#F28360", "#F7B846", "#97CD78",
    "#7A77AB", "#f898AE", "#3B9886", "#c85152",
    "#73B5E3", "#BAB0A8", "#8A99AD", "#FFB984",
    "#C6E7F4", "#F8C5C1", "#F5E4BE", "#B5BEA4",
    "#DCBFFC", "#B6C8D4", "#DAD3C7", "#DA9085"
  )


  # Get colors for each sample
  cols <- all_colors[1:length(unique(samples))]
  cols <- cols[match(samples, unique(samples))]

  # Plot
  plot(pca$x[, 1:2],
    pch = 21,
    col = darken_color(cols),
    bg = cols,
    cex.axis = 1.5,
    cex.lab = 1.5,
    xlab = paste("PC1 (", eigs[1], "%)", sep = ""),
    ylab = paste("PC2 (", eigs[2], "%)", sep = ""),
    ...
  )

  # Add labels (if add_labels == TRUE)
  if (add_labels == TRUE) {
    legend("topright",
      legend = unique(samples),
      text.col = all_colors[1:length(unique(samples))]
    )
  }

  # Return (if return_PCA == TRUE)
  if (return_PCA == TRUE) {
    return(pca)
  }
}
