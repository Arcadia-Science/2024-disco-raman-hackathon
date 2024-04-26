source("code/raman-cluster-functions.R")

##### Load data#####
# Beer
beer <- load_spectra("data/beer/")

# Chilis
peppers <- load_spectra("data/peppers/")

# Algae
algae <- load_spectra("data/algae/")

# Combine data
all <- cbind(
  beer,
  peppers,
  algae
)
colnames(all) <- c(
  rep("beer", ncol(beer)),
  rep("peppers", ncol(peppers)),
  rep("algae", ncol(algae))
)

# Combine into list
all_data <- list(
  all = all,
  beer = beer,
  peppers = peppers,
  algae = algae
)

##### Plot#####
# Set up plot
pdf("img/raman-cluster-plots.pdf",
  width = 16,
  height = 4
)
par(mfrow = c(1, 4))

# PCA on all data
data <- t(all)
samples <- unlist(lapply(strsplit(rownames(data), "_"), function(x) x[1]))

# Plot
plot_pca(data,
  samples,
  cex = 1.5
)

# Add title
title(
  main = "all",
  font.main = 1,
  cex.main = 1.5
)

# LDA on individual sample types
for (i in 2:length(all_data)) {
  # Set up data
  data <- t(all_data[[i]])
  samples <- unlist(lapply(strsplit(rownames(data), "_"), function(x) x[1]))

  # Plot
  plotLDA(data,
    samples,
    cex = 1.5,
  )

  # Add title
  title(
    main = names(all_data)[i],
    font.main = 1,
    cex.main = 1.5
  )
}
dev.off()
