library(ggplot2)
library(tidyverse)

setwd("~/Documents/ArcadiaScience/github/2024-disco-raman-hackathon/")
source("./scripts/raman-prediction-functions.R")
chili_meta <-
  read.table("./data/peppers/metadata.csv", sep = ",", header = TRUE)
colnames(chili_meta)[2] <- "chili_name"
# And then pull out the data
seed_meta <- chili_meta[which(chili_meta$sample_type == "seed"), ]

# Sort the metadata to order the species alphabetically as is done above
seed_meta <- seed_meta[order(seed_meta$chili_name), ]

# Get some unique IDs for each run
run_ids <- paste0(rep(flesh_meta$chili_name, each = 3), "_", 1:3)
sample_conditions <- rep(flesh_meta$chili_condition, each = 3)

# And then read in the spectra data
spectra_dirs <- list.dirs("./data/peppers", full.names = TRUE)[-1]
chili_seed_dat <- list()
for (f in seq_along(spectra_dirs)) {
  fpaths <-
    list.files(spectra_dirs[f], full.names = TRUE)
  for (i in seq_along(fpaths)) {
    tmp <- read.table(fpaths[[i]], sep = ",", header = TRUE, row.names = NULL)
    colnames(tmp) <- c("Pixel", "Intensity")
    tmp <- as.data.frame(apply(tmp, 2, as.numeric))
    tmp$chili_name <- gsub(".*/", "", spectra_dirs[f])
    tmp$rep_id <- i
    tmp$chili_id <- paste0(gsub(".*/", "", spectra_dirs[f]), "_rep_", i)
    chili_seed_dat[[length(chili_seed_dat) + 1]] <- tmp
  }
}
chili_seed_dat <- do.call(rbind, chili_seed_dat)

# Pull out only the pertinent metadata
seed_meta <-
  chili_meta[which(chili_meta$sample_type == "seed"), c(2, 4, 7:11)]

# And combine with the spectral data
chili_seed_dat <- merge(chili_seed_dat, seed_meta, by = "chili_name")
colnames(chili_seed_dat)[1] <- "id"

set.seed(123)  # For reproducibility
seed_scoville_res <-
  predict_from_raman(spectral_data = chili_seed_dat,
                     response_var = log10(seed_meta$mean_scoville + 1),
                     n_bs_reps = 5000)

############################################################################
# Save the plots and outputs to file

# Now, plot along with the spectra and then save out to file
seed_med_spectra <- seed_scoville_res$median_sample_intensity
seed_med_spectra <-
  merge(seed_med_spectra, unique(chili_seed_dat[, c(1, 13)]), by = "id")

# Plot the individual spectra, colored by their
# respective chili's scoville units
seed_spectra <-
  ggplot(data = seed_med_spectra,
         aes(y = MedianIntensity, x = Pixel,
             color = log10(scoville_median + 1))) +
  geom_line(alpha = 0.7, size = 0.75, aes(group = id)) +
  theme_classic(base_size = 14) +
  scale_color_viridis_c(option = "B", end = 0.9) +
  xlab("Pixel") +
  ylab("Intensity") +
  guides(color = guide_colorbar(title = "Median Scoville")) +
  theme(legend.position = "top")

seed_spect_manhat_plt <-
  plot_grid(seed_spectra, seed_scoville_res$ramanhattan_plot,
            nrow = 2, ncol = 1, rel_heights = c(1, 2), align = "v")

seed_mod_perform_plt <-
  suppressWarnings(plot_grid(seed_scoville_res$mse_plot,
                             seed_scoville_res$var_explained_plot,
                             nrow = 2, ncol = 1, rel_heights = c(1, 1),
                             align = "v"))

final_seed_plt <-
  plot_grid(seed_spect_manhat_plt, seed_mod_perform_plt, ncol = 2,
            rel_widths = c(3, 1))

# Create output directory
dir.create("./results/peppers/", showWarnings = FALSE, recursive = FALSE)

# Now save to file
ggsave(final_seed_plt, height = 8, width = 14,
       file = "./results/peppers/chili_seed_scoville_prediction.pdf")
ggsave(final_seed_plt, height = 8, width = 14, dpi = 600,
       file = "./results/peppers/chili_seed_scoville_prediction.png")

# Save some other intermediates:
write.table(seed_med_spectra,
            file = "./results/peppers/chili_seed_scoville_median_Pixel.csv",
            sep = ",", col.names = TRUE, row.names = FALSE, quote = FALSE)
write.table(seed_scoville_res$bs_coefficient_summary,
            file = "./results/peppers/chili_seed_scoville_lasso_coefficient_summary.csv",
            sep = ",", col.names = TRUE, row.names = FALSE, quote = FALSE)
write.table(seed_scoville_res$bs_coefficient_summary,
            file = "./results/peppers/chili_seed_scoville_lasso_coefficient_summary.csv",
            sep = ",", col.names = TRUE, row.names = FALSE, quote = FALSE)

