library(ggplot2)
library(tidyverse)

source("./code/raman_prediction_functions.R")
# Ignore first column of metadata - it's just the row number
chili_meta <-
  read.table("./data/peppers/metadata.csv", sep = ",", header = TRUE)[, -1]
# Change pepper abbreviation to ID for consistency with beer analysis
colnames(chili_meta)[1] <- "id"
# And then pull out the data
seed_meta <- chili_meta[which(chili_meta$sample_type == "seed"), ]

# Sort the metadata to order the species alphabetically as is done above
seed_meta <- seed_meta[order(seed_meta$id), ]

# Get some unique IDs for each run (three replicates per sample)
run_ids <- paste0(rep(seed_meta$id, each = 3), "_", 1:3)
sample_conditions <- rep(seed_meta$chili_condition, each = 3)

# And then read in the spectra data
# Remove first, which corresponds to the base directory "./data/peppers"
spectra_dirs <- list.dirs("./data/peppers", full.names = TRUE)[-1]
chili_seed_dat <- list()
for (f in seq_along(spectra_dirs)) {
  fpaths <-
    list.files(spectra_dirs[f], full.names = TRUE)
  for (i in seq_along(fpaths)) {
    tmp <- read.table(fpaths[[i]], sep = ",", header = TRUE, row.names = NULL)
    chili_seed_dat[[length(chili_seed_dat) + 1]] <-
      data.frame(Pixel = as.numeric(tmp[, 1]),
                 Intensity = as.numeric(tmp[, 2]),
                 id = gsub(".*/", "", spectra_dirs[f]),
                 rep_id = i,
                 chili_id = paste0(gsub(".*/", "", spectra_dirs[f]),
                                   "_rep_", i))
  }
}
chili_seed_dat <- do.call(rbind, chili_seed_dat)

# Pull out only the pertinent metadata
seed_meta <-
  chili_meta[which(chili_meta$sample_type == "seed"), c("id", "mean_scoville")]

# And combine with the spectral data
chili_seed_dat <- merge(chili_seed_dat, seed_meta, by = "id")

set.seed(123)  # For reproducibility
# 5000 BS reps is robust but not overkill for our sample size
# Add 1 to the scoville units to avoid log10(0):
# - min value in this space is thus 0
seed_scoville_res <-
  predict_from_raman(spectral_data = chili_seed_dat,
                     response_var = log10(seed_meta$mean_scoville + 1),
                     n_bs_reps = 5000)

############################################################################
# Save the plots and outputs to file

# Now, plot along with the spectra and then save out to file
seed_med_spectra <- seed_scoville_res$median_sample_intensity
seed_med_spectra <-
  merge(seed_med_spectra, unique(chili_seed_dat[, c("id", "mean_scoville")]),
        by = "id")

# Plot the individual spectra, colored by their
# respective chili's scoville units
seed_spectra <-
  ggplot(data = seed_med_spectra,
         aes(y = MedianIntensity, x = Pixel,
             color = log10(mean_scoville + 1))) +
  geom_line(alpha = 0.7, size = 0.75, aes(group = id)) +
  theme_classic(base_size = 14) +
  scale_color_viridis_c(option = "B", end = 0.9) +
  xlab("Pixel") +
  ylab("Intensity") +
  guides(color = guide_colorbar(title = "Median Scoville")) +
  theme(legend.position = "top")

# And then combine with the "ramanhattan" plot and model performance plots
seed_spect_manhat_plt <-
  plot_grid(seed_spectra, seed_scoville_res$ramanhattan_plot,
            nrow = 2, ncol = 1, rel_heights = c(1, 2), align = "v")
seed_mod_perform_plt <-
  plot_grid(seed_scoville_res$mse_plot,
            seed_scoville_res$var_explained_plot,
            nrow = 2, ncol = 1, rel_heights = c(1, 1),
            align = "v")
final_seed_plt <-
  plot_grid(seed_spect_manhat_plt, seed_mod_perform_plt, ncol = 2,
            rel_widths = c(3, 1))

# Create output directory
out_dir <- "./results/peppers/"
dir.create(out_dir, showWarnings = FALSE, recursive = FALSE)

# Now save to file
ggsave(final_seed_plt, height = 8, width = 14,
       file = paste0(out_dir, "chili_seed_scoville_prediction.pdf"))
ggsave(final_seed_plt, height = 8, width = 14, dpi = 600,
       file = paste0(out_dir, "chili_seed_scoville_prediction.png"))

# Save some other intermediates:
write.table(seed_med_spectra,
            file = paste0(out_dir, "chili_seed_scoville_median_Pixel.csv"),
            sep = ",", col.names = TRUE, row.names = FALSE, quote = FALSE)
write.table(seed_scoville_res$bs_coefficient_summary,
            file = paste0(out_dir,
                          "chili_seed_scoville_lasso_coefficient_summary.csv"),
            sep = ",", col.names = TRUE, row.names = FALSE, quote = FALSE)
write.table(seed_scoville_res$bs_coefficient_summary,
            file = paste0(out_dir,
                          "chili_seed_scoville_lasso_coefficient_summary.csv"),
            sep = ",", col.names = TRUE, row.names = FALSE, quote = FALSE)
