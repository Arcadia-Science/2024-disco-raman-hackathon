library(ggplot2)
library(tidyverse)

setwd("~/Documents/ArcadiaScience/github/2024-disco-raman-hackathon/")
source("./scripts/raman-prediction-functions.R")

chili_meta <-
  read.table("./data/peppers/chili_data_updated.csv", sep = ",", header = TRUE)
# Create a useful identifier
chili_meta$chili_id <-
  paste0(chili_meta$Abbreviation, "_", chili_meta$run_id, "_",
         chili_meta$chili_condition)

# And then pull out the data for the two tissue types
flesh_meta <- chili_meta[which(chili_meta$sample_type == "flesh"), ]
seed_meta <- chili_meta[which(chili_meta$sample_type == "seed"), ]

# Sort the metadata to order the species alphabetically as is done above
flesh_meta <- flesh_meta[order(flesh_meta$Abbreviation), ]
seed_meta <- seed_meta[order(seed_meta$Abbreviation), ]

# Get some unique IDs for each run
run_ids <- paste0(rep(flesh_meta$Abbreviation, each = 3), "_", 1:3)
sample_conditions <- rep(flesh_meta$chili_condition, each = 3)

# And then read in the spectra data
flesh_spectra <- list.files("./data/peppers/pepper_flesh/")
seed_spectra <- list.files("./data/peppers/pepper_seeds/")

# First the flesh data
chili_flesh_dat <- list()
for (i in seq_along(flesh_spectra)) {
  tmp <-
    read.table(paste0("./data/peppers/pepper_flesh/",
                      flesh_spectra[[i]]), sep = ",",
               header = TRUE, row.names = NULL)
  colnames(tmp) <- c("WaveNumber", "Intensity")
  tmp <- as.data.frame(apply(tmp, 2, as.numeric))
  tmp$chili_name <- run_ids[i]
  tmp$chili_condition <- sample_conditions[i]
  tmp$rep_id <- i
  tmp$chili_id <- paste0(tmp$chili_name, "_", tmp$chili_condition)
  chili_flesh_dat[[length(chili_flesh_dat) + 1]] <- tmp
}
chili_flesh_dat <- do.call(rbind, chili_flesh_dat)

# Get some useful identifiers
chili_flesh_dat$Abbreviation <- gsub("_.*", "", chili_flesh_dat$chili_name)

# Pull out only the pertinent metadata
flesh_meta <-
  chili_meta[which(chili_meta$sample_type == "flesh"), c(2, 4, 7:11)]
# And store the response variable in a vector equal in length and order to the
# samples we are making predictions for:
response <- log10(flesh_meta$scoville_median + 1)

# And combine with the spectral data
chili_flesh_dat <- merge(chili_flesh_dat, flesh_meta, by = "Abbreviation")
colnames(chili_flesh_dat)[1] <- "id"

set.seed(123)  # For reproducibility
flesh_scoville_res <-
  predict_from_raman(spectral_data = chili_flesh_dat,
                     response_var = log10(flesh_meta$scoville_median + 1),
                     n_bs_reps = 5000)

############################################################################
# Then the seed data
chili_seed_dat <- list()
for (i in seq_along(seed_spectra)) {
  tmp <-
    read.table(paste0("./data/peppers/pepper_seeds/",
                      seed_spectra[[i]]),
               sep = ",", header = TRUE, row.names = NULL)
  colnames(tmp) <- c("WaveNumber", "Intensity")
  tmp <- as.data.frame(apply(tmp, 2, as.numeric))
  tmp$chili_name <- run_ids[i]
  tmp$chili_condition <- sample_conditions[i]
  tmp$rep_id <- i
  tmp$chili_id <- paste0(tmp$chili_name, "_", tmp$chili_condition)
  chili_seed_dat[[length(chili_seed_dat) + 1]] <- tmp
}
chili_seed_dat <- do.call(rbind, chili_seed_dat)

# Get some useful identifiers
chili_seed_dat$Abbreviation <- gsub("_.*", "", chili_seed_dat$chili_name)

# Pull out only the pertinent metadata
seed_meta <-
  chili_meta[which(chili_meta$sample_type == "seed"), c(2, 4, 7:11)]

# And combine with the spectral data
chili_seed_dat <- merge(chili_seed_dat, seed_meta, by = "Abbreviation")
colnames(chili_seed_dat)[1] <- "id"

set.seed(123)  # For reproducibility
seed_scoville_res <-
  predict_from_raman(spectral_data = chili_seed_dat,
                     response_var = log10(seed_meta$scoville_median + 1),
                     n_bs_reps = 5000)

############################################################################
# Save the plots and outputs to file

# Now, plot along with the spectra and then save out to file
flesh_med_spectra <- flesh_scoville_res$median_sample_intensity
flesh_med_spectra <-
  merge(flesh_med_spectra, unique(chili_flesh_dat[, c(1, 13)]), by = "id")
seed_med_spectra <- seed_scoville_res$median_sample_intensity
seed_med_spectra <-
  merge(seed_med_spectra, unique(chili_seed_dat[, c(1, 13)]), by = "id")

# Plot the individual spectra, colored by their
# respective chili's scoville units
flesh_spectra <-
  ggplot(data = flesh_med_spectra,
         aes(y = MedianIntensity, x = WaveNumber,
             color = log10(scoville_median + 1))) +
  geom_line(alpha = 0.7, size = 0.75, aes(group = id)) +
  theme_classic(base_size = 14) +
  scale_color_viridis_c(option = "B", end = 0.9) +
  xlab("Wave Number (Pixel)") +
  ylab("Intensity") +
  guides(color = guide_colorbar(title = "Median Scoville")) +
  scale_y_log10() +
  annotation_logticks(side = "l") +
  theme(legend.position = "top")
seed_spectra <-
  ggplot(data = seed_med_spectra,
         aes(y = MedianIntensity, x = WaveNumber,
             color = log10(scoville_median + 1))) +
  geom_line(alpha = 0.7, size = 0.75, aes(group = id)) +
  theme_classic(base_size = 14) +
  scale_color_viridis_c(option = "B", end = 0.9) +
  xlab("Wave Number (Pixel)") +
  ylab("Intensity") +
  guides(color = guide_colorbar(title = "Median Scoville")) +
  theme(legend.position = "top")

flesh_spect_manhat_plt <-
  plot_grid(flesh_spectra, flesh_scoville_res$ramanhattan_plot,
            nrow = 2, ncol = 1, rel_heights = c(1, 2), align = "v")
seed_spect_manhat_plt <-
  plot_grid(seed_spectra, seed_scoville_res$ramanhattan_plot,
            nrow = 2, ncol = 1, rel_heights = c(1, 2), align = "v")

flesh_mod_perform_plt <-
  suppressWarnings(plot_grid(flesh_scoville_res$mse_plot,
                             flesh_scoville_res$var_explained_plot,
                             nrow = 2, ncol = 1, rel_heights = c(1, 1),
                             align = "v"))
seed_mod_perform_plt <-
  suppressWarnings(plot_grid(seed_scoville_res$mse_plot,
                             seed_scoville_res$var_explained_plot,
                             nrow = 2, ncol = 1, rel_heights = c(1, 1),
                             align = "v"))

final_flesh_plt <-
  plot_grid(flesh_spect_manhat_plt, flesh_mod_perform_plt, ncol = 2,
            rel_widths = c(3, 1))
final_seed_plt <-
  plot_grid(seed_spect_manhat_plt, seed_mod_perform_plt, ncol = 2,
            rel_widths = c(3, 1))

# Create output directory
dir.create("./results/peppers/", showWarnings = FALSE, recursive = FALSE)

# Now save to file
ggsave(final_flesh_plt, height = 8, width = 14,
       file = "./results/peppers/chili_flesh_scoville_prediction.pdf")
ggsave(final_flesh_plt, height = 8, width = 14, dpi = 600,
       file = "./results/peppers/chili_flesh_scoville_prediction.png")
ggsave(final_seed_plt, height = 8, width = 14,
       file = "./results/peppers/chili_seed_scoville_prediction.pdf")
ggsave(final_seed_plt, height = 8, width = 14, dpi = 600,
       file = "./results/peppers/chili_seed_scoville_prediction.png")

# Save some other intermediates:
write.table(flesh_med_spectra,
            file = "./results/peppers/chili_flesh_scoville_median_wavenumber.csv",
            sep = ",", col.names = TRUE, row.names = FALSE, quote = FALSE)
write.table(flesh_scoville_res$bs_coefficient_summary,
            file = "./results/peppers/chili_flesh_scoville_lasso_coefficient_summary.csv",
            sep = ",", col.names = TRUE, row.names = FALSE, quote = FALSE)
write.table(flesh_scoville_res$bs_coefficient_summary,
            file = "./results/peppers/chili_flesh_scoville_lasso_coefficient_summary.csv",
            sep = ",", col.names = TRUE, row.names = FALSE, quote = FALSE)
write.table(seed_med_spectra,
            file = "./results/peppers/chili_seed_scoville_median_wavenumber.csv",
            sep = ",", col.names = TRUE, row.names = FALSE, quote = FALSE)
write.table(seed_scoville_res$bs_coefficient_summary,
            file = "./results/peppers/chili_seed_scoville_lasso_coefficient_summary.csv",
            sep = ",", col.names = TRUE, row.names = FALSE, quote = FALSE)
write.table(seed_scoville_res$bs_coefficient_summary,
            file = "./results/peppers/chili_seed_scoville_lasso_coefficient_summary.csv",
            sep = ",", col.names = TRUE, row.names = FALSE, quote = FALSE)

