library(ggplot2)
library(tidyverse)

source("./code/raman_prediction_functions.R")

# Read in the beer metadata (ABV, IBU, etc.)
beer_meta <-
  read.table("./data/beer/metadata.csv", sep = ",", header = TRUE)
colnames(beer_meta)[5] <- "ABV"
# Store the paths to the directories containing the spectral data
# Remove first, which corresponds to the base directory "./data/beer"
spectra_dirs <- list.dirs("./data/beer", recursive = TRUE)[-1]
# Create a useful, simple name for the beers to use as an ID (e.g., "beer-name")
beer_meta$id <-
  gsub("\\(.*", "", beer_meta$Beer) |>
  str_to_lower() |>
  gsub(pattern = "\\s+$", replacement = "") |>
  gsub(pattern = " ", replacement = "-") |>
  gsub(pattern = "’", replacement = "")

# Now read in the spectral data
beer_dat <- list()
for (f in seq_along(spectra_dirs)) {
  fpaths <-
    list.files(spectra_dirs[f], full.names = TRUE)
  for (i in seq_along(fpaths)) {
    tmp <- read.table(fpaths[[i]], sep = ",", header = TRUE, row.names = NULL)
    beer_dat[[length(beer_dat) + 1]] <-
      data.frame(Pixel = as.numeric(tmp[, 1]),
                 Intensity = as.numeric(tmp[, 2]),
                 id = gsub(".*/", "", spectra_dirs[f]),
                 rep_id = i,
                 beer_id = paste0(gsub(".*/", "", spectra_dirs[f]),
                                  "_rep_", i))
  }
}
beer_dat <- do.call(rbind, beer_dat)

# And combine with the spectral data
beer_dat <- merge(beer_dat, beer_meta, by = "id")

set.seed(123)  # For reproducibility
# 5000 BS reps is robust but not overkill for our sample size
abv_res <-
  predict_from_raman(spectral_data = beer_dat,
                     response_var = beer_meta$ABV,
                     n_bs_reps = 5000)

med_spectra <- abv_res$median_sample_intensity
# Merge the median spectra with the the beer ID and ABV columns
med_spectra <- merge(med_spectra, unique(beer_dat[, c("id", "ABV")]), by = "id")

# Plot the individual spectra, colored by their respective beer's ABV
abv_spectra <-
  ggplot(data = med_spectra,
         aes(y = MedianIntensity, x = Pixel, color = ABV)) +
  geom_line(alpha = 0.7, size = 0.75, aes(group = id)) +
  theme_classic(base_size = 14) +
  scale_color_viridis_c(option = "A", end = 0.9) +
  ylab("Pixel") +
  ylab("Intensity") +
  guides(color = guide_colorbar(title = "ABV (%)")) +
  theme(legend.position = "top")

# And then combine with the "ramanhattan" plot and model performance plots
spect_manhat_plt <-
  plot_grid(abv_spectra, abv_res$ramanhattan_plot,
            nrow = 2, ncol = 1, rel_heights = c(1, 2), align = "v")
mod_perform_plt <-
  plot_grid(abv_res$mse_plot, abv_res$var_explained_plot,
            nrow = 2, ncol = 1, rel_heights = c(1, 1),
            align = "v")
final_abv_plt <-
  plot_grid(spect_manhat_plt, mod_perform_plt, ncol = 2,
            rel_widths = c(3, 1))

# Create output directory
dir.create("./results/beer/", showWarnings = FALSE, recursive = FALSE)

# Now save to file
ggsave(final_abv_plt, height = 8, width = 14,
       file = "./results/beer/beer_abv_prediction.pdf")
ggsave(final_abv_plt, height = 8, width = 14, dpi = 600,
       file = "./results/beer/beer_abv_prediction.png")

# Save some other intermediates:
write.table(med_spectra, file = "./results/beer/beer_abv_median_pixel.csv",
            sep = ",", col.names = TRUE, row.names = FALSE, quote = FALSE)
write.table(abv_res$bs_coefficient_summary,
            file = "./results/beer/beer_abv_lasso_coefficient_summary.csv",
            sep = ",", col.names = TRUE, row.names = FALSE, quote = FALSE)
