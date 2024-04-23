library(ggplot2)
library(tidyverse)

setwd("~/Documents/ArcadiaScience/github/2024-disco-raman-hackathon/")
source("./code/raman-prediction-functions.R")

beer_meta <-
  read.table("./data/beer/metadata.csv", sep = ",", header = TRUE)
spectra_dirs <- list.dirs("./data/beer", recursive = TRUE)[-1]
# Create a useful, simple name for the beers to use as an ID
beer_meta$beer_name <-
  gsub("\\(.*", "", beer_meta$Beer) |>
  str_to_lower() |>
  gsub(pattern = "\\s+$", replacement = "") |>
  gsub(pattern = " ", replacement = "-") |>
  gsub(pattern = "’", replacement = "")

beer_dat <- list()
for (f in seq_along(spectra_dirs)) {
  fpaths <-
    list.files(spectra_dirs[f], full.names = TRUE)
  for (i in seq_along(fpaths)) {
    tmp <- read.table(fpaths[[i]], sep = ",", header = TRUE, row.names = NULL)
    colnames(tmp) <- c("Pixel", "Intensity")
    tmp <- as.data.frame(apply(tmp, 2, as.numeric))
    tmp$beer_name <- gsub(".*/", "", spectra_dirs[f])
    tmp$rep_id <- i
    tmp$beer_id <- paste0(gsub(".*/", "", spectra_dirs[f]), "_rep_", i)
    beer_dat[[length(beer_dat) + 1]] <- tmp
  }
}
beer_dat <- do.call(rbind, beer_dat)

# And combine with the spectral data
beer_dat <- merge(beer_dat, beer_meta, by = "beer_name")

colnames(beer_dat)[1] <- "id"

set.seed(123)  # For reproducibility
abv_res <-
  predict_from_raman(spectral_data = beer_dat,
                     response_var = beer_meta$ABV,
                     n_bs_reps = 5000)

med_spectra <- abv_res$median_sample_intensity
med_spectra <- merge(med_spectra, unique(beer_dat[, c(1, 10)]), by = "id")
colnames(med_spectra)[4] <- "ABV"

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

spect_manhat_plt <-
  plot_grid(abv_spectra, abv_res$ramanhattan_plot,
            nrow = 2, ncol = 1, rel_heights = c(1, 2), align = "v")

mod_perform_plt <-
  suppressWarnings(plot_grid(abv_res$mse_plot, abv_res$var_explained_plot,
                             nrow = 2, ncol = 1, rel_heights = c(1, 1),
                             align = "v"))

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
write.table(med_spectra, file = "./results/beer/beer_abv_median_Pixel.csv",
            sep = ",", col.names = TRUE, row.names = FALSE, quote = FALSE)
write.table(abv_res$bs_coefficient_summary,
            file = "./results/beer/beer_abv_lasso_coefficient_summary.csv",
            sep = ",", col.names = TRUE, row.names = FALSE, quote = FALSE)
