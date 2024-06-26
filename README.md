# 2024-disco-raman-hackathon

[![run with conda](https://img.shields.io/badge/run%20with-conda-3EB049?labelColor=000000&logo=anaconda)](https://docs.conda.io/projects/miniconda/en/latest/)
[![Arcadia Pub](https://img.shields.io/badge/Arcadia-Pub-596F74.svg?logo=data:image/svg%2bxml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0idXRmLTgiPz4KPCEtLSBHZW5lcmF0b3I6IEFkb2JlIElsbHVzdHJhdG9yIDI3LjcuMCwgU1ZHIEV4cG9ydCBQbHVnLUluIC4gU1ZHIFZlcnNpb246IDYuMDAgQnVpbGQgMCkgIC0tPgo8c3ZnIHZlcnNpb249IjEuMSIgaWQ9IkxheWVyXzEiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgeG1sbnM6eGxpbms9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkveGxpbmsiIHg9IjBweCIgeT0iMHB4IgoJIHZpZXdCb3g9IjAgMCA0My4yIDQwLjQiIHN0eWxlPSJlbmFibGUtYmFja2dyb3VuZDpuZXcgMCAwIDQzLjIgNDAuNDsiIHhtbDpzcGFjZT0icHJlc2VydmUiPgo8c3R5bGUgdHlwZT0idGV4dC9jc3MiPgoJLnN0MHtmaWxsOm5vbmU7c3Ryb2tlOiNGRkZGRkY7c3Ryb2tlLXdpZHRoOjI7c3Ryb2tlLWxpbmVqb2luOmJldmVsO3N0cm9rZS1taXRlcmxpbWl0OjEwO30KPC9zdHlsZT4KPGc+Cgk8cG9seWdvbiBjbGFzcz0ic3QwIiBwb2ludHM9IjIxLjYsMyAxLjcsMzcuNCA0MS41LDM3LjQgCSIvPgoJPGxpbmUgY2xhc3M9InN0MCIgeDE9IjIxLjYiIHkxPSIzIiB4Mj0iMjEuNiIgeTI9IjI3LjMiLz4KCTxwb2x5bGluZSBjbGFzcz0ic3QwIiBwb2ludHM9IjEyLjIsMTkuNCAyNC42LDMwLjEgMjQuNiwzNy40IAkiLz4KCTxsaW5lIGNsYXNzPSJzdDAiIHgxPSIxNy42IiB5MT0iMTYuNyIgeDI9IjE3LjYiIHkyPSIyNC4xIi8+Cgk8bGluZSBjbGFzcz0ic3QwIiB4MT0iMjguNiIgeTE9IjE1LjIiIHgyPSIyMS43IiB5Mj0iMjIuMSIvPgoJPHBvbHlsaW5lIGNsYXNzPSJzdDAiIHBvaW50cz0iNi44LDI4LjcgMTkuNSwzNC40IDE5LjUsMzcuNCAJIi8+Cgk8bGluZSBjbGFzcz0ic3QwIiB4MT0iMzQuOCIgeTE9IjI1LjgiIHgyPSIyNC42IiB5Mj0iMzYuMSIvPgoJPGxpbmUgY2xhc3M9InN0MCIgeDE9IjI5LjciIHkxPSIyMi4yIiB4Mj0iMjkuNyIgeTI9IjMwLjkiLz4KPC9nPgo8L3N2Zz4K)](https://doi.org/10.57844/arcadia-085e-3ecf)


## Purpose

This repository contains code and results pertaining to the Raman hackathon carried out by the Discovery team at the Discovery offsite in March 2024.
It accompanies the pub "[Raman spectroscopy enables rapid and inexpensive exploration of biology](https://doi.org/10.57844/arcadia-085e-3ecf)".

## Installation and Setup

This repository uses conda to manage software environments and installations. You can find operating system-specific instructions for installing miniconda [here](https://docs.conda.io/projects/miniconda/en/latest/). After installing conda and [mamba](https://mamba.readthedocs.io/en/latest/), run the following commands to create the pipeline run environment.

```{bash}
mamba env create -n disco-raman --file envs/dev.yml
conda activate disco-raman
```

Some R packages are not installable through conda. For these, please run:

```{bash}
Rscript envs/install_r_packages.R
```

## Data

The contents and organization of the data used in the analyses for the pub are described in [data](data).

## Overview

### Description of the folder structure

This repository is organized into the following top-level directories.

* **code**:
  collection of R and Python scripts used for the analysis described in the pub as well as for generating the figures.
* **data**:
  subdirectories of spectral data recorded as csv files.
* **envs**:
  YAML file including the packages and dependencies used for creating the conda environment.
* **notebooks**:
  a Jupyter notebook written in Python for generating Figure 2 of the pub.
* **resources**:
  a csv file containing the list of parts used to construct the Raman spectrometer.
* **results**:
  collection of output files used for the analysis described in the pub as well as for generating the figures.

Preview of contents within each directory.

```
─ code
  ├── beer_spectra_analysis.R
  ├── chili_seed_spectra_analysis.R
  ├── plot_raw_spectra.py
  ├── raman-cluster-functions.R
  ├── raman-cluster-plot.R
  └── raman_prediction_functions.R
─ data
  ├── README.md
  ├── algae
  ├── beer
  ├── peppers_flesh
  └── peppers_seeds
─ envs
  └── dev.yml
─ notebooks
  └── Fig-2_Raw-Spectra.ipynb
─ resources
  ├── README.md
  └── OpenRaman_parts_list.csv
─ results
  ├── Fig-2_panel-A.svg
  ├── Fig-2_panel-B.svg
  ├── Fig-2_panel-C.svg
  ├── beer
  └── peppers
```

### Methods

Below is a brief, stepwise overview of how to generate each figure in the pub (and in doing so perform each corresponding analysis). The outputs are stored in the `results/` directory.

1. Generate Figure 2 (visualization of raw spectra) by running `notebooks/Fig-2_Raw-Spectra.ipynb`.
2. Generate Figure 3 (spectral clustering of samples via PCA and LDA) by running `Rscript code/raman-cluster-plot.R` from the root of the repository.
3. Generate Figure 4 (local importance and contribution of spectra in predicting alcohol content) by running `Rscript code/beer_spectra_analysis.R` from the root of the repository.
4. Generate Figure 5 (local importance and contribution of spectra in predicting perceived heat) by running `Rscript code/chili_seed_spectra_analysis.R` from the root of the repository.

### Compute Specifications

The OpenRAMAN spectrometer was connected to a HP 255 G8 Notebook PC running Windows 10. All of the analysis was done on Apple MacBook Pro computers running macOS Sonoma.

## Contributing

See how we recognize [feedback and contributions to our code](https://github.com/Arcadia-Science/arcadia-software-handbook/blob/main/guides-and-standards/guide-credit-for-contributions.md).
