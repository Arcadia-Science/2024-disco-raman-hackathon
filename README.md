# 2024-disco-raman-hackathon

[![run with conda](http://img.shields.io/badge/run%20with-conda-3EB049?labelColor=000000&logo=anaconda)](https://docs.conda.io/projects/miniconda/en/latest/)

## Purpose

This repo contains code and results pertaining to the Raman hackathon carried out by the Discovery team at the Discovery offsite in March 2024.

## Installation and Setup

This repository uses conda to manage software environments and installations. You can find operating system-specific instructions for installing miniconda [here](https://docs.conda.io/projects/miniconda/en/latest/). After installing conda and [mamba](https://mamba.readthedocs.io/en/latest/), run the following command to create the pipeline run environment.

```{bash}
TODO: Replace <NAME> with the name of your environment
mamba env create -n <NAME> --file envs/dev.yml
conda activate <NAME>
```

## Data

TODO: Add details about the description of input / output data and links to Zenodo depositions, if applicable.

## Overview

### Description of the folder structure

### Methods

TODO: Include a brief, step-wise overview of analyses performed.

> Example:
>
> 1. Download scripts using `download.sh`.
> 2. Preprocess using `./preprocessing.sh -a data/`
> 3. Run analysis script using `analysis.Rscript`
> 4. Generate figures using `pub/make_figures.R`.

### Compute Specifications

TODO: Describe what compute resources were used to develop and run the analysis. For example, you could list the operating system, number of cores, RAM, and storage space. You should log any major changes to the compute specifications here as they happen.

## Contributing

See how we recognize [feedback and contributions to our code](https://github.com/Arcadia-Science/arcadia-software-handbook/blob/main/guides-and-standards/guide-credit-for-contributions.md).
