## Data

### Contents

Each directory within `data/` contains subdirectories of spectral data recorded as csv files. The subdirectories are organized by sample name -- a preview of which is shown below for each group of samples: algae, beer, and peppers (both seeds and flesh). In almost all cases, there are 3 csv files per sample. Each csv file has two columns, `Pixels #` and `Intensity (a.u.)`, which are uncalibrated and output directly by the Raman spectrometer.

For each group of samples, there is also a metadata.csv file that contains non-spectral information regarding each sample. The metadata.csv file for beer, for example, contains information regarding each beer's ABV level, its style, the brewery in which it was made, and tags compiled by Untappd regarding its flavor profile.


### Preview of directory structure

```bash
.
├── algae
│   ├── 13f3
│   │   ├── 2024-03-22-14-06-23.csv
│   │   ├── 2024-03-22-14-06-29.csv
│   │   └── 2024-03-22-14-06-36.csv
│   ├── 13f4
│   │   ├── 2024-03-22-14-03-15.csv
│   │   ├── 2024-03-22-14-03-22.csv
│   │   └── 2024-03-22-14-03-28.csv
|   ├── ...
│   └── metadata.csv
├── beer
│   ├── big-love
│   │   ├── 2024-03-21-13-37-31.csv
│   │   └── 2024-03-21-13-37-50.csv
│   ├── colour-me-murphy
│   │   ├── 2024-03-21-13-59-49.csv
│   │   ├── 2024-03-21-14-00-08.csv
│   │   └── 2024-03-21-14-01-23.csv
|   ├── ...
│   └── metadata.csv
├── peppers_flesh
│   ├── Anah
│   │   ├── 2024-03-22-11-31-24.csv
│   │   ├── 2024-03-22-11-31-36.csv
│   │   └── 2024-03-22-11-32-14.csv
│   ├── Ancho
│   │   ├── 2024-03-22-11-41-53.csv
│   │   ├── 2024-03-22-11-42-05.csv
│   │   └── 2024-03-22-11-42-35.csv
|   ├── ...
│   └── metadata.csv
└── peppers_seeds
    ├── Anah
    │   ├── 2024-03-22-12-33-25.csv
    │   ├── 2024-03-22-12-33-37.csv
    │   └── 2024-03-22-12-33-49.csv
    ├── Ancho
    │   ├── 2024-03-22-12-40-51.csv
    │   ├── 2024-03-22-12-41-03.csv
    │   └── 2024-03-22-12-41-15.csv
    ├── ...
    └── metadata.csv
```
