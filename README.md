# DoubleqpcR
R Package accomodating research for DMAS qPCR study

## About
This package is designed for DMAS qPCR data. The functions provided are useful for analysis and can handle .csv input in a specific format (see below). RDML format is not supported. This package provides the methods and functions used in the almond DMAS qPCR study (LINK).

The package provides some basic workflow but does not claim to be universal. You may have to adapt certain functions for your purpose.

## Installation
Install with devtools, tough I will recommend to clone the repository and work with the raw code if you eg. want to adjust settings.

## Documentation

### Input Data

The input data has to be in csv format (or for downstream analysis a R dataframe). Two types are needed:
  - Fluorescence table: raw fluorescence qPCR curve data (optional for some parts)
  - Cq Table: a table with the two primer samples / concentration and optional Cq values

Example fluorescence data:

| Cycle | A1 | A2 |
| --- | --- | --- |
| 1 | 3.5 | 5.2 |
| ... | ... | ... |
| 50 | 40.4 | 24.5 |

Example Cq Table:

| well | concentration | "Cq value type1" | "Cq value type2" |
| --- | --- | --- | --- |
| A1  | 100 | 17.2 | 18.0 |
| A2  | 0 | 2.9 | 2.1 |

Important: the names of individual wells have to be coherent in both tables.

TODO: check further requirements

### Functions / Workflow

#### qCPR curves

#### Cq plots

## References
Packages used:  
(TODO linking)

- investR
- qpcr
- caret
- outliers

## Cite
(TODO)
