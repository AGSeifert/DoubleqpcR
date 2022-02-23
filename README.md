# DoubleqpcR
R Package accomodating research for DMAS qPCR study

## About
This package is designed for DMAS qPCR data. The functions provided are useful for analysis and can handle .csv input in a specific format (see below). RDML format is not supported. This package provides the methods and functions used in the almond DMAS qPCR study (LINK).

The package provides some basic workflow but does not claim to be universal. You may have to adapt certain functions for your purpose.

## Installation
Install with devtools, tough I will recommend to clone the repository and work with the raw code if you eg. want to adjust settings.

## Documentation


![overview drawio](https://user-images.githubusercontent.com/73955527/155363318-9657da5a-cbe0-4fc8-a179-aaa85b06a91a.png)


### Input Data

The input data has to be in csv format (or for downstream analysis a R dataframe). Two types are needed:
  - Fluorescence table: raw fluorescence qPCR curve data (optional for some parts)
  - Cq Table: a table with the two primer samples / concentration and optional Cq values

** Per analysis only 1 Table of each is considerd ** This means that if you have to combine your csv files before and make sure the wells are named individually. (in future versions this will be adressed, but for now it is what it is).

Example fluorescence data:

| Cycle | A1 | A2 |
| --- | --- | --- |
| 1 | 3.5 | 5.2 |
| ... | ... | ... |
| 50 | 40.4 | 24.5 |

Example Cq Table:

| type | sample | well | "Cq value type1" | "Cq value type2" | ... |
| ---  | ---  | ---  | ---              | ---              | --- |
| gen A | 100 | A1   | 17.2             | 18.0             | ... |
| gen B | 0   | B1   | 2.5              | 3.0              | ... |

Important: the names of individual wells have to be coherent in both tables.

Sample is normally the experiment. It needs to be set to a number if later used in regression. !careful!
TODO: check further requirements

### Functions / Workflow

#### Input

- `read.cqTable()` will read the Cq table.
- `read.fluorescenceTable()` will read the fluorescence data.

> only .csv at this point. 

#### Cq calculation

- `calc.Cq()` can calculate Cq values based on second / third derivative of an fitted drc model.
- `calc.Cq.qpcR()` is the same but with the qpcR package.

#### qCPR curves

- `plot.curve()` will plot all or per sample Amplification curves

#### Cq plots

## References
Packages used:  
(TODO linking)

- investR
- qpcr
- caret
- outliers
- ggplot2

## Cite
(TODO)
