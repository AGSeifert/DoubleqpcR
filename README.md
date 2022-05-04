# DoubleqpcR

<img align="right" width="200" src="https://user-images.githubusercontent.com/73955527/166162800-318a29a1-7e39-42d6-bad1-ef0dc28c3332.png">

R Package accomodating research for DMAS-qPCR experiments.

**WORK IN PROGRESS**  
This package is still in preview status. Not all functionality is included jet. The first Version will be released when the corresponding publication is available in literature.

## About
This package is designed for DMAS-qPCR data. The functions provided are useful for analysis and can handle .csv input in a specific format (see below). RDML format is not supported. This package provides the methods and functions used in the "Detection of almond adulteration by genotyping of sweet and bitter almonds (Prunus dulcis) with double-mismatch allele-specific qPCR (DMAS-qPCR)" study (LINK).

A vignette (introduction) with the corresponding data from the publication listed above is available! The vignette is also available in inst/test dir with a [formatted html](https://raw.githack.com/LucasFVoges/DoubleqpcR/main/inst/test/test.html)

The package provides some basic workflow but does not claim to be universal. You may have to adapt certain functions for your purpose.

## Installation

Install with devtools, tough I recommend to clone the repository and work with the raw code if you eg. want to adjust functions or reuse them in your code.

`devtools::install_github("LucasFVoges/DoubleqpcR", build_vignettes = FALSE)`

And to include the vignette, which is also accessible in the inst/test folder from here.

`devtools::install_github("LucasFVoges/DoubleqpcR", build_vignettes = TRUE)`

## Idea

### Shifted square root representation of a dilution series

As reported for the DMAS-qPCR (Lefever et al. 2019) [Nature.com](https://www.nature.com/articles/s41598-019-38581-z) the Delta Cq values for a de- and ascending line if plotted against the steady dilution. To make a regression model a continous data representation is neccesary. A plot against the exact proportion forms a curve which can be fitted by a 3rd degree polynomial.

To transform this into a linear reprensentation a shift to 0 is needed as well as the square root transformation. Therefore, to use the hole data in one regression we center 50 % at 0 wich will go up and down to 50 representing 0 and 100 % of target genotype.

To calculate the corresponding concentration from this shifted values, again a transformation is needed. This can be achieved with small constraints with the following formula:

<img class="center" width="500" src="https://user-images.githubusercontent.com/73955527/166445391-81ffc119-57b5-48b2-9253-15d6f26a8b25.png">

where x is the shifted root of concentration (-sqrt(50) < x < sqrt(50)) and s_x is the sign of x.

## Documentation

![overview drawio](https://user-images.githubusercontent.com/73955527/166452505-61444692-5419-4585-b218-e849706b5875.png)


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

##### Data Handling

- `read.cqTable()` will read the Cq table.
- `read.fluorescenceTable()` will read the fluorescence data.

> only .csv at this point. 

##### Cq calculation

- `calc.Cq()` can calculate Cq values based on second / third derivative of an fitted drc model.
- `calc.Cq.qpcR()` is the same but with the qpcR package.

##### qCPR curves

- `plot.curve()` will plot all or per sample Amplification curves

##### Cq Value table

- `table.Cq()` gives an overview of the Cq values (outliers can be removed)

#### Make Data Object for further analasys

- `make.Cq.data()` will generate the main list of dataframes (outliers can be removed)
- `combineSubsamples()` will combine e.g. 100.a, 100.b to one sample 100
- `Cq.data.mean()` will generate a summery of the data (mean and sd)
- `Cq.data.df()` dataframe report
- `delta.Cq.data()` returns a dataframe of Delta Cq values.

> The connection to fluorescence data will be cut at this point.

#### Cq plots

- `plot.Cq()`
- `plot.Cq.efficiency()`

#### Delta Cq plots

- `plot.delta.Cq.box()` Boxplots
- `plot.delta.Cq.overview()` Data overview

#### Regression Analsysis
- `regression.delta.Cq()` will perform model generation, plot and cross validation
- the shifted square root transformation can be used for the regression analysis.
- `reTransform()` will transform the values back to percentages / proportions.

## References
Packages used in this project:  

- investR - [GitHub](https://github.com/bgreenwell/investr)

> Greenwell, B. M.; Schubert Kabban, C. M., investr: an R package for inverse estimation. R Journal **2014**, 6 (1), 90-100.

- qpcr - [Spiess.de](http://www.dr-spiess.de/qpcR.html)

> Ritz, C.; Spiess, A.-N., qpcR: an R package for sigmoidal model selection in quantitative real-time polymerase chain reaction analysis. Bioinformatics **2008**, 24 (13), 1549-1551.

- caret

> Kuhn, M., Building Predictive Models in R Using the caret Package. Journal of Statistical Software **2008**, 28 (5), 1-26.

- outliers - [Komsta.net](http://www.komsta.net/software)

> Komsta, L., Processing data for outliers. The Newsletter of the R Project Volume 6/2, May 2006 2006, 6, 10.

- ggplot2

> Wickham, H., Ggplot2: Elegant graphics for data analysis. Springer-Verlag New York: 2016.

## Cite
If you want to cite this package please use the following citation:

```
TODO
```
