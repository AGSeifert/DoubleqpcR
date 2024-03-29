# DoubleqpcR

<img align="right" width="200" src="https://user-images.githubusercontent.com/73955527/166162800-318a29a1-7e39-42d6-bad1-ef0dc28c3332.png">

R Package accomodating research for DMAS-qPCR experiments.

The default branch `v1.x` will include the most recent release.

**NEWS**
- 20.06.2023: **v1.0.4 Paper Release**
- 09.11.2022: **v1.0.3**
- 25.08.2022: **v1.0.2**
- 11.05.2022: **v1.0.1**
- 04.05.2022: **v1.0.0 Initial Release**

## About
This package is designed for DMAS-qPCR data. The functions provided are useful for analysis and can handle .csv input in a specific format (see below). RDML format is not supported. This package provides the methods and functions used in the study:
> Detection of almonds (*Prunus dulcis*) adulteration by genotyping of sweet and bitter almonds with double-mismatch allele-specific qPCR (DMAS-qPCR)
https://www.sciencedirect.com/science/article/pii/S0956713523002669

![TOC5](https://github.com/LucasFVoges/DoubleqpcR/assets/73955527/2db5f7ff-3393-4c7d-b7e5-1ae2b5ea369c)

Abstract: The main ingredient in marzipan is sweet almonds (Prunus dulcis var. dulcis). Bitter almonds (Prunus dulcis var. amara) are also added to create a more intense flavour. In Germany the proportion of bitter almonds in marzipan is limited to a maximum of 12%. The use of debittered bitter almonds is prohibited. However, currently no analytical technique can reliably monitor compliance with this legal requirement. In this study we used next-generation sequencing technology to sequence the plastid genomes of six sweet cultivars (Larguetta, Ferragnes, Nonpareil, Carmel, Monterey, and Marcona) and six bitter almond batches of different origin (Iran, Morocco, Kyrgyzstan, Spain, Syria, and Turkey) to locate single nucleotide variants (SNVs). Sweet and bitter almonds share the same maternal lineage, but sequence variants on the plastid genome were present in most bitter almond populations. To exploit these differences for detection of bitter almonds in processed products, we used the double-mismatch allele-specific qPCR (DMAS-qPCR) for two polymorphic loci (rpoB, rps4). We provide evidence that the rps4 variant was detectable in almond raw pastes containing (debittered) bitter almonds, showing that the DNA sequence was sufficiently stable throughout food processing. A clear distinction from sweet almonds was observed for samples that contained at least 8% of bitter almonds. Our results present a promising approach to detect adulterations with bitter almonds in marzipan.

### Package 

A vignette (introduction) with the corresponding data from the publication listed above is available! The vignette is also available in inst/test dir with a [formatted html](https://agseifert.github.io/DoubleqpcR/vignette.html)

The package provides some basic workflow but does not claim to be universal or a ready to use Workflow. You may have to adapt certain functions for your own purpose.

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

![overview drawio](https://user-images.githubusercontent.com/73955527/186659805-246e8f40-6193-45a8-bbd4-2f75d015eb6b.png)
[made with draw.io](https://www.drawio.com/)

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

| type | sample | well | "Cq value type1" | "Cq value type2" | ... |
| ---  | ---  | ---  | ---              | ---              | --- |
| gen A | 100 | A1   | 17.2             | 18.0             | ... |
| gen B | 0   | B1   | 2.5              | 3.0              | ... |

Important: the names of individual wells have to be coherent in both tables.

Sample is normally the experiment. It needs to be set to a number if later used in regression. !careful!

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

- drc [PLOS ONE](http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0146021)

> Ritz C. et al., Dose-Response Analysis Using R, PLOS ONE, 10, 2015.

- caret

> Kuhn, M., Building Predictive Models in R Using the caret Package. Journal of Statistical Software **2008**, 28 (5), 1-26.

- outliers - [Komsta.net](http://www.komsta.net/software)

> Komsta, L., Processing data for outliers. The Newsletter of the R Project Volume 6/2, May 2006 2006, 6, 10.

- ggplot2

> Wickham, H., Ggplot2: Elegant graphics for data analysis. Springer-Verlag New York: 2016.

- reshape2 [JStatSoft](http://www.jstatsoft.org/v21/i12/)

> Wickham, H., Reshaping Data with the {reshape} Package. Journal of Statistical Software, 21, 2007.

- scales [GitHub](https://github.com/r-lib/scales)

> Wickham, H. and Seidel, D., scales: Scale Functions for Visualization. 2022.

## Cite
If you want to cite this package please use the following citation:

```
Nils Wax, Lucas F. Voges, Sören H. Wenck, Jana L. Herold, Stephan Seifert, Markus Fischer,
Detection of almonds (Prunus dulcis) adulteration by genotyping of sweet and bitter almonds with double-mismatch allele-specific qPCR (DMAS-qPCR),
Food Control,
Volume 152,
2023,
109866,
ISSN 0956-7135,
https://doi.org/10.1016/j.foodcont.2023.109866.
```
