# ezcor
A tool to perform correlation in a batch mode and return tidy result

## :star: Motivation
**Correlation analysis** is very common, here we wrap several functions for correlation analysis to help us analyze datasets such as **TCGA** (The Cancer Genome Atlas) dataset in bioinformatics

## :arrow_double_down: Installation
``` r
# install.packages("remotes")
remotes::install_github("Byronxy/ezcor")
```
## :rocket: Support functions
```r
ezcor() ##run basic correlation
ezcor_batch() ##run basic correlation in a batch mode
ezcor_bicor() ##run WCGAA Biweight Midcorrelation correlation
ezcor_bicor_batch() ##run WCGAA Biweight Midcorrelation correlation in a batch mode
ezcor_partial_cor() ##run partial correlation
ezcor_partial_cor_batch() ##run partial correlation in a batch mode
```
