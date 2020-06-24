
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ezcor

A tool to perform correlation in a batch mode and return tidy result

## :star: Motivation

**Correlation analysis** is very common, here we wrap several functions
for correlation analysis to help us analyze datasets such as **TCGA**
(The Cancer Genome Atlas) dataset in bioinformatics

## :arrow\_double\_down: Installation

``` r
if(!require("remotes")){install.packages("remotes")}
remotes::install_github("Byronxy/ezcor")
```

## :rocket: Support functions

``` r
ezcor() ##run basic correlation
ezcor_batch() ##run basic correlation in a batch mode
ezcor_bicor() ##run WCGAA Biweight Midcorrelation correlation
ezcor_bicor_batch() ##run WCGAA Biweight Midcorrelation correlation in a batch mode
ezcor_partial_cor() ##run partial correlation
ezcor_partial_cor_batch() ##run partial correlation in a batch mode
```

## :sunny: Examples

We select HALLMARK\_HYPOXIA pathway genes from TCGA pan-cancer dataset
to illustrate examples

``` r
data("exprSet", package = "ezcor", envir = environment())
exprSet[1:5,1:5]
#>                   patient tissue B3GALT6  ERRFI1    ENO1
#> TCGA-RZ-AB0B TCGA-RZ-AB0B    UVM  3.4517  0.4865  9.6230
#> TCGA-V3-A9ZX TCGA-V3-A9ZX    UVM  3.5971  0.6880 10.3269
#> TCGA-V3-A9ZY TCGA-V3-A9ZY    UVM  3.3061  0.0014  9.0812
#> TCGA-V4-A9E5 TCGA-V4-A9E5    UVM  3.2841 -0.3940 10.5923
#> TCGA-V4-A9E8 TCGA-V4-A9E8    UVM  3.9929 -0.6873 10.3873
g1 <- colnames(exprSet)[3]
g2 <- colnames(exprSet)[4]
res <- ezcor::ezcor(data= exprSet,
                      split = TRUE,
                      split_var = "tissue",
                      var1 = g1,
                      var2 = g2,
                      cor_method = "spearman",
                      adjust_method = "none",
                      sig_label = TRUE,
                      verbose = TRUE)
head(res)
#>              cor      p.value   method adjust      v1     v2 group pstar
#> ACC   0.43894888 6.512276e-05 spearman   none B3GALT6 ERRFI1   ACC   ***
#> BLCA  0.10167474 4.034319e-02 spearman   none B3GALT6 ERRFI1  BLCA     *
#> BRCA  0.12752295 2.373650e-05 spearman   none B3GALT6 ERRFI1  BRCA   ***
#> CESC  0.03334268 5.625112e-01 spearman   none B3GALT6 ERRFI1  CESC      
#> CHOL -0.15958816 3.525194e-01 spearman   none B3GALT6 ERRFI1  CHOL      
#> COAD  0.04617798 4.349938e-01 spearman   none B3GALT6 ERRFI1  COAD
```


## :writing\_hand: Author

  - [Yi Xiong](https://github.com/Byronxy)
  - [Shixiang Wang](https://github.com/ShixiangWang)

