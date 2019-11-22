



# LCA_plotter
##### Additional functionalities for LCA modeling


<br>


## Table of Contents  

[Introduction](#introduction)  
[Installation](#installation)  
[Examples](#examples)  
[References](#references)


<br>

## Introduction
This is a package built on top of utilities provided by the [poLCA](https://cran.r-project.org/web/packages/poLCA/index.html) package. 

Latent class analysis (LCA) or Latent profile analysis (LPA), which uses a parametric model to place respondents into classes (or clusters) based on 
their response patterns. In LPA, the number of classes is determined by the expectation-maximization algorithm 
which involves an iterative process until the model converges on a best fit for the data. 
This involves the notion that there should be shared variance within the clusters, and that clusters should be 
empirically distinct from each other. For more detailed introduction to the method, please refer [here](https://stats.idre.ucla.edu/mplus/seminars/lca/)

LCA has been extensively used in social science however it is poorly implemented in R compared with its counterparts Mplus or STATA.

The poLCA package only offers the following 3D plots:

<p align="center">
  <img src = "https://github.com/DavidykZhao/LCA_plotter/blob/master/materials/3dplot.png" width="500" height="400"/>
</p>

This package aims to bridge the gap by providing additional plotting functions to better understand the model restuls.


## Installation

Please install from github:
``` r
devtools::install_github("DavidykZhao/LCA_plotter")
```

## Examples

In the example, I used the dataset of the [World Value Survey](http://www.worldvaluessurvey.org/wvs.jsp) wave 5. I have cleaned the data and it 
could be found in the materials folder. This data set contains data from 22 countries on their attitudes towards 6 democracy related questions.
** Profile plot **

```{r}
library(poLCA)
#' # Define a formula for the LDA modeling
#' f = with(data, cbind(tax, religion, free_election, state_aid, civil_rights, women)~1)
#' profile_plot(data, num_var, f) # This will yield the plot
```
<p align="center">
  <img src = "https://github.com/DavidykZhao/LCA_plotter/blob/master/materials/profile_plot_pooled.png" width="600" height="500"/>
</p>



## References:
Latent class modeling in [STATA](https://www.stata.com/features/overview/latent-class-analysis/) 


Latent class modeling in [Mplus](https://stats.idre.ucla.edu/mplus/seminars/lca/)


A good blogpost on the use of [poLCA package](https://statistics.ohlsen-web.de/latent-class-analysis-polca/)
