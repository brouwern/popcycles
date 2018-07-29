# popcycles

popcycles is a package for the plotting, analaysis and simulation of ecological population models.

## Installation

You can install the released version of popcycles from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("popcycles")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("brouwern/popcycles")
```

## Example

This is a basic example of logistic population growth.

First, project poulaiton growth

``` r
N.rabbit.logit <- logit_discrete_multistep(alpha = NULL, 
                                K = 3000,
                                r.d = 1,
                                time.steps = 20,
                                Nt = 10)
```

Second, Plot the output
``` r
gg_plot_logit(rabbit,
              title = "Exponential population growth in rabbits") +
xlim(0,20) +
ylim(0,3000) 
```
