
# Datavar

The goal of Datavar is to provide functions of description with stored parameters in a datavar.

## Installation

You can install the released version of Datavar from [Github](https://github.com/GuillaumeMulier/Datavar) with:

``` r
# install.packages("devtools")
devtools::install_github("GuillaumeMulier/Datavar")
```

## Functions

* 3 functions of description specific to the type of variable: **TabQuanti** for quantitative variables, **TabQuali** for categorical variables and **TabBinaire** for binary variables;
* function **Description** that uses the datavar and the 3 functions above to produce a table of description;
* **CreateDatavar** to produce a default datavar that you can custom and save to use after.

## Example

This is a basic example which shows you how to describe mtcars dataset:

``` r
library(Datavar)
DatavarMtcars
# Univariate
Description(mtcars, .Datavar = DatavarMtcars)
# Crossed with variable am
Description(mtcars, y = am, .Datavar = DatavarMtcars)
```

More is shown in the vignette (building).
