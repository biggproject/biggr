# biggr

This is an open-source R package for the statistical analysis of building's data within the framework of [BIGG project](https://www.bigg-project.eu). It contains a set of AI toolbox that allows the clustering, classification and modelling of building time series and its metadata. This package contains the needed functionalities to take advantage of the [BIGG Ontology](http://www.github.com/biggproject/Ontology), thus the authors recommends its usage when elaborating your own data pipelines. This would generate a bigger chance of re-usage among different companies/users.

## How to install?
Once R is installed (>=4.1 is recommended), execute the following sentences to install all dependent libraries:
```
install.packages(c("devtools","pastecs","purrr","arules","glmnet","ranger","rdflib",
"magrittr","parsedate","lubridate","readr","dplyr","tidyr","tibble","zoo",
"roll","padr","quantreg","testthat","kernlab","fastDummies","caret","penalized",
"GA","parallel","mgcv","qgam","FinCal","onlineforecast","matrixStats","gratia")
# Additionally, is recommended to install libraries that are often used in pipelines based on 'biggr' functions
install.packages(c("plotly","ggplot2","carrier","mlflow","mongolite","gridExtra"))
```
Then, install the package through the GitHub installer if you want to use the last version of the code, or install an stable release located in the [releases repository](https://www.github.com/biggproject/biggr/releases) directly using the install.packages command.

```
# Install development version from GitHub 
devtools::install_github("biggproject/biggr")
# Install stable version from the source package (*.tar.gz) 
install.packages(<.tar.gz file>, repos = NULL, type ="source")
```

## License
This R package is licensed under the MIT License. It also depends on other popular open-source R libraries, from which it will retain their licenses.
