### Fake news & ideology on covid-19
### Francisco Villarroel, carlos Hidalgo, Pedro Ávila
## load packages


ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage

packages <- c("here", "tidyverse", "patchwork", "ggeasy", "car", "corrplot", "MASS", "PerformanceAnalytics", "stargazer",
              "sjPlot","dplyr","haven","ggplot2", "patchwork","stringr","insight","MatchIt",
              "tidyr","postHoc","plyr", "gridExtra", "parameters","marginaleffects","stats","wesanderson", "gridExtra","ggmosaic",
              "margins", "plyr","modelsummary", "renderthis")

ipak(packages)
