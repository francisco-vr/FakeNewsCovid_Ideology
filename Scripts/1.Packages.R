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

packages <- c("tidyverse", "patchwork", "corrplot", "MASS", "PerformanceAnalytics", "stargazer",
              "sjPlot","dplyr","ggplot2", "stringr","MatchIt","tidyr","plyr", "gridExtra", "parameters",
              "marginaleffects","wesanderson", "gridExtra","ggmosaic", "margins", "plyr","modelsummary")

ipak(packages)
