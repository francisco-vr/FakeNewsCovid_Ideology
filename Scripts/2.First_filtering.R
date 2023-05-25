#Load Packages

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("tidyverse","dplyr","haven","ggplot2","readxl","summarytools", "patchwork","stringr",
              "tidyr","kableExtra","psych", "MASS", "foreign", "data.table","gtools","lubridate","AER",
              "xtable","pBrackets","Hmisc","ri","ggpubr", "stargazer", "Rmisc","wesanderson", "gridExtra","ggmosaic",
              "vcd", "plyr", "ggannotate","scales")
ipak(packages)


## Load data ##

cDF <-read.csv("Data/InputData/test5.csv", na.strings=c("","NA") )


# select just the valid surveys

cDF <-cDF%>%
  slice(-c(1,2))

cDF <-filter(cDF, Consent=='1')%>%
  dplyr::filter(Finished=="1")

cDF <-dplyr::select(cDF, Age:SC0)


## Create three new variables: Homophily index, digital citizenship and political position

# Eliminate incomplete surveys

cDF <-cDF%>%
  drop_na("DigiCit_14")
