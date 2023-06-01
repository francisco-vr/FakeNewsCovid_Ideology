#Load Packages

source("Scripts/1.Packages.R")

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
