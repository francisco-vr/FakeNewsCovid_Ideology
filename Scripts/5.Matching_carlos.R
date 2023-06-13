### matching test ###

source("Scripts/1.Packages.R")

covid <- read.csv("Data/FinalData/covid_fake.csv")

covid <- dplyr::filter(covid, Sex =="Female" | Sex == "Male")

covid$Sex <-relevel(factor(covid$Sex), ref = "Female")
covid$Sex <-as.factor(covid$Sex)



## Create new variables of rightwing (vs the rest) and leftwing (vs rest)
covid <-covid %>% mutate(
  rightwing = ifelse(Ideology2 == "Derecha", "Derecha", "Otro"),
  leftwing = ifelse(Ideology2 == "Izquierda", "Izquierda", "Otro")
)



#### lOGIT RESULTS ####
## Wrapping function for matched data ##
data_fun <- function(data, var_match) {
  m.out2 <- matchit(var_match ~ EcoCount + Citizen + Age + Sex + NivEco, method ="full", data = data,
                    distance = "glm", link = "logit")
  m.data_full2 <<- match.data(m.out2) }

data_fun(data = covid, var_match = as.factor(covid$leftwing)) # con var_match cambiar en base a que hacer el matching



## Wrapping function for compared models ##

models_fun <- function(data, DV, IV) {
  IV_label = IV
  DV_label = DV
  IV <- get(IV, data)
  IV <- as.factor(IV)
  DV <- get(DV, data)
  m1 <- glm(DV ~ IV, data = data, family = binomial(link = "logit"))
  m2 <- glm(DV ~ IV + Age, data = data, family = binomial(link = "logit"))
  m3 <- glm(DV ~ IV + Age + Sex, data = data, family = binomial(link = "logit"))
  m4 <- glm(DV ~ IV + Age + Sex + EcoCount, data = data, family = binomial(link = "logit"))
  m5 <- glm(DV ~ IV + Age + Sex + EcoCount + Citizen, data = data, family = binomial(link = "logit"))
  m6 <- glm(DV ~ IV + Age + Sex + EcoCount + Citizen + Educ, data = data, family = binomial(link = "logit"))
  m7 <- glm(DV ~ IV + Age + Sex + EcoCount + Citizen + Educ + NivEco, data = data, family = binomial(link = "logit"))

  
  RESULT_MODEL <<- stargazer(m1, m2, m3, m4, m5, m6, m7, 
                             title = paste0("Results ", DV_label, ". ",  IV_label, " for matching"), 
                             dep.var.labels = DV_label, 
                             align = TRUE,
                             covariate.labels = IV_label,
                             type="html", 
                             out= paste0(IV_label, DV_label, ".html"))
}

models_fun(data = m.data_full2, IV = "leftwing", DV = "Con") # DV = "COVID" o "Con"; IV = "con lo que se haya hecho el matching"



## filter non-partisan in Extreme-moderate: ocupar solo para ExtMod

covid <-covid %>%
  dplyr::filter(ExtMod!="Ninguno")


