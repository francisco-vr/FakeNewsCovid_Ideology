### matching test ###

source("Scripts/packages.R")

covid <-read.csv("Data/FinalData/covid_fake.csv")

covid<-dplyr::filter(covid, Sex=="Female" | Sex=="Male")

covid$Sex <-relevel(factor(covid$Sex), ref = "Female")
covid$Sex <-as.factor(covid$Sex)



## Matching using ideology, age & gender

mod_match <- matchit(as.factor(IdePol) ~ SC0 + EcoCount + Citizen + Age + Sex + NivEco, method ="cem", data = covid)

summary(mod_match)
plot(mod_match)
dta_m <- match.data(mod_match)

dim(dta_m)

table(dta_m$Sex)





logit_con1 <-glm(Con ~ Sex + IdePol + EcoCount + Citizen + Age + Educ + NivEco + SC0,
                 data = dta_m, family = "binomial"(link = "logit"))

summary(logit_con1)

sjPlot::plot_model(logit_con1)


table(covid$Sex)
