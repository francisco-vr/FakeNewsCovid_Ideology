### matching test ###

source("Scripts/1.Packages.R")

covid <-read.csv("Data/FinalData/covid_fake.csv")

covid<-dplyr::filter(covid, Sex=="Female" | Sex=="Male")

covid$Sex <-relevel(factor(covid$Sex), ref = "Female")
covid$Sex <-as.factor(covid$Sex)

##filter non-partisan in Extreme-moderate

covid <-covid%>%
  dplyr::filter(ExtMod!="Ninguno")


### Testing with partisan/non-partisan
## Matching using ideology, age & gender

m.out0 <- matchit(as.factor(IdePol) ~ EcoCount + Citizen + Age + Sex + NivEco, method =NULL, data = covid,
                     distance = "glm", link = "logit")

summary(m.out0)
plot(m.out0)
dta_m <- match.data(mod_match)
dim(dta_m)
table(dta_m$Sex)


## poor performance of nearest neihbor. now we test with full method

m.out2 <- matchit(as.factor(IdePol) ~ EcoCount + Citizen + Age + Sex + NivEco, method ="full", data = covid,
                  distance = "glm", link = "logit")
m.data_full <-match.data(m.out2)
m.out2

summary(m.out2, un=FALSE)
plot(summary(m.out2))


######### Ok the fake news about Contraloria didn't work to much. lets try covid


## didn't work. lets try with full matching

logit_covid_full <-glm(Con ~ IdePol + Sex + EcoCount + Citizen + Age + Educ + NivEco,
                          data = m.data_full, family = "binomial"(link = "logit"))
summary(logit_covid_full)

avg_comparisons(logit_covid_full,
                variables = "IdePol",
                vcov = ~subclass,
                newdata = subset(m.data, IdePol == 1),
                wts = "weights")

#Testing with extreme-moderate

 #NULL model

m.out4 <-matchit(as.factor(ExtMod) ~ EcoCount + Citizen + Age + Sex + NivEco, method =NULL, data = covid,
                           distance = "glm", link = "logit")


## wITH FULL MATCHING

m.out5 <-matchit(as.factor(ExtMod) ~ EcoCount + Citizen + Age + Sex + NivEco, method ="full", data = covid,
                 distance = "glm", link = "logit")

summary(m.out5)

m.data_ExtMod_full <-match.data(m.out5)
m.out5

summary(m.out2, un=FALSE)
plot(summary(m.out2))

logit_full_extMod <-glm(Con ~ ExtMod + Sex + (EcoCount*Age) + Citizen + Educ + NivEco,
                      data = m.data_ExtMod_full, family = "binomial"(link = "logit"))

summary(logit_full_extMod)


logit_covid_full <-glm(Con ~ IdePol + Sex + EcoCount + Citizen + Age + Educ + NivEco,
                       data = m., family = "binomial"(link = "logit"))
