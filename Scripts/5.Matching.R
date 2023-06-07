### matching test ###

source("Scripts/1.Packages.R")

covid <-read.csv("Data/FinalData/covid_fake.csv")

covid<-dplyr::filter(covid, Sex=="Female" | Sex=="Male")

covid$Sex <-relevel(factor(covid$Sex), ref = "Female")
covid$Sex <-as.factor(covid$Sex)



## Matching using ideology, age & gender

m.out0 <- matchit(as.factor(IdePol) ~ EcoCount + Citizen + Age + Sex + NivEco, method =NULL, data = covid,
                     distance = "glm", link = "logit")

summary(m.out0)
plot(m.out0)
dta_m <- match.data(mod_match)
dim(dta_m)
table(dta_m$Sex)


## testing with nearest neighbor

m.out1 <- matchit(as.factor(IdePol) ~ EcoCount + Citizen + Age + Sex + NivEco, method ="nearest", data = covid,
                  distance = "glm", link = "logit")

summary(m.out1, un=FALSE)
plot(m.out1, type= "jitter", interactive = FALSE)
plot(m.out1, type = "density", interactive = FALSE, which.xs = ~ EcoCount + Citizen + Age + Sex + NivEco)

## poor performance of nearest neihbor. now we test with full method

m.out2 <- matchit(as.factor(IdePol) ~ EcoCount + Citizen + Age + Sex + NivEco, method ="full", data = covid,
                  distance = "glm", link = "logit")
m.data_full <-match.data(m.out2)
m.out2

summary(m.out2, un=FALSE)
plot(summary(m.out2))

##looks very good. we will test coarced matching btw

m.out3 <- matchit(as.factor(IdePol) ~ EcoCount + Citizen + Age + Sex + NivEco, method = "cem", data = covid,
                  distance = "glm", link = "logit")
m.out3

summary(m.out3, un=FALSE)  #even better than full !
plot(summary(m.out3))

m.data_cem <-match.data(m.out3)
head(m.data_cem)

logit_cem <-glm(Con ~ IdePol * (EcoCount + Citizen + Age + Sex + NivEco),
                data = m.data_cem,family = "binomial"(link = "logit") )
summary(logit_cem)

avg_comparisons(logit_cem,
                variables = "IdePol",
                vcov = ~subclass,
                newdata = subset(m.data_cem, IdePol == 1),
                wts = "weights")

####

m.data <-match.data(m.out2)
head(m.data)


logit_nearest <-glm(Con ~ IdePol + Sex + EcoCount + Citizen + Age + Educ + NivEco,
                 data = m.data, family = "binomial"(link = "logit"))

summary(logit_nearest)

sjPlot::plot_model(logit_nearest)


table(covid$Sex)


######### Ok the fake news about Contraloria didn't work to much. lets try covid

## testing regression with nearest neibor

logit_covid_nearest <-glm(COVID ~ IdePol + Sex + EcoCount + Citizen + Age + Educ + NivEco,
                    data = m.data, family = "binomial"(link = "logit"))
summary(logit_covid_nearest)

avg_comparisons(logit_covid_nearest,
                variables = "IdePol",
                vcov = ~subclass,
                newdata = subset(m.data, IdePol == 1),
                wts = "weights")

## didn't work. lets try with full matching

logit_covid_full <-glm(COVID ~ IdePol + Sex + EcoCount + Citizen + Age + Educ + NivEco,
                          data = m.data_full, family = "binomial"(link = "logit"))
summary(logit_covid_full)

avg_comparisons(logit_covid_full,
                variables = "IdePol",
                vcov = ~subclass,
                newdata = subset(m.data, IdePol == 1),
                wts = "weights")

## didn't work, again. lets try with cem

logit_covid_cem <-glm(COVID ~ IdePol + Sex + EcoCount + Citizen + Age + Educ + NivEco,
                      data = m.data_cem, family = "binomial"(link = "logit"))
summary(logit_covid_cem)

