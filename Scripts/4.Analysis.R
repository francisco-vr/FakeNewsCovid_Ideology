### Fake news & ideology on covid-19
### Francisco Villarroel, carlos Hidalgo, Pedro Ávila

source("Scripts/packages.R")


covid <-read.csv("Data/FinalData/covid_fake.csv")

## Exploremos qué weá

covid <- covid%>%
  mutate_if(is.character, as.factor)

sapply(covid, class)

corrplot(covid, type = "upper", order = "hclust", tl.col = "black")


# Fit the full model 
full.model <- glm(Con ~., data = covid[,-4], family = binomial(link = "logit"))
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)


covid_numeric <-covid[,c(5,6,7,13:21)]

chart.Correlation(covid_numeric, histogram=TRUE)



####### Logit models #########

#### Contraloria ####
logit_con1 <-glm(COVID ~ as.factor(Sex) + Ideology1 + EcoCount + Citizen,
                 data = covid, family = "binomial"(link = "logit"))

summary(logit_con1)

logit_con2 <-glm(COVID ~ as.factor(Sex) + Ideology2 + EcoCount + Citizen, data = covid, family = "binomial"(link = "logit"))
summary(logit_con2)

logit_con3 <-glm(Con ~ as.factor(Sex) + IdePol + EcoCount + Citizen,
                 data = df, family = "binomial"(link = "logit"))
summary(logit_con3)

logit_con4 <-glm(Con ~ as.factor(Sex) + ExtMod + EcoCount + Citizen,
                 data = df, family = "binomial"(link = "logit"))
summary(logit_con4)


stargazer::stargazer(logit_con1, logit_con2, logit_con3, logit_con4, type = "html", out = "Results/tabla.html")


sjPlot::plot_model(logit_con1)








#### COVID ####
logit_covid1 <-  glm(COVID ~ Age + NivEco + Sex + Educ + EC + LR + PR + Homphily + Bias, 
                     data = df, family = "binomial"(link = "logit"))
summary(logit_covid1)

sjPlot::plot_model(logit_covid1)



#### COVID ####
logit_covid1 <-  glm(COVID ~ Age + NivEco + Sex + Educ + EC + LR + PR, data = df, family = "binomial"(link = "logit"))
summary(logit_covid1)