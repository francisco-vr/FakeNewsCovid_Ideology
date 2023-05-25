### Fake news & ideology on covid-19
### Francisco Villarroel, carlos Hidalgo, Pedro Ávila

# LIBRARIES AND LOAD DATA ----


source("Scripts/packages.R")


## Set theme for plots ##
theme_set(theme_bw(base_family = 'serif'))

# Avoid harmless DPLYR warning message of group_by
options(dplyr.summarise.inform = FALSE)

# disable scientific numbering
options(scipen=999)

## LOAD DATA ##
df <- read.csv("Data/FinalData/df_covid_FN.csv")

## rename index ##
df <- rename(df, ID = X)


## Explore data ##
# Distributions #
ggplot(df, aes(x = Ideolo_1)) + geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "black") +
  scale_x_continuous(n.breaks = n_distinct(df$Ideolo_1)) +
  geom_density(colour = "red")


#df %>%
#  keep(is.numeric) %>%                     # Keep only numeric columns
#  gather() %>%                             # Convert to key-value pairs
#  ggplot(aes(value)) +                     # Plot the values
#  facet_wrap(~ key, scales = "free") +   # In separate panels
#  geom_density()                         # as density


## Change gender variable ##
df <- df %>% mutate(Sex = case_when(
  Genre == 1 ~ "Male",
  Genre == 2 ~ "Female",
  Genre == 3 ~ "Other"
))


## Collapse variable Contraloria ##
df <- df %>% mutate(Con = coalesce(E2TC_1, E2T1a_1, E2T1b_1, E2T1c_1, E2T1d_1,
                                   E2T2a_1, E2T2b_1, E2T2c_1))%>%
  dplyr::filter(!is.na(Con))

# quedan valores NA despues de colapsar y quedan valores 1 y 2...¿no deberían ser 1 y 0?


## Collapse variable COVID ##
df <- df %>% mutate(COVID = coalesce(E2TC_3, E2T1a_3, E2T1b_3, E2T1c_3, E2T1d_3,
                                     E2T2a_3, E2T2b_3, E2T2c_3))%>%
  dplyr::filter(!is.na(COVID))

# quedan valores NA despues de colapsar y quedan valores 1 y 2...¿no deberían ser 1 y 0?

## Age ##

df <-df%>%
  drop_na("Age")


df <-mutate(df, AgeRecod = dplyr::recode(df$Age, "1" = "18 a 29 años","2" = "30 a 40 años","3" = "41 a 65 años",
                                                 "4" = "+66 años"))
table(df$AgeRecod)

## Education ##

df <-mutate(df, EducRec = dplyr::recode(df$Educ, "1" = "Sin Estudios","2" = "Básica","3" = "Media",
                                                "4" = "Superior", "5" = "Postgrado"))
table(df$EducRec)

## Income ##

df <-mutate(df, IncomeRecod = dplyr::recode(df$NivEco, "1" = "Menos de $224.000", "2" = "Entre $224.001 - $448.000",
                                                    "3" = "Ente $448.001 y $1.000.000", "4" = "Entre $1.000.001 - $3.000.000","5" = "Más de $3.000.000"))
table(df$IncomeRecod)

## Genre ##

df <-mutate(df, GenRecod = dplyr::recode(df$Genre, "1" = "Masculino", "2" = "Femenino", "3" = "Otro"))
table(df$GenRecod)

## Political position ##

df <-df%>%
  drop_na("IdePol")


## Notes: Ideology1 = (left,centerleft, center, center-right, right and "without Ideology"
###       Ideology2 = Left, center, right, "without ideology"
###       Represent = if they representer by left, center or right = 1; if not = 0
###       ExMod = if 1,2,3,8,9.10 = "extreme"; if 4,5,6,7 = "moderate". else "Without ideology"


# CREATE IDEOLOGY1

df <-dplyr::mutate(df, IdeoRec = car::recode(df$Ideolo_1, "1:2 = 1; 3:4 = 2; 5:6 = 3; 7:8 = 4; 9:10 = 5")) 

df <-dplyr::mutate(df, IdeoRec = dplyr::recode(df$IdeoRec, "1" = "E. Izquierda","2" = "Centro-Izquierda","3" = "Centro",
                                                       "4" = "Centro-Derecha", "5" = "E. Derecha"))
table(df$IdeoRec)

df<-mutate(df, identity = dplyr::recode(df$IdePol, "0" = "Ninguno", "1" = ''))

#Merge with "no ideology" column

df <-mutate(df, Ideology1 = coalesce(df$IdeoRec, df$identity))


## CREATE IDEOLOGY2

df <-dplyr::mutate(df, IdeoRec2 = car::recode(df$Ideolo_1, "1:4 = 1; 5:6 = 2; 7:10 = 3")) 

df <-dplyr::mutate(df, IdeoRec2 = dplyr::recode(df$IdeoRec2, "1" = "Izquierda","2" = "Centro",
                                               "3" = "Derecha"))
table(df$IdeoRec2)

df<-mutate(df, identity = dplyr::recode(df$IdePol, "0" = "Ninguno", "1" = ''))

#Merge with "no ideology" column

df <-mutate(df, Ideology2 = coalesce(df$IdeoRec2, df$identity))

## CREATE Represent

### "represent" its IdePol. lol

## CREATE ExtMod

df <-dplyr::mutate(df, IdeoRec3 = car::recode(df$Ideolo_1, "1:3 = 1; 8:10 = 1; 4:7 = 0")) 

table(df$IdeoRec3)

df<-mutate(df, identity = dplyr::recode(df$IdePol, "0" = "Ninguno", "1" = ' '))

df <-mutate(df, ExtMod = coalesce(as.character(df$IdeoRec3), as.character(df$identity)))

table(df$ExtMod)


## relevel variables


### CREATE BASE VARIABLES


df$GenRecod <-relevel(factor(df$Sex), ref = "Male")
df$AgeRecod <-relevel(factor(df$AgeRecod), ref = "18 a 29 años")
df$Ideology1 <-relevel(factor(df$Ideology1), ref = "Ninguno")
df$Ideology2 <-relevel(factor(df$Ideology2), ref = "Centro")
df$IdePol <-relevel(factor(df$IdePol), ref = "0")
df$ExtMod <-relevel(factor(df$ExtMod), ref = "0")
df$IncomeRecod <-relevel(factor(df$IncomeRecod), ref = "Menos de $224.000")
df$EducRec <-relevel(factor(df$EducRec), ref = "Postgrado")


## Change outcome variables to 0 and 1 ##
## In Contraloria ##
df$Con[df$Con == 1] <- 0
df$Con[df$Con == 2] <- 1

## In Covid ##
df$COVID[df$COVID == 2] <- 0


## Create other independent variables ##
df <- df %>% mutate(Homphily = Homo_2 + Homo_3 + Homo_6,
                    Bias = Homo_4 + Homo_5 + Homo_7)


df <-df%>%
  mutate(skill = DigiCit_1 + DigiCit_2 + DigiCit_3)

df <-df%>%
  mutate(Global = DigiCit_4 + DigiCit_5)

df <-df%>%
  mutate(EC = DigiCit_7 + DigiCit_8 + DigiCit_9 + DigiCit_6)

df <-df%>%
  mutate(CA = DigiCit_10 + DigiCit_11)

df <-df%>%
  mutate(DPol = DigiCit_12 + DigiCit_13 + DigiCit_14)

## create Echo chamber count

df$EcoCount <-df$Homo_1+df$Homo_2+df$Homo_3+df$Homo_4+df$Homo_5+df$Homo_6+df$Homo_7

hist(df$EcoCount)

## Create Digital Citizenship count

df$Citizen <-df$DigiCit_1+df$DigiCit_2+df$DigiCit_3+df$DigiCit_4+df$DigiCit_5+df$DigiCit_6+df$DigiCit_7+df$DigiCit_8+df$DigiCit_9+
  df$DigiCit_10+df$DigiCit_11+df$DigiCit_12+df$DigiCit_13+df$DigiCit_14

hist(df$Citizen)

###

covid <-dplyr::select(df, Sex, Con, COVID, Age, NivEco, Educ, GenRecod, Ideology1, Ideology2, IdePol,
                      ExtMod, Homphily, Bias, skill, Global, EC, CA, DPol, EcoCount, Citizen, SC0)

### save intermediate DataFrame

write.csv(covid, file = "Data/FinalData/covid_fake.csv")
