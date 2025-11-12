# global slavery data set --- data visualisation and EDA (hypothesis testing and model fitting e.g. predictions)

# load packages
library(readxl)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(sf)
library(countrycode)
library("rnaturalearth")
library("rnaturalearthdata")
library(stringr)

# read in data
my.folder <- "C:\\Users\\User\\Documents\\Global Slavery"
my.data.path <- paste(my.folder,"2023-Global-Slavery-Index-Data.xlsx",sep="/")
my.data <- read_excel(my.data.path,sheet=2,skip=2)


### GLMs (to begin to understand the predictors of prevalence)
#####

# what link to use?

# countries
my.data$`Estimated prevalence of modern slavery per 1,000 population`
my.glm <- glm(formula=`Estimated prevalence of modern slavery per 1,000 population`~.,family=poisson,data=my.data)
summary(my.glm)

# population + region + vulnerability+ government response
my.glm.2 <- glm(formula=`Estimated prevalence of modern slavery per 1,000 population`~Population+factor(Region)+`Total Vulnerability score (%)`+`Government response total (%)`,family=poisson,data=my.data)
summary(my.glm.2)
# population shouldn't affect a prevalence i.e. `per 1k pop`

# which vulnerability indices are significant?
my.glm.3 <- glm(formula=`Estimated prevalence of modern slavery per 1,000 population`~`Governance issues`+`Lack of basic needs`+Inequality+`Disenfranchised groups`+`Effects of conflict`,family=poisson,data=my.data)
summary(my.glm.3)
# data points - countries
dim(my.data) # 180 entries

# which government responses are significant?
my.string <- paste("`",paste(as.character(colnames(my.data)[12:17]),collapse="`+`"),"`",sep="")
my.formula.4 <- as.formula(`Estimated prevalence of modern slavery per 1,000 population`~`Survivors of slavery are identified and supported to exit and remain out of modern slavery (%)`+`Criminal justice mechanisms function effectively to prevent modern slavery (%)`+`Coordination occurs at the national and regional level and across borders, and governments are held to account for their response (%)`+`Risk factors, such as attitudes, social systems, and institutions that enable modern slavery are addressed (%)`+`Government and business stop sourcing goods and services produced by forced labour (%)`+`Government response total (%)`)
my.glm.4 <- glm(formula=my.formula.4,family=poisson,data=my.data)
summary(my.glm.4)
# none are significant -- try accounting for factors
my.formula.5 <- as.formula(`Estimated prevalence of modern slavery per 1,000 population`~factor(Region)+`Total Vulnerability score (%)`+`Survivors of slavery are identified and supported to exit and remain out of modern slavery (%)`+`Criminal justice mechanisms function effectively to prevent modern slavery (%)`+`Coordination occurs at the national and regional level and across borders, and governments are held to account for their response (%)`+`Risk factors, such as attitudes, social systems, and institutions that enable modern slavery are addressed (%)`+`Government and business stop sourcing goods and services produced by forced labour (%)`+`Government response total (%)`)
my.glm.5 <- glm(formula=my.formula.5,family=poisson,data=my.data)
summary(my.glm.5)
# smaller p-values but individual goverment interventions still not significant


# interaction between region and key variables?

