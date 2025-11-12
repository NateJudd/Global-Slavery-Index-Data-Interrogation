



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


### significance testing on prevalence (comparison between two countries?)
#####

n <- dim(my.data)[1] # even

# grouped by vulnerability

vul.vec <- my.data$`Total Vulnerability score (%)`
vul.vec1 <- vul.vec[order(vul.vec)]

vul.group1 <- vul.vec1[1:n/2]
vul.group2 <- vul.vec1[n/2+1:n]

t.test(x=vul.group1,y=vul.group2) # sig

# grouped by govt response
gr.vec <- my.data$`Government response total (%)`
gr.vec1 <- gr.vec[order(gr.vec)]

group1 <- gr.vec1[1:n/2]
group2 <- gr.vec1[n/2+1:n]

t.test(x=group1,y=group2) # sig
