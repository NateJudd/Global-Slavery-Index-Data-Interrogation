

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

# summary of data
str(my.data)
colnames(my.data)
rownames(my.data)
my.data$Country


### Mapping
#####


theme_set(theme_bw())
world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) +
  geom_sf()


world$name_en[37]
my.data$Country[166]
# Viet Nam vs vietnam

# merge map with my.data
length(world$name_en)
length(my.data$Country)
str(world)
test <- countrycode(sourcevar=my.data$Country,origin="country.name",destination="iso3c")
test2 <- countrycode(sourcevar=world$name_en,origin="country.name",destination="iso3c")

indices <-match(x=test,table=test2)
my.countries <- test2[indices]
test2[indices]

world_modified <- world %>% 
  mutate(my_selection = ifelse(admin %in% my.countries,
                               1, NA))

length(world$name)
length(world_modified$name)

dim(my.data)

my.data$Country

new.world <- world[indices,]

new.world$Prevalence <- my.data$`Estimated prevalence of modern slavery per 1,000 population`
new.world$Vulnerability <- my.data$`Total Vulnerability score (%)`
new.world$GovernmentResponse <- my.data$`Government response total (%)`


# prevalence

ggplot(data = new.world) +
  geom_sf(aes(fill=Prevalence)) +
  theme_bw()+xlab("Longitude") + ylab("Latitude")
# colour scale?


ggplot(data = new.world) +
  geom_sf(aes(fill=Vulnerability)) +
  theme_bw()+xlab("Longitude") + ylab("Latitude")

ggplot(data = new.world) +
  geom_sf(aes(fill=GovernmentResponse)) +
  theme_bw()+xlab("Longitude") + ylab("Latitude")





