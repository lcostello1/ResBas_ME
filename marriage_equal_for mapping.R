#############################################################################
### Liam Costello - Research Fellow - University of Melbourne
### School of Ecosystem and Forest Science
### liam.costello@unimelb.edu.au
#############################################################################

### Erase memory and graphs
rm(list=ls())
dev.off() # go to the console and press Ctrl+L

### to comment multiple at once: Cntrl-Shift C 
options(stringsAsFactors = FALSE)

## install.packages("")
library(dplyr)
library(rgdal)
library(tmap)
library(ggplot2)

#---------------------------------------------------------------------------#

# Verification working directory
getwd()

### Setting working directory
#setwd("E:/ResBas/data")
setwd("/Users/liamcostello/Documents/ResBas/data")
### Call data
female <- read.csv("ResponseFemale.csv")
male <- read.csv("ResponseMale.csv")
total <- read.csv("ResponseTotal.csv")
response <- read.csv("AMLPS_RESP_2017_17112017134913689.csv")
map <- readOGR(dsn="shapes/COM20111216_ELB_region.shp",layer="COM20111216_ELB_region")
nat <- readOGR(dsn="shapes/STE_2011_AUST.shp", layer="STE_2011_AUST")


# lots of duplicates in the data so we first need to get our data clean

dup_free_female = distinct(female)
dup_free_male = distinct(male)
dup_free_total = distinct(total)
dup_free_alms = distinct(alms)

# Find out what variables are measures 
unique(total$Participation)

# Filter on different measurement parametes 
total_sub <- dup_free_total[which(dup_free_total$Participation == "Participation rate (%)"),]

# Make column names the same to merge
total_sub$ELECT_DIV <- total_sub$FederalElectoralDivison

# Merge data 
map1 <- merge(map, total_sub, by = "ELECT_DIV")

### Filter on state
map_elec <- map1[which(map1$STATE == "VIC"),]

# find variables
names(map_elec)

# Create map using 'fill' to define the variable
qtm(shp = map_elec, fill = "X18.19.years", fill.palette = "Set3")

# Display maps side-by-side
qtm(shp = map_elec, fill = c("X18.19.years", "X75.79.years"), fill.palette = "Blues", ncol = 2)

#####################################

# Create maps of Yes/No
statefilter = c("New South Wales","Victoria","Queensland","South Australia","Western Australia", "Northern Territory" ,"Tasmania", "Australian Capital Territory" )
response_by_state <- response %>%
  filter(Federal.Electoral.Division %in% statefilter)

names(response)

# Create tables
yes_by_state <- response_by_state[response_by_state$RESPONSE_CAT== "RESPCLR_Y" & response_by_state$Measure== "Percentage (%)", c(8,11)]
no_by_state <- response_by_state[response_by_state$RESPONSE_CAT== "RESPCLR_N" & response_by_state$Measure== "Percentage (%)", c(8,11)]

yes_by_state[-8, ]


# Merge Yes and NO
yes_no_state <- merge(yes_by_state, no_by_state, by="Federal.Electoral.Division")
names(nat)
n <- c("STE_NAME11", "Yes", "No")
names(yes_no_state) <- n

# Merge back into SpatialDataFrame
map_yn_state <- merge(nat, yes_no_state, by="STE_NAME11")

# Print maps 
tm_shape(map_yn_state) +
  tm_fill("Yes", style = "jenks", palette = "Set3", n=8)

tm_shape(map_yn_state) +
  tm_fill("No", style = "jenks", palette = "Set3", n=8)



