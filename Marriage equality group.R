#lots of duplicates in the data so we first need to get our data clean#
#Steps and corresponding actions#
#1. Remove the duplicates in dataset#
#2A. Percentage of Yes per state#
#2B. Bar chart for above percentage#
#3. Per state participation#
#4. Mapping#
#5. Relationship between gender ratio & Yes rate#
-------------------------------------------------------------------------------------------------
#1. Remove the duplicates#
library(dplyr)
female = read.csv("C:/Users/Administrator/Desktop/Marriage Equality/ResponseFemale.csv")
male = read.csv("C:/Users/Administrator/Desktop/Marriage Equality/ResponseMale.csv")
total = read.csv("C:/Users/Administrator/Desktop/Marriage Equality/ResponseTotal.csv")

dup_free_female = distinct(female)
dup_free_male = distinct(male)
dup_free_total = distinct(total)

#2A.Yes per state#
library(dplyr)
AMLPS <- read.csv("C:/Users/Administrator/Desktop/Marriage Equality/AMLPS_RESP_2017_17112017134913689.csv", header=TRUE, stringsAsFactors = FALSE)

state = c("New South Wales", "Victoria", "Queensland", "South Australia", 
          "Western Australia", "Tasmania", "Northern Territory", 
          "Australian Capital Territory", "Australia")
test_ampls = AMLPS %>% 
  filter(Response %in% c("Response clear - Total", "Response clear - Yes", "Response clear - No")) %>%
  filter(Measure == 'Percentage (%)' & `Federal.Electoral.Division` %in% state)

yes <- test_ampls[which(test_ampls$Response == "Response clear - Yes"),]
no <- test_ampls[which(test_ampls$Response == "Response clear - No"),]

-----------------------------------------------------------------------------------------------------
#2B Bar chart#
library(ggplot2)
#Yes#
yes %>%
  ggplot()+
  geom_bar(aes(x= Federal.Electoral.Division,y= Value , fill=Federal.Electoral.Division), stat = "identity") +
  xlab("State") + ylab("Yes (%)") +
  ggtitle("Yes! Responses Per State")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(hjust=0.5) )+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),  panel.grid.minor = element_blank(), axis.line =  element_line(colour = "grey"))+
  theme(panel.border = element_blank(), axis.line.y  = element_line(colour = "grey"))+
  scale_fill_discrete(name = "State")

#No#
no %>%
  ggplot()+
  geom_bar(aes(x= Federal.Electoral.Division,y= Value , fill=Federal.Electoral.Division), stat = "identity") +
  xlab("State") + ylab("No (%)") +
  ggtitle("No Responses Per State")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(hjust=0.5) )+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),  panel.grid.minor = element_blank(), axis.line =  element_line(colour = "grey"))+
  theme(panel.border = element_blank(), axis.line.y  = element_line(colour = "grey")) +
  scale_fill_discrete(name = "State")
#Ideas for changing plot 
#Order from highest to lowest plot
-----------------------------------------------------------------------------------------------------------
#3. Per state participation#

states = c("New South Wales (Total)", "Victoria (Total)", "Queensland (Total)", "South Australia (Total)", 
           "Western Australia (Total)", "Tasmania (Total)", "Northern Territory (Total)", 
           "Australian Capital Territory (Total)", "Australia (Total)")

# data selection
test_m = dup_free_male %>% 
  filter(FederalElectoralDivison %in% states, Participation %in% "Participation rate (%)")
test_fm = dup_free_female %>%
  filter(FederalElectoralDivison %in% states, Participation %in% "Participation rate (%)")
# get % participation response
a_m = test_m %>%
  filter(Participation == "Participation rate (%)") 
a_fm = test_fm %>%
  filter(Participation == "Participation rate (%)") 

for_plot = cbind(a_m$FederalElectoralDivison,a_m$Total.Males.b.,a_fm$Total.Females.b.)
for_plot = as.data.frame(for_plot) 
for_plot$V2 = as.numeric(as.character(for_plot$V2))
for_plot$V3 = as.numeric(as.character(for_plot$V3))
install.packages("tidyr")
library(tidyr)
for_plot %>%
  gather(V2,V3,key='gender',value="participation (%)") %>%
  ggplot() +
  geom_bar(aes(y=`participation (%)`,x=V1, fill=gender),stat = 'identity',position = "dodge")+
  coord_flip()+
  scale_fill_manual(values = c("blue", "Red"), name = "State/Gender", labels = c("Male ","Female"))
#is not working :( need to check again#

------------------------------------------------------------------------------------------------------
# install.packages("")
  install.packages("rgdal")
install.packages("tmap")

library(dplyr)
library(rgdal)
library(tmap)
#---------------------------------------------------------------------------#
### Call data
total <- read.csv("ResponseTotal.csv")
map <- readOGR(dsn="101_electoral_boundaries/COM20111216_ELB_region.shp",layer="COM20111216_ELB_region")

# Find out what variables are measures 
unique(total$Participation)

# Filter on different measurement parametes 
total_sub <- dup_free_total[which(dup_free_total$Participation == "Participation rate (%)"),]

# Make column names the same to merge
total$ELECT_DIV <- total$FederalElectoralDivison

# Merge data 
map1 <- merge(map, total_sub, by = "ELECT_DIV")

# Filter on state
map_vic <- map1[which(map1$STATE == "VIC"),]

# find variables
names(map1)

# Create map using 'fill' to define the variable
qtm(shp = map_vic, fill = "X18.19.years", fill.palette = "-Blues")

qtm(shp = map_vic, fill = c("X18.19.years", "X75.79.years"), fill.palette = "Blues", ncol = 2)

--------------------------------------------------------------------------------------------------------
#5. Relationship between gender ratio & Yes rate#
#some code, based on Sadia's code and Kate's to merge two dataframe's to combine info about 
#total male and female responses, state and yes vote

#make a state filter
states = c("New South Wales (Total)", "Victoria (Total)", "Queensland (Total)", "South Australia (Total)", 
           "Western Australia (Total)", "Tasmania (Total)", "Northern Territory (Total)", 
           "Australian Capital Territory (Total)", "Australia Total")
# data selection
test_m = dup_free_male %>% 
  filter(!FederalElectoralDivison %in% states, Participation %in% "Total participants")
test_fm = dup_free_female %>%
  filter(!FederalElectoralDivison %in% states, Participation %in% "Total participants")
# get % participation response
a_m = test_m %>%
  filter(Participation == "Total participants") 
a_fm = test_fm %>%
  filter(Participation == "Total participants")

for_plot = cbind(a_m$FederalElectoralDivison,a_m$Total.Males.b.,a_fm$Total.Females.b.)
for_plot = as.data.frame(for_plot) 
for_plot$V2 = as.numeric(as.character(for_plot$V2))
for_plot$V3 = as.numeric(as.character(for_plot$V3))

#Rename columns to match my other dataframe
colnames(for_plot) <- c("Electorate","Total_Male_Response", "Total_Female_Reponse")

##Why can't get rid of Australia??

##calculate the female to male participation ratio

for_plot$Female_Ratio <- for_plot[,3]/for_plot[,2]


##Add in State column


for_plot$State <-NA 
for_plot$State[1] <- "New South Wales"
for_plot$State[2] <- "Victoria"
for_plot$State[3] <- "Queensland"
for_plot$State[4] <- "South Australia"
for_plot$State[5] <- "Western Australia"
for_plot$State[6] <- "Tasmania"
for_plot$State[7] <- "Northern Territory"
for_plot$State[8] <- "Australian Capital Territory"

******************************????#Need to check the following commands#**
#read in
response <- read.csv("C:/Users/Administrator/Desktop/Marriage Equality/AMLPS_RESP_2017_17112017134913689.csv", header=TRUE, stringsAsFactors = FALSE)

#filter out the state (!)
statefilter = c("New South Wales","Victoria","Queensland","South Australia","Western Australia",
                "Northern Territory" ,"Tasmania", "Australian Capital Territory" ,"Australia")
response_by_electorate <- response %>%
  filter(!Federal.Electoral.Division %in% statefilter)

#create new dataframe
yes_by_electorate <- response_by_electorate[response_by_electorate$RESPONSE_CAT== "RESPCLR_Y" & 
                                              response_by_electorate$Measure== "Percentage (%)", c(8,11)]

#Rename 
#Rename columns to match the other dataframe
colnames(yes_by_electorate) <- c("Electorate","Yes")

#count how many rows check the same before we merge (but doesn¡¯t actually matter I found out it still merges)

nrow(yes_by_electorate)
nrow(for_plot)


# merge data frames by Electorate
gender_yes_electorate <- merge(for_plot, yes_by_electorate, by="Electorate")
nrow(gender_yes_electorate)
#Lost some rows...why? 


#Run a linear regression
summary(lm(Yes~Female_Ratio, data=gender_yes_electorate ))

#Plot result
ggplot(gender_yes_electorate, aes(x=Female_Ratio, y=Yes, label=Electorate )) +
  geom_point(aes(colour=State)) +   
  geom_smooth(method=lm) +
  theme_classic()+
  xlab("Ratio of Females to Males") + ylab("Yes (%)") +
  ggtitle("Does the Ratio of Female to Male Voters Relate to the Outcome of the Marriage Equality Vote?")+
  geom_text(
    data= gender_yes_electorate[gender_yes_electorate$Yes <40 | gender_yes_electorate$Yes >75,],
    size= 4, hjust=-0.1,vjust=0.5,  check_overlap = TRUE, aes(colour = factor(State)))




