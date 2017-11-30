setwd("C:/Users/Kate/SkyDrive/Openres")

library(dplyr)
library(ggplot2)

response_total <- read.csv("MarriageEqualityResponseTotal.csv", header=TRUE, stringsAsFactors = FALSE)
head(response_total)
response_female <- read.csv("MarriageEqualityResponseFemale.csv", header=TRUE, stringsAsFactors = FALSE)
head(response_female)
response_male <- read.csv("MarriageEqualityResponseMale.csv", header=TRUE, stringsAsFactors = FALSE)
head(response_male)
response <- read.csv("AMLPS_RESP_2017_17112017134913689.csv", header=TRUE, stringsAsFactors = FALSE)
head(response)
str(response)



statefilter = c("New South Wales","Victoria","Queensland","South Australia","Western Australia", "Northern Territory" ,"Tasmania", "Australian Capital Territory" )
response_by_state <- response %>%
  filter(Federal.Electoral.Division %in% statefilter)


yes_by_state <- response_by_state[response_by_state$ï..RESPONSE_CAT== "RESPCLR_Y" & response_by_state$Measure== "Percentage (%)", c(8,11)]

yes_by_state%>%
  ggplot()+
  geom_bar(aes(x= Federal.Electoral.Division,y= Value , fill=Federal.Electoral.Division), stat = "identity") +
  xlab("State") + ylab("Yes (%)") +
  ggtitle("Yes! Responses Per State")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian ()+
  theme( plot.title = element_text(hjust=0.5) )+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),  panel.grid.minor = element_blank(), axis.line =  element_line(colour = "grey"))+
  theme(panel.border = element_blank(), axis.line.y  = element_line(colour = "grey"))


##No


no_by_state <- response_by_state[response_by_state$ï..RESPONSE_CAT== "RESPCLR_N" & response_by_state$Measure== "Percentage (%)", c(8,11)]

no_by_state%>%
  ggplot()+
  geom_bar(aes(x= Federal.Electoral.Division,y= Value , fill=Federal.Electoral.Division), stat = "identity") +
  xlab("State") + ylab("No (%)") +
  ggtitle("No Responses Per State")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian ()+
  theme( plot.title = element_text(hjust=0.5) )+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),  panel.grid.minor = element_blank(), axis.line =  element_line(colour = "grey"))+
  theme(panel.border = element_blank(), axis.line.y  = element_line(colour = "grey"))

