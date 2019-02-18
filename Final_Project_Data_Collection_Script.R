#Final_Project_Data_Collection_Script
#Chaitra Subramaniam 

library (tidyverse)
library (ggplot2)

#row.names=1 makes sure an additional index column (X) gets imported
data<-read.csv("data/dataset_merged.csv",row.names = 1)

#Matching on Business_Id Function 

business_info_per_county <- function(path,city){
  #takes in path to a county's dataset 
  businesses <- read.csv(paste(path,"businesses.csv",sep=""))
  inspections <- read.csv(paste(path,"inspections.csv",sep=""))
  violations<-read.csv(paste(path,"violations.csv",sep=""))
  
  #joins inspection and businesses and violations on business_id
  business_info <- inspections %>% 
    left_join(businesses, 
              by = c("business_id"="business_id")) %>%
    left_join(violations,
              by = c("business_id"="business_id"))
  
  #Additional Changes
  #Clarifying date _ inspection and date _ violation column 
  names(business_info)[names(business_info)=="date.x"]<-"date_inspection"
  names(business_info)[names(business_info)=="date.y"]<-"date_violation"
  
  #Adding additional info for county to differentiate later
  business_info[,"County"] <- city
  return (business_info)
}

evanston_path <- "data/unprocessed/Evanston/"
ev<-business_info_per_county(evanston_path,"Evanston")

anchorage_path <- "data/unprocessed/Anchorage/"
anchorage<-business_info_per_county(anchorage_path,"Anchorage")

boulder_path <- "data/unprocessed/Boulder/"
boulder<-business_info_per_county(boulder_path,"Boulder")

sf_path <- "data/unprocessed/SF/"
sf<-business_info_per_county(sf_path,"SF")

fortworth_path<-"data/unprocessed/FortWorth/"
fortworth<-business_info_per_county(fortworth_path,"Fortworth")

#Removing blank, unused columns for cleanliness of dataset 
ev[,"latitude"]<-NA
ev[,"longitude"]<-NA
ev[,"critical"]<-NA

anchorage<-subset(anchorage, select = -c(description.x,phone_number))
names(anchorage)[names(anchorage)=="description.y"]<-"description"
anchorage[,"critical"]<-NA

sf<-subset(sf, select = -c(phone_number))
sf[,"code"] <- NA
sf[,"critical"]<-NA

boulder<-subset(boulder, select = -c(description.x,phone_number))
names(boulder)[names(boulder)=="description.y"]<-"description"
boulder[,"critical"]<-NA

fortworth<-subset(fortworth, select = -c(phone_number))
names(fortworth)[names(fortworth)=="Score"]<-"score"

merged_data<- 
  ev%>%
  rbind(anchorage)%>%
  rbind(sf)%>%
  rbind(fortworth)%>%
  rbind(boulder)

names(merged_data)

write.csv(merged_data,"data/processed/dataset_merged.csv")
