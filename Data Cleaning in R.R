library(readxl)
library(plyr)
library(ggplot2)

Mydata <- read_excel("D:/Google drive/2017-Fall-Gazetteer/Data Analysis/Group Progress/Jinming/Crops/Crops.xlsx",col_types = c(rep("text",4), "numeric", "text"))

#### for data with only unit but without numeric value ####

county1 = unique(Mydata$d[is.na(Mydata$Crops)==TRUE & is.na(Mydata$Crops_UNIT)==FALSE]) #### find all the county with only unit but without numeric value ####
print(county1)                            #### print out these data ####

for (i in 1:length(county1)){             #### view if these units are dragged down from the first column in excel ####
  View(Mydata[Mydata$d==county1[i], ])
}

for (i in 1:length(county1)){             #### if the units are dragged down in excel, then delete them ####
  Mydata$Crops_UNIT[Mydata$d==county1[i] & is.na(Mydata$Crops)==TRUE] = NA
  #print(Mydata$Crops_UNIT[Mydata$d==county1[i] & is.na(Mydata$Crops)==TRUE])
}

#### for data with only numeric value but without units ####

x <- read_excel("D:/Google drive/2017-Fall-Gazetteer/Data Analysis/Group Progress/Jinming/unit_list/Area_list.xlsx",col_types = "text", col_names = TRUE)
y = as.vector(t(x[,1]))          #### input a list of all the units I need, if there is a unit does not match the unit in my list then delete it ####

Mydata$Crops_UNIT[!(Mydata$Crops_UNIT %in% y)] = NA

county2 = unique(Mydata$d[is.na(Mydata$Crops)==FALSE & is.na(Mydata$Crops_UNIT)==TRUE]) #### find all the county with only numeric value but without units ####

compareNearby = function(gbcode, ncounties ){     #### initialize a compareNearby function which is used to compare the unit missing county with other 5-10 nearby counties ####
  #### this function helps me determine which unit should be add to the missing unit column ####
  
  fileLocation = paste("D:\\Google drive\\2017-Fall-Gazetteer\\Data Analysis\\Raw Data\\distance_by_counties\\",
                       sep="", gbcode, ".csv")
  
  gbcode = as.character(gbcode)
  
  distance = read.csv(fileLocation)
  distance$origin = as.character(distance$origin)
  distance$destination = as.character(distance$destination)
  distance = arrange(distance, distance)
  
  subdt = Mydata[Mydata$d %in% distance$destination, ]
  subdt = subdt[is.na(subdt$Crops)==FALSE & is.na(subdt$Crops_UNIT)==FALSE, ]
  
  
  subdt2 = Mydata[Mydata$d == gbcode, ]
  
  
  for (i in (1:dim(distance)[1])){
    
    if ((distance$destination[i] %in% subdt$d) & (length(unique((subdt2$county))) < ncounties) ){
      subdt2 = rbind(subdt2, subdt[subdt$d == distance$destination[i], ])
    }
  }
  subdt2$d = as.factor(subdt2$d)
  p = ggplot(subdt2, aes(x=year, y=Crops, color = d)) + geom_point() + scale_colour_brewer(palette = "Set1")
  print(p)
  
  View(subdt2)
  return(subdt2)
}

#specificCounties = function(county){              #### initialize a specificCounties that compare the unit missing county with a specific county ####
#  newDt = Mydata[Mydata$d %in% county, ]
#  View(newDt)
#  print(ggplot(newDt, aes(x=year, y=Crops, color=d)) + geom_point())
#  #return(newDt)
#}

compareNearby(county2[1], 5)                                                      #### call the compareNearby function ####
Mydata$Crops_UNIT[Mydata$d ==county2[1] & is.na(Mydata$Crops)==FALSE] = "pounds"  #### fill up the unit "pounds" after compare the unit missing county with other 5 nearby counties ####

compareNearby(county2[2], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[2] & is.na(Mydata$Crops)==FALSE] = "tons"

compareNearby(county2[3], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[3] & is.na(Mydata$Crops)==FALSE] = "tons"

compareNearby(county2[4], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[4] & is.na(Mydata$Crops)==FALSE] = "tons"

compareNearby(county2[5], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[5] & is.na(Mydata$Crops)==FALSE] = "pounds"

compareNearby(county2[6], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[6] & is.na(Mydata$Crops)==FALSE] = "pounds"

compareNearby(county2[7], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[7] & is.na(Mydata$Crops)==FALSE] = "pounds"

compareNearby(county2[8], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[8] & is.na(Mydata$Crops)==FALSE] = "pounds"

compareNearby(county2[9], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[9] & is.na(Mydata$Crops)==FALSE] = "tons"

compareNearby(county2[10], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[10] & is.na(Mydata$Crops)==FALSE] = "pounds"

compareNearby(county2[11], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[11] & is.na(Mydata$Crops)==FALSE] = "kilograms"

compareNearby(county2[12], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[12] & is.na(Mydata$Crops)==FALSE] = "tons"

compareNearby(county2[13], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[13] & is.na(Mydata$Crops)==FALSE] = "tons"

compareNearby(county2[14], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[14] & is.na(Mydata$Crops)==FALSE] = "tons"

compareNearby(county2[15], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[15] & is.na(Mydata$Crops)==FALSE] = "tons"

compareNearby(county2[16], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[16] & is.na(Mydata$Crops)==FALSE] = "tons"

compareNearby(county2[17], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[17] & is.na(Mydata$Crops)==FALSE] = "tons"

compareNearby(county2[18], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[18] & is.na(Mydata$Crops)==FALSE] = "tons"

compareNearby(county2[19], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[19] & is.na(Mydata$Crops)==FALSE] = "tons"

compareNearby(county2[20], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[20] & is.na(Mydata$Crops)==FALSE] = "tons"

compareNearby(county2[21], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[21] & is.na(Mydata$Crops)==FALSE] = "tons"

compareNearby(county2[22], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[22] & is.na(Mydata$Crops)==FALSE] = "tons"

compareNearby(county2[23], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[23] & is.na(Mydata$Crops)==FALSE] = "pounds"

compareNearby(county2[24], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[24] & is.na(Mydata$Crops)==FALSE] = "tons"

compareNearby(county2[25], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[25] & is.na(Mydata$Crops)==FALSE] = "tons"

compareNearby(county2[26], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[26] & is.na(Mydata$Crops)==FALSE] = "tons"

compareNearby(county2[27], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[27] & is.na(Mydata$Crops)==FALSE] = "tons"

compareNearby(county2[28], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[28] & is.na(Mydata$Crops)==FALSE] = "tons"

compareNearby(county2[29], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[29] & is.na(Mydata$Crops)==FALSE] = "tons"

compareNearby(county2[30], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[30] & is.na(Mydata$Crops)==FALSE] = "tons"

compareNearby(county2[31], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[31] & is.na(Mydata$Crops)==FALSE] = "tons"

compareNearby(county2[32], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[32] & is.na(Mydata$Crops)==FALSE] = "tons"

compareNearby(county2[33], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[33] & is.na(Mydata$Crops)==FALSE] = "pounds"

compareNearby(county2[34], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[34] & is.na(Mydata$Crops)==FALSE] = "pounds"

compareNearby(county2[35], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[35] & is.na(Mydata$Crops)==FALSE] = "pounds"

compareNearby(county2[36], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[36] & is.na(Mydata$Crops)==FALSE] = "pounds"

compareNearby(county2[37], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[37] & is.na(Mydata$Crops)==FALSE] = "pounds"

compareNearby(county2[38], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[38] & is.na(Mydata$Crops)==FALSE] = "tons"

compareNearby(county2[39], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[39] & is.na(Mydata$Crops)==FALSE] = "tons"

compareNearby(county2[40], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[40] & is.na(Mydata$Crops)==FALSE] = "pounds"

compareNearby(county2[41], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[41] & is.na(Mydata$Crops)==FALSE] = "tons"

compareNearby(county2[42], 5)
Mydata$Crops_UNIT[Mydata$d ==county2[42] & is.na(Mydata$Crops)==FALSE] = "tons"


#### Unified Units ####

county3 = unique(Mydata$Crops_UNIT[is.na(Mydata$Crops)==FALSE & is.na(Mydata$Crops_UNIT)==FALSE]) #### find all the units this variable "Crops" has ####

for (i in 1:length(county3)){                               #### unify all the units "tons", "pounds" into "kilograms" ####
  Mydata[which(Mydata$Crops_UNIT==county3[i]), "Crops"] =
    Mydata$Crops[which(Mydata$Crops_UNIT==county3[i])] * as.numeric(as.vector(t(x[x$Area_list==county3[i],2])))
  Mydata$Crops_UNIT[Mydata$Crops_UNIT==county3[i]]="kilograms"
  #print(Mydata$Crops_UNIT[Mydata$d==county1[i] & is.na(Mydata$Crops)==TRUE])
}

#### Output scatter plot ####                           #### after units unification, draw the scatter plot of unit missing county and other 5 nearby counties ####
#### the assumption is that all these 6 counties share the same productivity ####
#### their magnitude should be the same ####
for (i in 1:length(county2)){
  t <- try(compareNearby(county2[i], 5), silent = TRUE)
  if ('try-error' %in% class(t)) next
  else
    compareNearby(county2[i], 5)
  name_i <- county2[i]
  ggsave(filename <- paste(name_i,".png"))
  
}

county1 = unique(Mydata$d[is.na(Mydata$Crops)==TRUE & is.na(Mydata$Crops_UNIT)==FALSE]) #### check if I miss the county which has only units but without numeric value ####
print(county1)
county2 = unique(Mydata$d[is.na(Mydata$Crops)==FALSE & is.na(Mydata$Crops_UNIT)==TRUE]) #### check if I miss the county which has only numeric value but without units ####
print(county2)

###### output file
write.csv(Mydata, file = "Cropsoutput.csv", row.names = FALSE) #### output .csv or .xlsx files ####

