library(readxl)
library(writexl)

Unit_Type<-read_excel("D:/Google Drive/2017-Fall-Gazetteer/Data Analysis/Group Progress/Jinming/agriculture_units（have classfied）.xlsx",col_names = TRUE)

variable1<-Unit_Type$varibale[Unit_Type$Class==1]##Area
variable2<-Unit_Type$varibale[Unit_Type$Class==2]##Amount
variable3<-Unit_Type$varibale[Unit_Type$Class==3]##Output Value
variable4<-Unit_Type$varibale[Unit_Type$Class==4]##Percentage
variable5<-Unit_Type$varibale[Unit_Type$Class==5]##Engine Power
variable6<-Unit_Type$varibale[Unit_Type$Class==6]##Person
variable7<-Unit_Type$varibale[Unit_Type$Class==7]##Area per person
variable8<-Unit_Type$varibale[Unit_Type$Class==8]##Car
variable9<-Unit_Type$varibale[Unit_Type$Class==9]##Animal

variable1a<-Unit_Type$varibale_UNIT[Unit_Type$Class_UNIT==1]##Area_UNIT
variable2a<-Unit_Type$varibale_UNIT[Unit_Type$Class_UNIT==2]##Amount_UNIT
variable3a<-Unit_Type$varibale_UNIT[Unit_Type$Class_UNIT==3]##Output Value_UNIT
variable4a<-Unit_Type$varibale_UNIT[Unit_Type$Class_UNIT==4]##Percentage_UNIT
variable5a<-Unit_Type$varibale_UNIT[Unit_Type$Class_UNIT==5]##Engine Power_UNIT
variable6a<-Unit_Type$varibale_UNIT[Unit_Type$Class_UNIT==6]##Person_UNIT
variable7a<-Unit_Type$varibale_UNIT[Unit_Type$Class_UNIT==7]##Area per person_UNIT
variable8a<-Unit_Type$varibale_UNIT[Unit_Type$Class_UNIT==8]##Car_UNIT
variable9a<-Unit_Type$varibale_UNIT[Unit_Type$Class_UNIT==9]##Animals_UNIT

a <- read_excel("D:/Google Drive/2017-Fall-Gazetteer/Data Analysis/Group Progress/Jinming/unit_list/Area.xlsx",col_types = "text", col_names = TRUE)
b <- read_excel("D:/Google Drive/2017-Fall-Gazetteer/Data Analysis/Group Progress/Jinming/unit_list/Amount.xlsx",col_types = "text", col_names = TRUE)
c <- read_excel("D:/Google Drive/2017-Fall-Gazetteer/Data Analysis/Group Progress/Jinming/unit_list/OutputValue.xlsx",col_types = "text", col_names = TRUE)
d <- read_excel("D:/Google Drive/2017-Fall-Gazetteer/Data Analysis/Group Progress/Jinming/unit_list/Percentage.xlsx",col_types = "text", col_names = TRUE)
e <- read_excel("D:/Google Drive/2017-Fall-Gazetteer/Data Analysis/Group Progress/Jinming/unit_list/EnginePower.xlsx",col_types = "text", col_names = TRUE)
f <- read_excel("D:/Google Drive/2017-Fall-Gazetteer/Data Analysis/Group Progress/Jinming/unit_list/Person.xlsx",col_types = "text", col_names = TRUE)
g <- read_excel("D:/Google Drive/2017-Fall-Gazetteer/Data Analysis/Group Progress/Jinming/unit_list/Areaperperson.xlsx",col_types = "text", col_names = TRUE)
h <- read_excel("D:/Google Drive/2017-Fall-Gazetteer/Data Analysis/Group Progress/Jinming/unit_list/Car.xlsx",col_types = "text", col_names = TRUE)
k <- read_excel("D:/Google Drive/2017-Fall-Gazetteer/Data Analysis/Group Progress/Jinming/unit_list/Animal.xlsx",col_types = "text", col_names = TRUE)

Mydata <- read_excel("D:/Google Drive/2017-Fall-Gazetteer/Data Analysis/Raw Data/agriculture_raw_panel_full_1114.xlsx",col_names=TRUE,col_types=c(rep("text",3),rep("numeric",3),rep(c("numeric","text"),109)))

##### Unify Area Units #####

for (i in 1:length(variable1a)){
  county1 = unique(Mydata$d[is.na(Mydata[[variable1[i]]])==TRUE & is.na(Mydata[[variable1a[i]]])==FALSE])
  print(county1)
}

for (i in 1:length(variable1a)){
  county2 = unique(Mydata$d[is.na(Mydata[[variable1[i]]])==FALSE & is.na(Mydata[[variable1a[i]]])==TRUE]) 
  print(county2)
}

for (i in 1:length(variable1a)){
  county3 = unique(Mydata[[variable1a[i]]][is.na(Mydata[[variable1[i]]])==FALSE & is.na(Mydata[[variable1a[i]]])==FALSE]) 
  print(county3)
}

for (i in 1:length(variable1)){
  county3 = unique(Mydata[[variable1a[i]]][is.na(Mydata[[variable1[i]]])==FALSE & is.na(Mydata[[variable1a[i]]])==FALSE]) 

  for (j in 1:length(county3)){
    Mydata[which(Mydata[[variable1a[i]]]==county3[j]), variable1[i]] =
      Mydata[[variable1[i]]][which(Mydata[[variable1a[i]]]==county3[j])] * as.numeric(as.vector(t(a[a$Area==county3[j],2])))
    Mydata[[variable1a[i]]][Mydata[[variable1a[i]]]==county3[j]]="hectare"
  }
}

##### Unify Amount Units #####

for (i in 1:length(variable2a)){
  county1 = unique(Mydata$d[is.na(Mydata[[variable2[i]]])==TRUE & is.na(Mydata[[variable2a[i]]])==FALSE])
  print(county1)
}

for (i in 1:length(variable2a)){
  county2 = unique(Mydata$d[is.na(Mydata[[variable2[i]]])==FALSE & is.na(Mydata[[variable2a[i]]])==TRUE]) 
  print(county2)
}

for (i in 1:length(variable2a)){
  county3 = unique(Mydata[[variable2a[i]]][is.na(Mydata[[variable2[i]]])==FALSE & is.na(Mydata[[variable2a[i]]])==FALSE]) 
  print(county3)
}

for (i in 1:length(variable2)){
  county3 = unique(Mydata[[variable2a[i]]][is.na(Mydata[[variable2[i]]])==FALSE & is.na(Mydata[[variable2a[i]]])==FALSE]) 
  
  for (j in 1:length(county3)){
    Mydata[which(Mydata[[variable2a[i]]]==county3[j]), variable2[i]] =
      Mydata[[variable2[i]]][which(Mydata[[variable2a[i]]]==county3[j])] * as.numeric(as.vector(t(b[b$Amount==county3[j],2])))
    Mydata[[variable2a[i]]][Mydata[[variable2a[i]]]==county3[j]]="kilograms"
  }
}

##### Unify Output Value Units #####

for (i in 1:length(variable3a)){
  county1 = unique(Mydata$d[is.na(Mydata[[variable3[i]]])==TRUE & is.na(Mydata[[variable3a[i]]])==FALSE])
  print(county1)
}

for (i in 1:length(variable3a)){
  county2 = unique(Mydata$d[is.na(Mydata[[variable3[i]]])==FALSE & is.na(Mydata[[variable3a[i]]])==TRUE]) 
  print(county2)
}

for (i in 1:length(variable3a)){
  county3 = unique(Mydata[[variable3a[i]]][is.na(Mydata[[variable3[i]]])==FALSE & is.na(Mydata[[variable3a[i]]])==FALSE]) 
  print(county3)
}

for (i in 1:length(variable3)){
  county3 = unique(Mydata[[variable3a[i]]][is.na(Mydata[[variable3[i]]])==FALSE & is.na(Mydata[[variable3a[i]]])==FALSE]) 
  
  for (j in 1:length(county3)){
    Mydata[which(Mydata[[variable3a[i]]]==county3[j]), variable3[i]] =
      Mydata[[variable3[i]]][which(Mydata[[variable3a[i]]]==county3[j])] * as.numeric(as.vector(t(c[c$OutputValue==county3[j],2])))
    Mydata[[variable3a[i]]][Mydata[[variable3a[i]]]==county3[j]]="yuan"
  }
}

##### Unify Percentage Units #####

for (i in 1:length(variable4a)){
  county1 = unique(Mydata$d[is.na(Mydata[[variable4[i]]])==TRUE & is.na(Mydata[[variable4a[i]]])==FALSE])
  print(county1)
}

for (i in 1:length(variable4a)){
  county2 = unique(Mydata$d[is.na(Mydata[[variable4[i]]])==FALSE & is.na(Mydata[[variable4a[i]]])==TRUE]) 
  print(county2)
}

for (i in 1:length(variable4a)){
  county3 = unique(Mydata[[variable4a[i]]][is.na(Mydata[[variable4[i]]])==FALSE & is.na(Mydata[[variable4a[i]]])==FALSE]) 
  print(county3)
}


for (i in 1:length(variable4)){
  county3 = unique(Mydata[[variable4a[i]]][is.na(Mydata[[variable4[i]]])==FALSE & is.na(Mydata[[variable4a[i]]])==FALSE]) 
  
  for (j in 1:length(county3)){
    Mydata[which(Mydata[[variable4a[i]]]==county3[j]), variable4[i]] =
      Mydata[[variable4[i]]][which(Mydata[[variable4a[i]]]==county3[j])] * as.numeric(as.vector(t(d[d$Percentage==county3[j],2])))
    Mydata[[variable4a[i]]][Mydata[[variable4a[i]]]==county3[j]]="%"
  }
}

##### Unify Engine Power Units #####

for (i in 1:length(variable5a)){
  county1 = unique(Mydata$d[is.na(Mydata[[variable5[i]]])==TRUE & is.na(Mydata[[variable5a[i]]])==FALSE])
  print(county1)
}

for (i in 1:length(variable5a)){
  county2 = unique(Mydata$d[is.na(Mydata[[variable5[i]]])==FALSE & is.na(Mydata[[variable5a[i]]])==TRUE]) 
  print(county2)
}

for (i in 1:length(variable5a)){
  county3 = unique(Mydata[[variable5a[i]]][is.na(Mydata[[variable5[i]]])==FALSE & is.na(Mydata[[variable5a[i]]])==FALSE]) 
  print(county3)
}

for (i in 1:length(variable5)){
  county3 = unique(Mydata[[variable5a[i]]][is.na(Mydata[[variable5[i]]])==FALSE & is.na(Mydata[[variable5a[i]]])==FALSE]) 
  
  for (j in 1:length(county3)){
    Mydata[which(Mydata[[variable5a[i]]]==county3[j]), variable5[i]] =
      Mydata[[variable5[i]]][which(Mydata[[variable5a[i]]]==county3[j])] * as.numeric(as.vector(t(e[e$EnginePower==county3[j],2])))
    Mydata[[variable5a[i]]][Mydata[[variable5a[i]]]==county3[j]]="kilowatts"
  }
}

##### Unify Person Units #####

for (i in 1:length(variable6a)){
  county1 = unique(Mydata$d[is.na(Mydata[[variable6[i]]])==TRUE & is.na(Mydata[[variable6a[i]]])==FALSE])
  print(county1)
}

for (i in 1:length(variable6a)){
  county2 = unique(Mydata$d[is.na(Mydata[[variable6[i]]])==FALSE & is.na(Mydata[[variable6a[i]]])==TRUE]) 
  print(county2)
}

for (i in 1:length(variable6a)){
  county3 = unique(Mydata[[variable6a[i]]][is.na(Mydata[[variable6[i]]])==FALSE & is.na(Mydata[[variable6a[i]]])==FALSE]) 
  print(county3)
}

for (i in 1:length(variable6)){
  county3 = unique(Mydata[[variable6a[i]]][is.na(Mydata[[variable6[i]]])==FALSE & is.na(Mydata[[variable6a[i]]])==FALSE]) 
  
  for (j in 1:length(county3)){
    Mydata[which(Mydata[[variable6a[i]]]==county3[j]), variable6[i]] =
      Mydata[[variable6[i]]][which(Mydata[[variable6a[i]]]==county3[j])] * as.numeric(as.vector(t(f[f$Person==county3[j],2])))
    Mydata[[variable6a[i]]][Mydata[[variable6a[i]]]==county3[j]]="person"
  }
}

##### Unify AreaPerPerson Units #####

for (i in 1:length(variable7a)){
  county1 = unique(Mydata$d[is.na(Mydata[[variable7[i]]])==TRUE & is.na(Mydata[[variable7a[i]]])==FALSE])
  print(county1)
}

for (i in 1:length(variable7a)){
  county2 = unique(Mydata$d[is.na(Mydata[[variable7[i]]])==FALSE & is.na(Mydata[[variable7a[i]]])==TRUE]) 
  print(county2)
}

for (i in 1:length(variable7a)){
  county3 = unique(Mydata[[variable7a[i]]][is.na(Mydata[[variable7[i]]])==FALSE & is.na(Mydata[[variable7a[i]]])==FALSE]) 
  print(county3)
}

for (i in 1:length(variable7)){
  county3 = unique(Mydata[[variable7a[i]]][is.na(Mydata[[variable7[i]]])==FALSE & is.na(Mydata[[variable7a[i]]])==FALSE]) 
  
  for (j in 1:length(county3)){
    Mydata[which(Mydata[[variable7a[i]]]==county3[j]), variable7[i]] =
      Mydata[[variable7[i]]][which(Mydata[[variable7a[i]]]==county3[j])] * as.numeric(as.vector(t(g[g$AreaPerPerson==county3[j],2])))
    Mydata[[variable7a[i]]][Mydata[[variable7a[i]]]==county3[j]]="hectare"
  }
}

##### Unify Car Units #####

for (i in 1:length(variable8a)){
  county1 = unique(Mydata$d[is.na(Mydata[[variable8[i]]])==TRUE & is.na(Mydata[[variable8a[i]]])==FALSE])
  print(county1)
}

for (i in 1:length(variable8a)){
  county2 = unique(Mydata$d[is.na(Mydata[[variable8[i]]])==FALSE & is.na(Mydata[[variable8a[i]]])==TRUE]) 
  print(county2)
}

for (i in 1:length(variable8a)){
  county3 = unique(Mydata[[variable8a[i]]][is.na(Mydata[[variable8[i]]])==FALSE & is.na(Mydata[[variable8a[i]]])==FALSE]) 
  print(county3)
}

for (i in 1:length(variable8)){
  county3 = unique(Mydata[[variable8a[i]]][is.na(Mydata[[variable8[i]]])==FALSE & is.na(Mydata[[variable8a[i]]])==FALSE]) 
  
  for (j in 1:length(county3)){
    Mydata[which(Mydata[[variable8a[i]]]==county3[j]), variable8[i]] =
      Mydata[[variable8[i]]][which(Mydata[[variable8a[i]]]==county3[j])] * as.numeric(as.vector(t(h[h$Car==county3[j],2])))
    Mydata[[variable8a[i]]][Mydata[[variable8a[i]]]==county3[j]]="cars"
  }
}

##### Unify Animal Units #####

for (i in 1:length(variable9a)){
  county1 = unique(Mydata$d[is.na(Mydata[[variable9[i]]])==TRUE & is.na(Mydata[[variable9a[i]]])==FALSE])
  print(county1)
}

for (i in 1:length(variable9a)){
  county2 = unique(Mydata$d[is.na(Mydata[[variable9[i]]])==FALSE & is.na(Mydata[[variable9a[i]]])==TRUE]) 
  print(county2)
}

for (i in 1:length(variable9a)){
  county3 = unique(Mydata[[variable9a[i]]][is.na(Mydata[[variable9[i]]])==FALSE & is.na(Mydata[[variable9a[i]]])==FALSE]) 
  print(county3)
}

for (i in 1:length(variable9)){
  county3 = unique(Mydata[[variable9a[i]]][is.na(Mydata[[variable9[i]]])==FALSE & is.na(Mydata[[variable9a[i]]])==FALSE]) 
  
  for (j in 1:length(county3)){
    Mydata[which(Mydata[[variable9a[i]]]==county3[j]), variable9[i]] =
      Mydata[[variable9[i]]][which(Mydata[[variable9a[i]]]==county3[j])] * as.numeric(as.vector(t(k[k$Animal==county3[j],2])))
    Mydata[[variable9a[i]]][Mydata[[variable9a[i]]]==county3[j]]="heads"
  }
}

##### 输出 #####
write_xlsx(Mydata,path ="Agriculture Data Completed.xlsx")


