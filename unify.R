library(readxl)
library(writexl)

Unit_Type<-read_excel("D:/Google 云端硬盘/2017-Fall-Gazetteer/Data Analysis/Group Progress/Jinming/agriculture_units（have classfied）.xlsx",col_names = TRUE)

variable1<-Unit_Type$varibale[Unit_Type$Class==1]##面积
variable2<-Unit_Type$varibale[Unit_Type$Class==2]##产量
variable3<-Unit_Type$varibale[Unit_Type$Class==3]##产值
variable4<-Unit_Type$varibale[Unit_Type$Class==4]##占比
variable5<-Unit_Type$varibale[Unit_Type$Class==5]##动力
variable6<-Unit_Type$varibale[Unit_Type$Class==6]##人
variable7<-Unit_Type$varibale[Unit_Type$Class==7]##人均面积单位
variable8<-Unit_Type$varibale[Unit_Type$Class==8]##台数
variable9<-Unit_Type$varibale[Unit_Type$Class==9]##头数

variable1a<-Unit_Type$varibale_UNIT[Unit_Type$Class_UNIT==1]##面积_UNIT
variable2a<-Unit_Type$varibale_UNIT[Unit_Type$Class_UNIT==2]##产量_UNIT
variable3a<-Unit_Type$varibale_UNIT[Unit_Type$Class_UNIT==3]##产值_UNIT
variable4a<-Unit_Type$varibale_UNIT[Unit_Type$Class_UNIT==4]##占比_UNIT
variable5a<-Unit_Type$varibale_UNIT[Unit_Type$Class_UNIT==5]##动力_UNIT
variable6a<-Unit_Type$varibale_UNIT[Unit_Type$Class_UNIT==6]##人_UNIT
variable7a<-Unit_Type$varibale_UNIT[Unit_Type$Class_UNIT==7]##人均面积单位_UNIT
variable8a<-Unit_Type$varibale_UNIT[Unit_Type$Class_UNIT==8]##台数_UNIT
variable9a<-Unit_Type$varibale_UNIT[Unit_Type$Class_UNIT==9]##头数_UNIT

a <- read_excel("D:/Google 云端硬盘/2017-Fall-Gazetteer/Data Analysis/Group Progress/Jinming/unit_list/面积单位.xlsx",col_types = "text", col_names = TRUE)
b <- read_excel("D:/Google 云端硬盘/2017-Fall-Gazetteer/Data Analysis/Group Progress/Jinming/unit_list/产量单位.xlsx",col_types = "text", col_names = TRUE)
c <- read_excel("D:/Google 云端硬盘/2017-Fall-Gazetteer/Data Analysis/Group Progress/Jinming/unit_list/产值单位.xlsx",col_types = "text", col_names = TRUE)
d <- read_excel("D:/Google 云端硬盘/2017-Fall-Gazetteer/Data Analysis/Group Progress/Jinming/unit_list/占比单位.xlsx",col_types = "text", col_names = TRUE)
e <- read_excel("D:/Google 云端硬盘/2017-Fall-Gazetteer/Data Analysis/Group Progress/Jinming/unit_list/动力单位.xlsx",col_types = "text", col_names = TRUE)
f <- read_excel("D:/Google 云端硬盘/2017-Fall-Gazetteer/Data Analysis/Group Progress/Jinming/unit_list/人单位.xlsx",col_types = "text", col_names = TRUE)
g <- read_excel("D:/Google 云端硬盘/2017-Fall-Gazetteer/Data Analysis/Group Progress/Jinming/unit_list/人均面积单位.xlsx",col_types = "text", col_names = TRUE)
h <- read_excel("D:/Google 云端硬盘/2017-Fall-Gazetteer/Data Analysis/Group Progress/Jinming/unit_list/台数单位.xlsx",col_types = "text", col_names = TRUE)
k <- read_excel("D:/Google 云端硬盘/2017-Fall-Gazetteer/Data Analysis/Group Progress/Jinming/unit_list/头数单位.xlsx",col_types = "text", col_names = TRUE)

Mydata <- read_excel("D:/Google 云端硬盘/2017-Fall-Gazetteer/Data Analysis/Raw Data/agriculture_raw_panel_full_1114.xlsx",col_names=TRUE,col_types=c(rep("text",3),rep("numeric",3),rep(c("numeric","text"),109)))

##### 统一面积单位 #####

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
      Mydata[[variable1[i]]][which(Mydata[[variable1a[i]]]==county3[j])] * as.numeric(as.vector(t(a[a$面积单位==county3[j],2])))
    Mydata[[variable1a[i]]][Mydata[[variable1a[i]]]==county3[j]]="公顷"
  }
}

##### 统一产量单位 #####

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

for (i in 1:length(variable2a)){
  Mydata[[variable2a[i]]][Mydata[[variable2a[i]]]=="亩"]="斤"
  Mydata[[variable2a[i]]][Mydata[[variable2a[i]]]=="担子"]="担"
}

for (i in 1:length(variable2)){
  county3 = unique(Mydata[[variable2a[i]]][is.na(Mydata[[variable2[i]]])==FALSE & is.na(Mydata[[variable2a[i]]])==FALSE]) 
  
  for (j in 1:length(county3)){
    Mydata[which(Mydata[[variable2a[i]]]==county3[j]), variable2[i]] =
      Mydata[[variable2[i]]][which(Mydata[[variable2a[i]]]==county3[j])] * as.numeric(as.vector(t(b[b$产量单位==county3[j],2])))
    Mydata[[variable2a[i]]][Mydata[[variable2a[i]]]==county3[j]]="千克"
  }
}

##### 统一产值单位 #####

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

for (i in 1:length(variable3a)){
  Mydata[[variable3a[i]]][Mydata[[variable3a[i]]]=="万公斤"]="万元"
  Mydata[[variable3a[i]]][Mydata[[variable3a[i]]]=="（1980不变价）万元"]="万元（1980年不变价）"
  Mydata[[variable3a[i]]][Mydata[[variable3a[i]]]=="万元（1980不变价）"]="万元（1980年不变价）"
}

for (i in 1:length(variable3)){
  county3 = unique(Mydata[[variable3a[i]]][is.na(Mydata[[variable3[i]]])==FALSE & is.na(Mydata[[variable3a[i]]])==FALSE]) 
  
  for (j in 1:length(county3)){
    Mydata[which(Mydata[[variable3a[i]]]==county3[j]), variable3[i]] =
      Mydata[[variable3[i]]][which(Mydata[[variable3a[i]]]==county3[j])] * as.numeric(as.vector(t(c[c$产值单位==county3[j],2])))
    Mydata[[variable3a[i]]][Mydata[[variable3a[i]]]==county3[j]]="万元"
  }
}

##### 统一占比单位 #####

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
      Mydata[[variable4[i]]][which(Mydata[[variable4a[i]]]==county3[j])] * as.numeric(as.vector(t(d[d$占比单位==county3[j],2])))
    Mydata[[variable4a[i]]][Mydata[[variable4a[i]]]==county3[j]]="%"
  }
}

##### 统一动力单位 #####

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
      Mydata[[variable5[i]]][which(Mydata[[variable5a[i]]]==county3[j])] * as.numeric(as.vector(t(e[e$动力单位==county3[j],2])))
    Mydata[[variable5a[i]]][Mydata[[variable5a[i]]]==county3[j]]="千瓦"
  }
}

##### 统一人单位 #####

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
      Mydata[[variable6[i]]][which(Mydata[[variable6a[i]]]==county3[j])] * as.numeric(as.vector(t(f[f$人单位==county3[j],2])))
    Mydata[[variable6a[i]]][Mydata[[variable6a[i]]]==county3[j]]="人"
  }
}

##### 统一人均面积单位 #####

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
      Mydata[[variable7[i]]][which(Mydata[[variable7a[i]]]==county3[j])] * as.numeric(as.vector(t(g[g$人均面积单位==county3[j],2])))
    Mydata[[variable7a[i]]][Mydata[[variable7a[i]]]==county3[j]]="公顷"
  }
}

##### 统一台数单位 #####

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
      Mydata[[variable8[i]]][which(Mydata[[variable8a[i]]]==county3[j])] * as.numeric(as.vector(t(h[h$台数单位==county3[j],2])))
    Mydata[[variable8a[i]]][Mydata[[variable8a[i]]]==county3[j]]="台"
  }
}

##### 统一头数单位 #####

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
      Mydata[[variable9[i]]][which(Mydata[[variable9a[i]]]==county3[j])] * as.numeric(as.vector(t(k[k$头数单位==county3[j],2])))
    Mydata[[variable9a[i]]][Mydata[[variable9a[i]]]==county3[j]]="头"
  }
}

##### 输出 #####
write_xlsx(Mydata,path ="Agriculture Data Completed.xlsx")


