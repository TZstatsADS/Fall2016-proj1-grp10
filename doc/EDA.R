library(data.table)
library(vcd)
#set direction
getwd()
setwd("/Users/sun93/Documents/ADS/pro1/")

#load disability file
dis <- fread('data_dis.csv') 
#load military file
mil <- fread('data_MIL.csv')
#load race file
rac <- fread('data_RAC1P.csv')
#load education file
schl <- fread('data_SCHL.csv') 
#load veteran file
vps <- fread('data_vps.csv') 

# mosaic plot based on disability
dis <- as.data.frame(dis)
disibility <- factor(dis[,1],labels=c("with dis","without dis"))
marriage.status <- factor(dis[,2],labels = c("good","not good","bad"))

mosaic(~marriage.status+disibility,data = dis,shade=TRUE,labeling=labeling_values,
       main = "Dependency Plot about Disibility and Marraige Status", 
       sub = "Red: less likely   Blue: more likely")


# mosaic plot based on military
mil <- as.data.frame(mil)
military <- factor(mil[,1],labels=c("Now","Past","only","Never"))
marriage.status <- factor(mil[,2],labels = c("good","not good","bad"))

mosaic(~marriage.status+military,data = mil,shade=TRUE,labeling=labeling_values,
       main = "Dependency Plot about Military and Marraige Status", 
       sub = "Red: less likely   Blue: more likely")



# mosaic plot based on race
rac <- as.data.frame(rac)
race <- factor(rac[,1],labels=c("White","Black","Indian","Asian"))
marriage.status <- factor(rac[,2],labels = c("good","not good","bad"))

mosaic(~marriage.status+race,data = rac,shade=TRUE,labeling=labeling_values,
       main = "Dependency Plot about Race and Marraige Status", 
       sub = "Red: less likely   Blue: more likely")


# mosaic plot based on race
schl <- as.data.frame(schl)
education <- factor(schl[,1],labels=c("No","High schl","Bachelor","Master","Doctorate"))
marriage.status <- factor(schl[,2],labels = c("good","not good","bad"))

mosaic(~marriage.status+education,data = schl,shade=TRUE,labeling=labeling_values,
       main = "Dependency Plot about Education and Marraige Status", 
       sub = "Red: less likely   Blue: more likely")


# mosaic plot based on veteran
vps <- as.data.frame(vps)
veteran <- factor(vps[,1],labels=c("Gulf War","Vietnam Era","WWII"))
marriage.status <- factor(vps[,2],labels = c("good","not good","bad"))

mosaic(~marriage.status+veteran,data = vps,shade=TRUE,labeling=labeling_values,
       main = "Dependency Plot about Veteran and Marraige Status", 
       sub = "Red: less likely   Blue: more likely")




