library(data.table)
library(vcd)
library(dplyr)
#set direction
getwd()
setwd("/Users/sun93/Documents/ADS/Fall2016-proj1-grp10/")

#load disability file
dis <- fread('data/data_dis.csv') 
#load military file
mil <- fread('data/data_MIL.csv')
#load race file
rac <- fread('data/data_RAC1P.csv')
#load education file
schl <- fread('data/data_SCHL.csv') 
#load veteran file
vps <- fread('data/data_vps.csv') 

#############################################################
# mosaic plot based on disability
dis <- as.data.frame(dis)
#head(dis)
# add weight to count
dis.num<- count(dis, DIS, wt = PWGTP)
dis1fac.num <- count(dis[which(dis$DIS==1),], fac, wt = PWGTP)
dis2fac.num <- count(dis[which(dis$DIS==2),], fac, wt = PWGTP)
#subdata = data[sample(1:dim(data)[1], as.integer(dim(data)[1]/10), replace=FALSE),]

#set up dataframe
disibility <- rep(1:2,c(dis.num$n))
marriage.status <- c(rep(1:3,c(dis1fac.num$n)),rep(1:3,c(dis2fac.num$n)))
#disibility <- factor(rep(1:2,c(dis.num$n)),labels=c("with dis","without dis"))
#marriage.status <- factor(c(rep(1:3,c(dis1fac.num$n)),rep(1:3,c(dis2fac.num$n))),labels = c("good","not good","bad"))
dis.plot <- data.frame(disibility,marriage.status)
#get 10% sample
subdis.plot <- dis.plot[sample(1:dim(dis.plot)[1], as.integer(dim(dis.plot)[1]/10), replace=FALSE),]
disibility <- factor(subdis.plot[,1],labels=c("with dis","without dis"))
marriage.status <- factor(subdis.plot[,2], labels = c("good","not good","bad"))
subdis.plot[,1] <- disibility
subdis.plot[,2] <- marriage.status
#str(subdis.plot)
#mosic plot
mosaic(~marriage.status+disibility,data = subdis.plot,shade=TRUE,labeling=labeling_values,
       main = "Dependency Plot about Disibility and Marraige Status", 
       sub = "Red: less likely   Blue: more likely")

#disibility <- factor(dis[,1],labels=c("with dis","without dis"))
#marriage.status <- factor(dis[,3],labels = c("good","not good","bad"))


#############################################################
# mosaic plot based on military
mil <- as.data.frame(mil)
#head(mil)

# add weight to count
mil.num<- count(mil, MIL, wt = PWGTP)
mil1fac.num <- count(mil[which(mil$MIL==1),], fac, wt = PWGTP)
mil2fac.num <- count(mil[which(mil$MIL==2),], fac, wt = PWGTP)
mil3fac.num <- count(mil[which(mil$MIL==3),], fac, wt = PWGTP)
mil4fac.num <- count(mil[which(mil$MIL==4),], fac, wt = PWGTP)
#set up dataframe
military <- rep(1:4,c(mil.num$n))
marriage.status <- c(rep(1:3,c(mil1fac.num$n)),rep(1:3,c(mil2fac.num$n)),rep(1:3,c(mil3fac.num$n)),rep(1:3,c(mil4fac.num$n)))
mil.plot <- data.frame(military,marriage.status)
#get 10% sample
submil.plot <- mil.plot[sample(1:dim(mil.plot)[1], as.integer(dim(mil.plot)[1]/10), replace=FALSE),]
military <- factor(submil.plot$military, labels=c("Now","Past","only","Never"))
marriage.status <- factor(submil.plot$marriage.status, labels = c("good","not good","bad"))
submil.plot[,1] <- military
submil.plot[,2] <- marriage.status

#mosaic plot
mosaic(~marriage.status+military,data = submil.plot,shade=TRUE,labeling=labeling_values,
       main = "Dependency Plot about Military and Marraige Status", 
       sub = "Red: less likely   Blue: more likely")

#military <- factor(mil[,1],labels=c("Now","Past","only","Never"))
#marriage.status <- factor(mil[,3],labels = c("good","not good","bad"))

#############################################################
# mosaic plot based on race
rac <- as.data.frame(rac)
#head(rac)

# add weight to count
rac.num<- count(rac, RAC1P, wt = PWGTP)
rac1fac.num <- count(rac[which(rac$RAC1P==1),], fac, wt = PWGTP)
rac2fac.num <- count(rac[which(rac$RAC1P==2),], fac, wt = PWGTP)
rac3fac.num <- count(rac[which(rac$RAC1P==3),], fac, wt = PWGTP)
rac4fac.num <- count(rac[which(rac$RAC1P==6),], fac, wt = PWGTP)
#set up dataframe
race <- rep(1:4,c(rac.num$n))
marriage.status <- c(rep(1:3,c(rac1fac.num$n)),rep(1:3,c(rac2fac.num$n)),rep(1:3,c(rac3fac.num$n)),rep(1:3,c(rac4fac.num$n)))
rac.plot <- data.frame(race,marriage.status)
#get 10% sample
subrac.plot <- rac.plot[sample(1:dim(rac.plot)[1], as.integer(dim(rac.plot)[1]/10), replace=FALSE),]
race <- factor(subrac.plot$race,labels=c("White","Black","Indian","Asian"))
marriage.status <- factor(subrac.plot$marriage.status,labels=c("good","not good","bad"))
subrac.plot[,1] <- race
subrac.plot[,2] <- marriage.status

#mosaic plot
mosaic(~marriage.status+race,data = subrac.plot,shade=TRUE,labeling=labeling_values,
       main = "Dependency Plot about Race and Marraige Status", 
       sub = "Red: less likely   Blue: more likely")

#race <- factor(rac[,1],labels=c("White","Black","Indian","Asian"))
#marriage.status <- factor(rac[,2],labels = c("good","not good","bad"))


#############################################################
# mosaic plot based on education
schl <- as.data.frame(schl)

#head(schl)

# add weight to count
schl.num<- count(schl, SCHL, wt = PWGTP)
schl1fac.num <- count(schl[which(schl$SCHL==1),], fac, wt = PWGTP)
schl16fac.num <- count(schl[which(schl$SCHL==16),], fac, wt = PWGTP)
schl21fac.num <- count(schl[which(schl$SCHL==21),], fac, wt = PWGTP)
schl22fac.num <- count(schl[which(schl$SCHL==22),], fac, wt = PWGTP)
schl24fac.num <- count(schl[which(schl$SCHL==24),], fac, wt = PWGTP)
#set up dataframe
education <- rep(1:5,c(schl.num$n))
marriage.status <- c(rep(1:3,c(schl1fac.num$n)),rep(1:3,c(schl16fac.num$n)),rep(1:3,c(schl21fac.num$n)),rep(1:3,c(schl22fac.num$n)),rep(1:3,c(schl24fac.num$n)))
schl.plot <- data.frame(education,marriage.status)

#get 10% sample
subschl.plot <- schl.plot[sample(1:dim(schl.plot)[1], as.integer(dim(schl.plot)[1]/10), replace=FALSE),]
education <- factor(subschl.plot$education,labels=c("No","High schl","Bachelor","Master","Doctorate"))
marriage.status <- factor(subschl.plot$marriage.status,labels=c("good","not good","bad"))
subschl.plot[,1] <- education
subschl.plot[,2] <- marriage.status

mosaic(~marriage.status+education, data = subschl.plot,shade=TRUE,labeling=labeling_values,
       main = "Dependency Plot about Education and Marraige Status", 
       sub = "Red: less likely   Blue: more likely")


#education <- factor(schl[,1],labels=c("No","High schl","Bachelor","Master","Doctorate"))
#marriage.status <- factor(schl[,2],labels = c("good","not good","bad"))


#############################################################
# mosaic plot based on veteran
vps <- as.data.frame(vps)
#head(vps)

# add weight to count
vps.num<- count(vps, VPS, wt = PWGTP)
vps1fac.num <- count(vps[which(vps$VPS==1),], fac, wt = PWGTP)
vps6fac.num <- count(vps[which(vps$VPS==6),], fac, wt = PWGTP)
vps11fac.num <- count(vps[which(vps$VPS==11),], fac, wt = PWGTP)

#set up dataframe
veteran <- rep(1:3,c(vps.num$n))
marriage.status <- c(rep(1:3,c(vps1fac.num$n)),rep(1:3,c(vps6fac.num$n)),rep(1:3,c(vps11fac.num$n)))
vps.plot <- data.frame(veteran,marriage.status)
#get 10% sample
subvps.plot <- vps.plot[sample(1:dim(vps.plot)[1], as.integer(dim(vps.plot)[1]/10), replace=FALSE),]
veteran <- factor(subvps.plot$veteran, labels=c("Gulf War","Vietnam Era","WWII"))
marriage.status <- factor(subvps.plot$marriage.status,labels = c("good","not good","bad"))
subvps.plot[,1] <- veteran
subvps.plot[,2] <- marriage.status

mosaic(~marriage.status+veteran,data = subvps.plot,shade=TRUE,labeling=labeling_values,
       main = "Dependency Plot about Veteran and Marraige Status", 
       sub = "Red: less likely   Blue: more likely")


#veteran <- factor(vps[,1],labels=c("Gulf War","Vietnam Era","WWII"))
#marriage.status <- factor(vps[,2],labels = c("good","not good","bad"))






