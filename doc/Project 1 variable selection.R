library(data.table)
data.14pusa<-fread("ss14pusa.csv")
class(data.14pusa)
head(data.14pusa)
##attach(data.14pusa)
data.test1<-data.14pusa[1:500000,]
attach(data.test1)
marr.test<-data.test1[!(is.na(MARHT[1:50000])|is.na(WAGP[1:50000])), ]$MARHT
income.test<-data.test1[!(is.na(MARHT[1:50000])|is.na(WAGP[1:50000])), ]$WAGP

dataset <- cbind(marr.test,income.test)
marr_1 <- dataset[dataset[,1] == 1, ]
marr_2 <- dataset[dataset[,1] == 2, ]
marr_3 <- dataset[dataset[,1] == 3, ]
boxplot(marr_1[,2],marr_2[,2],marr_3[,2])
###############关于edu的，不可行###################

#edu.test<-data.test1[!(is.na(MARHT[1:500000])|is.na(SCHL[1:500000])), ]$SCHL
#marr.test.2<-data.test1[!(is.na(MARHT[1:500000])|is.na(SCHL[1:500000])), ]$MARHT

#dataset.1 <- cbind(edu.test,marr.test.2)
#marr_1 <- dataset.1[dataset.1[,1] == 1, ]
#marr_2 <- dataset.1[dataset.1[,1] == 16, ]
#marr_3 <- dataset.1[dataset.1[,1] == 21, ]
#marr_4 <- dataset.1[dataset.1[,1] == 24, ]
#boxplot(marr_1[,2],marr_2[,2],marr_3[,2],marr_4[,2])
#dataset.1[,1]
###############以上部分不用###############
edu.test<-data.test1[!(is.na(data.test1$MARHT[1:500000])|is.na(data.test1$SCHL[1:500000])), ]$SCHL
marr.test.2<-data.test1[!(is.na(data.test1$MARHT[1:500000])|is.na(data.test1$SCHL[1:500000])), ]$MARHT
dataset.2 <- cbind(edu.test,marr.test.2)
marr_1.edu<-dataset.2[dataset.2[,2]==1,]
marr_2.edu<-dataset.2[dataset.2[,2]==2,]
marr_3.edu<-dataset.2[dataset.2[,2]==3,]

nrow(marr_1.edu)
length(which(marr_1.edu[,1] == 1))
length(which(marr_1.edu[,1] == 16))
length(which(marr_1.edu[,1] == 21))
length(which(marr_1.edu[,1] == 24))

nrow(marr_2.edu)
length(which(marr_2.edu[,1] == 1))
length(which(marr_2.edu[,1] == 16))
length(which(marr_2.edu[,1] == 21))
length(which(marr_2.edu[,1] == 24))

nrow(marr_3.edu)
length(which(marr_3.edu[,1] == 1))
length(which(marr_3.edu[,1] == 16))
length(which(marr_3.edu[,1] == 21))
length(which(marr_3.edu[,1] == 24))

###############Race
race.test<-data.test1[!(is.na(data.test1$MARHT[1:500000])), ]$RAC1P
#race.test<-data.test1[!(is.na(data.test1$MARHT[1:500000])|is.na(data.test1$RAC1P[1:500000])), ]$RAC1P
marr.test.3<-data.test1[!(is.na(data.test1$MARHT[1:500000])|is.na(data.test1$RAC1P[1:500000])), ]$MARHT
dataset.3 <- cbind(race.test,marr.test.3)

marr_1.race<-dataset.3[dataset.3[,2]==1,]
marr_2.race<-dataset.3[dataset.3[,2]==2,]
marr_3.race<-dataset.3[dataset.3[,2]==3,]

nrow(marr_1.race)
length(which(marr_1.race[,1] == 1))
length(which(marr_1.race[,1] == 2))
length(which(marr_1.race[,1] == 3))
length(which(marr_1.race[,1] == 6))

nrow(marr_2.race)
length(which(marr_2.race[,1] == 1))
length(which(marr_2.race[,1] == 2))
length(which(marr_2.race[,1] == 3))
length(which(marr_2.race[,1] == 6))

nrow(marr_3.race)
length(which(marr_3.race[,1] == 1))
length(which(marr_3.race[,1] == 2))
length(which(marr_3.race[,1] == 3))
length(which(marr_3.race[,1] == 6))
######################交通工具
car.test<-data.test1[!(is.na(data.test1$MARHT[1:500000])|is.na(data.test1$JWRIP[1:500000])), ]$JWRIP
marr.test.4<-data.test1[!(is.na(data.test1$MARHT[1:500000])|is.na(data.test1$JWRIP[1:500000])), ]$MARHT
dataset.4 <- cbind(car.test,marr.test.4)
marr_1.car<-dataset.4[dataset.4[,2]==1,]
marr_2.car<-dataset.4[dataset.4[,2]==2,]
marr_3.car<-dataset.4[dataset.4[,2]==3,]

nrow(marr_1.car)
length(which(marr_1.car[,1] == 1))
length(which(marr_1.car[,1] == 2))
length(which(marr_1.car[,1] == 4))
length(which(marr_1.car[,1] == 6))
length(which(marr_1.car[,1] == 7))
length(which(marr_1.car[,1] == 8))


nrow(marr_2.car)
length(which(marr_2.car[,1] == 1))
length(which(marr_2.car[,1] == 2))
length(which(marr_2.car[,1] == 4))
length(which(marr_2.car[,1] == 6))
length(which(marr_2.car[,1] == 7))
length(which(marr_2.car[,1] == 8))

nrow(marr_3.car)
length(which(marr_3.car[,1] == 1))
length(which(marr_3.car[,1] == 2))
length(which(marr_3.car[,1] == 4))
length(which(marr_3.car[,1] == 6))
length(which(marr_3.car[,1] == 7))
length(which(marr_3.car[,1] == 8))
####################disable
dis.test<-data.test1[!(is.na(data.test1$MARHT[1:500000])|is.na(data.test1$DIS[1:500000])), ]$DIS
marr.test.5<-data.test1[!(is.na(data.test1$MARHT[1:500000])|is.na(data.test1$DIS[1:500000])), ]$MARHT
dataset.5 <- cbind(dis.test,marr.test.5)
marr_1.dis<-dataset.5[dataset.5[,2]==1,]
marr_2.dis<-dataset.5[dataset.5[,2]==2,]
marr_3.dis<-dataset.5[dataset.5[,2]==3,]

nrow(marr_1.dis)
length(which(marr_1.dis[,1] == 1))
length(which(marr_1.dis[,1] == 2))

nrow(marr_2.dis)
length(which(marr_2.dis[,1] == 1))
length(which(marr_2.dis[,1] == 2))

nrow(marr_3.dis)
length(which(marr_3.dis[,1] == 1))
length(which(marr_3.dis[,1] == 2))
################work time
data.test1<-data.14pusa[1:500000,]
attach(data.test1)
marr.test.6<-data.test1[!(is.na(MARHT[1:500000])|is.na(WKHP[1:500000])), ]$MARHT
worktime.test<-data.test1[!(is.na(MARHT[1:500000])|is.na(WKHP[1:500000])), ]$WKHP

dataset.6 <- cbind(marr.test.6,worktime.test)
marr_1.wt <- dataset.6[dataset.6[,1] == 1, ]
marr_2.wt <- dataset.6[dataset.6[,1] == 2, ]
marr_3.wt <- dataset.6[dataset.6[,1] == 3, ]
boxplot(marr_1.wt[,2],marr_2.wt[,2],marr_3.wt[,2])
