library(data.table)

data1 <- fread('ss14pusa.csv')
data2 <- fread('ss14pusb.csv')
data_ori <- rbind(data1, data2)
as.data.table(data_ori)

databad <- data_ori[(MAR == 3 | MARHT >= 2)]
datagood <- data_ori[(MAR != 3 & MARHT < 2)]
#Define a good marriage attitude
datagood[, fac := 1, ]
databad1 <- databad[!(MARHT >= 3 | (MARHT == 2 & MAR == 3))]
#Define a poor marriage attitude
databad1[, fac := 2, ]
databad2 <- databad[MARHT >= 3 | (MARHT == 2 & MAR == 3)]
#Define a inferior marriage attitude
databad2[, fac := 3, ]
data_f <- rbind(datagood, databad1, databad2)

#DIS (Disability recode)
# 1 .With a disability
# 2 .Without a disability
data_final <- data_f
data_dis <- data_final[!is.na(DIS), list(DIS, fac)]
write.table(data_dis, 'data_dis.csv',sep = ',', row.names = F)

#RAC1P (Recoded detailed race code)
# 1 .White alone
# 2 .Black or African American alone
# 3 .American Indian alone
# 6 .Asian alone
data_final <- data_f
data_RAC1P <- data_final[!is.na(RAC1P), list(RAC1P, fac)]
write.table(data_RAC1P, 'data_RAC1P.csv', sep = ',', row.names = F)

#SCHL (Educational attainment)
data_final <- data_f
data_SCHL <- data_final[!is.na(SCHL), list(SCHL, fac)]
write.table(data_SCHL, 'data_SCHL.csv', sep = ',', row.names = F)

#WAGP (Wages or salary income past 12 months)
data_final <- data_f
data_WAGP <- data_final[!is.na(WAGP), list(WAGP, fac)]
write.table(data_WAGP, 'data_WAGP.csv', sep = ',', row.names = F)

#VPS (Veteran period of service)
data_final <- data_f
data_VPS <- data_final[!is.na(VPS), list(VPS, fac)]
write.table(data_VPS, 'data_VPS.csv', sep = ',', row.names = F)




