library(data.table)
#set direction
getwd()
setwd("/Users/sun93/Documents/ADS/Fall2016-proj1-grp10/")

#load disability file
allvar <- fread('updated_data_all_var.csv') 
allvar <- data.frame(allvar)

#clustering based on wage
allvar$WAGP <- as.array(kmeans(allvar$WAGP, 10, nstart = 20)$cluster)
#head(allvar)

# 1.DIC 2. RAC1P  3.SCHL 4.WAGE 5.MIL 6.PWGTP 7.fac

#######
#1.wage -> dis
f.dis <- function(col1){ 
  a <- sort(unique(allvar[,col1]))
  b <- sort(unique(allvar$DIS))
  results <- matrix(NA, nrow = length(a), ncol = length(b) )
  cou = 1
  for (i in 1:length(a)){
    flow <- count(allvar[which(allvar[,col1]==a[i]),],DIS, wt = PWGTP)
    colnames(results) <- sort(b)
    rownames(results) <- sort(a)
    results[cou,] <-flow$n
    cou <- cou + 1
  }
  return(results)
}

# Wage to fac
f.dis(4)


#######
#2.dis -> edu
unique(allvar$DIS)
f.edu <- function(col1){ 
  a <- sort(unique(allvar[,col1]))
  b <- sort(unique(allvar$SCHL))
  results <- matrix(NA, nrow = length(a), ncol = length(b) )
  cou = 1
  for (i in 1:length(a)){
    flow <- count(allvar[which(allvar[,col1]==a[i]),],SCHL, wt = PWGTP)
    colnames(results) <- sort(b)
    rownames(results) <- sort(a)
    results[cou,] <-flow$n
    cou <- cou + 1
  }
  return(results)
}

# Dic to edu
f.edu(1)


#######
#3. edu -> race
unique(allvar$SCHL)
unique(allvar$RAC1P)
f.rac <- function(col1){ 
  a <- sort(unique(allvar[,col1]))
  b <- sort(unique(allvar$RAC1P))
  results <- matrix(NA, nrow = length(a), ncol = length(b) )
  cou = 1
  for (i in 1:length(a)){
    flow <- count(allvar[which(allvar[,col1]==a[i]),],RAC1P, wt = PWGTP)
    colnames(results) <- sort(b)
    rownames(results) <- sort(a)
    results[cou,] <-flow$n
    cou <- cou + 1
  }
  return(results)
}

# Edu to race
f.rac(3)


#######
#4. race -> millitary
unique(allvar$RAC1P)
unique(allvar$MIL)
f.mil <- function(col1){ 
  a <- sort(unique(allvar[,col1]))
  b <- sort(unique(allvar$MIL))
  results <- matrix(NA, nrow = length(a), ncol = length(b) )
  cou = 1
  for (i in 1:length(a)){
    flow <- count(allvar[which(allvar[,col1]==a[i]),],MIL, wt = PWGTP)
    colnames(results) <- sort(b)
    rownames(results) <- sort(a)
    results[cou,] <-flow$n
    cou <- cou + 1
  }
  return(results)
}

# race to millitary
f.mil(2)



########################################
# each variable numbers flow to fac
f <- function(col1){ #col1 ->col2
  a <- sort(unique(allvar[,col1]))
  b <- sort(unique(allvar$fac))
  results <- matrix(NA, nrow = length(a), ncol = length(b) )
  cou = 1
  for (i in 1:length(a)){
    #flow <- count(allvar[which(allvar[,col1]==i),],colnames(allvar)[col2], wt = PWGTP)
    flow <- count(allvar[which(allvar[,col1]==a[i]),],fac, wt = PWGTP)
    colnames(results) <- sort(b)
    rownames(results) <- sort(a)
    results[cou,] <-flow$n
    cou <- cou + 1
  }
  return(results)
}

f(1) # DIC to fac

f(2) # RACE to fac

f(3) # SCHL to fac

f(4) # WAGP to fac

f(5) # MIL to fac














