library(data.table)
library(plotly)
library(ggplot2)
library(categorize)
library(networkD3)
library(extracat)
require(rCharts)
require(plyr)

#set direction
getwd()
setwd("~/Desktop/ADS/Fall2016-proj1-grp10/")

#import data
## fac means: 1 2 3 oridinal variabal

dis <- fread('data/data_dis.csv') #disability, category
mil <- fread('data/data_MIL.csv') #military, category
rac <- fread('data/data_RAC1P.csv') #race, category
schl <- fread('data/data_SCHL.csv') #education, category
vps <- fread('data/data_vps.csv') #veteran, category

wagp <- fread('data/data_WAGP.csv') #wage, numeric, int
wagp$fac <- as.factor(wagp$fac)
#wagp$WAGP <- categorize(wagp$WAGP, breaks = 4, quantile = TRUE, labels = NULL, ...)

##
data <- fread('data/data_all_var.csv')
data$fac <- as.factor(data$fac)
colnames(data)

newdata <- data[which(data$WAGP > 0),]

#plot
##wages: 
# 1.box-plots, problems: 0 wages?
plot_ly(ggplot2::diamonds, y = newdata$WAGP  , color = newdata$fac , type = "box")

# 2.scatter plot
plot_ly(data = newdata, x = wagp$WAGP , y = wagp$fac , mode = "markers", color = wagp$fac)

# 3.ggscatter, so bad
(d <- ggplot(wagp, aes(wagp$WAGP, wagp$fac)) + geom_point(aes(colour = wagp$fac)))


##others sankey
#scpcp(newdata)

#subset
subdata = newdata[sample(1:dim(newdata)[1], as.integer(dim(newdata)[1]/10), replace=FALSE),]

scpcp(subdata)
parcoords(subdata, brushMode = "1d-axes", reorderable = TRUE) # 2d-strums are really neat


##sankey-plot
sankeyPlot2$setLib('http://timelyportfolio.github.io/rCharts_d3_sankey/')
sankeyPlot2$set(
  data = wagp[c(1,100),],
  nodeWidth = 15,
  nodePadding = 10,
  layout = 32,
  width = 960,
  height = 500
)

sankeyPlot2