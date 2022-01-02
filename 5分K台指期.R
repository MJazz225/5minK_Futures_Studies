

## 2020/08/30
## 研究台指期的5分K用RSI回測績效

rm(list=ls())
setwd("C:\\Users\\User\\Documents\\TXF_5minK")
if(!require(data.table))install.packages("data.table")
if(!require(dplyr))install.packages("dpylr")
library(data.table)
library(xts)
library(zoo)
library(stargazer)
library(dplyr)

rawdata2020 <- read.csv("TXF20200101~20200630.csv", sep = ",", stringsAsFactors = FALSE) ##資料是1分K
rawdata2020 <- rawdata2020[,c(1,2,6,7)]
MDATE <- data.frame(MDATE = as.Date(rawdata2020[,1]))
colnames(MDATE) <- "MDATE"
MTIME <- as.data.frame(as.vector(rawdata2020[,2]))
colnames(MTIME) <- "MTIME"

data2020_1min <- rawdata2020
data2020_1min <- cbind(MDATE, MTIME, data2020_1min[,c(3,4)])
colnames(data2020_1min) <- c("DATE", "TIME", "CLOSE", "VOLUME")

##########################################################    製作5分資料   ##########################################################


data2020_5min <- NULL
for (i in 1:(nrow(rawdata2020)/5)) { ##1分K轉成5分K
  
  cat(i , "/", nrow(rawdata2020)/5, "\n")
  
  tem_TIME <- as.vector(MTIME[i*5,])
  
  tem_DATE <- MDATE[i*5,]

  tem_close <- rawdata2020[(i*5),3]
  
  tem_volume <- rawdata2020[c((i*5+1-5):(i*5)),4]

  tem2 <- cbind.data.frame(tem_DATE, tem_TIME, tem_close, sum(tem_volume))
  
  data2020_5min <- rbind.data.frame(data2020_5min, tem2)
}
colnames(data2020_5min) <- c("DATE", "TIME", "CLOSE", "VOLUME")
rm(rawdata2020)


CLOSE_CHG <- diff(data2020_5min[,3]) ##點數的差
upchange <- ifelse(CLOSE_CHG>0, CLOSE_CHG, 0)
downchange <- ifelse(CLOSE_CHG<0, abs(CLOSE_CHG), 0)
upchange <- cbind(c(NA, upchange))
downchange <- cbind(c(NA, downchange))
colnames(upchange) <- "UPCHANGE"
colnames(downchange) <- "DOWNCHANGE"


data2020_5min <- cbind(data2020_5min, upchange, downchange)
data2020_5min <- na.omit(data2020_5min)

##上漲力度下跌力度
SMUP <- rep(0, nrow(data2020_5min))
SMDOWN <- rep(0, nrow(data2020_5min))

##  n期的漲跌力度
for (i in 6:nrow(data2020_5min)) { ##設定6是看6期的上漲下跌力度
  
  SMUP[i] <- mean(upchange[(i-5):i], na.rm=TRUE)
  
  SMDOWN[i] <- mean(downchange[(i-5):i], na.rm=TRUE)
  
}

data_5min <- cbind(data2020_5min, SMUP, SMDOWN)
data_5min <- na.omit(data_5min)

##計算n期的RSI的值
rsi_N <- 100*SMUP/(SMUP+SMDOWN) ##RSI公式
rsi_N <- data.frame(rsi_N)
names(rsi_N) <- "RSI"

data_5min <- cbind(data_5min, rsi_N)
data_5min <- data_5min[6:nrow(data_5min),]
data_5min$CLOSE <- as.xts(data_5min$CLOSE)

data1_5min <- data_5min[,c(1,2,3,7,8,9)]

plot.zoo(data1_5min[,c(3,4,5,6)],
         col = c("red", "blue", "yellow", "green"),
         main = "RSI indicator")














