require(plyr)
require(reshape)
library(ggplot2)
library(zoo)
library(lubridate)
library(scales)
library(forecast)
library(fpp2)

#TODO: set data, submit and r paths to correct directories, if needed
paths = list(data='./DATA/',    # ./ is relative path
             submit='./Submissions/',
             r='../R/')

# Loads the training data with correct classes
#cls <- c('factor', 'factor','Date', 'numeric', 'logical')
#train <- read.csv(paste0(paths$data, 'Cliftrainclean.csv'), 
#                    colClasses=cls)
# Store 67 to 72 must be deleted. 

train <- read.csv(paste0(paths$data, "Cliftrainclean.csv"),header = TRUE)
train$Date<-as.Date(as.character(train$Date),format="%m/%d/%Y")
train$Store<-as.factor(train$Store)
train$Dept<-as.factor(train$Dept)


train.dates <- unique(train$Date)
num.train.dates <- length(train.dates)
all.stores <- unique(train$Store)
num.stores <- length(all.stores)
train.frame <- data.frame(Date=rep(train.dates, num.stores),
                          Store=rep(all.stores, each=num.train.dates))

tr.1 <- join(train.frame,
             train[train$Dept==1, c('Store','Date','Weekly_Sales')])
tr.1 <- cast(tr.1, Date ~ Store) 
tr.date<-tr.1[,1]


tr.sales<-tr.1[,2:ncol(tr.1)]
colnames(tr.sales)<-paste("Store",colnames(tr.sales),sep="")
head(tr.sales)  #dim(tr.sales)=129*84

tr.x<-cbind(tr.date,tr.sales)  
colnames(tr.x)[1] <- "Date"
head(tr.x[,1:6])


png(file='ALBSales.png', width=700, height=320)

ALB.df= data.frame(Dates=tr.date, ALB_ACME=tr.x$Store2, ALB_Denver=tr.x$Store3
                   ,ALB_Eastern=tr.x$Store4,ALB_Houston=tr.x$Store5,ALB_Intermountain=tr.x$Store6
                   ,ALB_Jewel=tr.x$Store7,ALB_Portland=tr.x$Store9
                   ,ALB_Seattle=tr.x$Store10,ALB_Shaws=tr.x$Store11
                   ,ALB_Southern=tr.x$Store13,ALB_Southwest=tr.x$Store14)

ggplot(data = ALB.df, aes(x=Dates, y=value, color=variable )  )             +
  ylab('Weekly Sales of ALB supermartkets')                                 +
  geom_line(aes(y=ALB_ACME , col='ALB_ACME'),  size=1, alpha=.5)  +
  geom_line(aes(y=ALB_Denver, col='ALB_Denver'), size=1, alpha=.5)    +
  geom_line(aes(y=ALB_Eastern , col='ALB_Eastern'),  size=1, alpha=.5)  +
  geom_line(aes(y=ALB_Houston, col='ALB_Houston'), size=1, alpha=.5)    +
  geom_line(aes(y=ALB_Intermountain , col='ALB_Intermountian'),  size=1, alpha=.5)  +
  geom_line(aes(y=ALB_Jewel, col='ALB_Jewel'), size=1, alpha=.5)    +
  geom_line(aes(y=ALB_Portland, col='ALB_Portland'), size=1, alpha=.5)    +
  geom_line(aes(y=ALB_Seattle, col='ALB_Seattle'), size=1, alpha=.5)    +
  geom_line(aes(y=ALB_Shaws, col='ALB_Shaws'),  size=1, alpha=.5)  +
  geom_line(aes(y=ALB_Southern, col='ALB_Southern'),  size=1, alpha=.5)  +
  geom_line(aes(y=ALB_Southwest, col='ALB_Southwest'), size=1, alpha=.5)    +
  theme(legend.position=c(.1,.85))			  

dev.off()		   

png(file='ALBNorthSales.png', width=700, height=320)

ALBN.df= data.frame(Dates=tr.date, ALB_Portland=tr.x$Store9
                    ,ALB_Seattle=tr.x$Store10)
ALBN.df$Month <- as.Date(cut(ALBN.df$Dates,
                             breaks = "month"))

ggplot(data = ALBN.df, aes(x=Dates, y=value, color=variable )  )             +
  ylab('Weekly Sales of North ALB supermartkets')                                 +
  geom_line(aes(y=ALB_Portland, col='ALB_Portland'), size=1, alpha=.5)    +
  geom_line(aes(y=ALB_Seattle, col='ALB_Seattle'), size=1, alpha=.5)    +
  scale_x_date(labels = date_format("%Y-%m"), breaks = '2 months') +
  theme(legend.position=c(.1,.85))			  

dev.off()		   


png(file='ALBEastSouthSales.png', width=700, height=320)

ALBS.df= data.frame(Dates=tr.date ,ALB_Eastern=tr.x$Store4,ALB_Houston=tr.x$Store5
                    ,ALB_Southern=tr.x$Store13,ALB_Southwest=tr.x$Store14)

ggplot(data = ALBS.df, aes(x=Dates, y=value, color=variable )  )             +
  ylab('Weekly Sales of Eastern and Southern ALB supermartkets')                                 +
  geom_line(aes(y=ALB_Eastern, col='ALB_Eastern'), size=1, alpha=.5)    +
  geom_line(aes(y=ALB_Houston, col='ALB_Houston'), size=1, alpha=.5)    +
  geom_line(aes(y=ALB_Southern, col='ALB_Southern'), size=1, alpha=.5)    +
  geom_line(aes(y=ALB_Southwest, col='ALB_Southwest'), size=1, alpha=.5)    +
  scale_x_date(labels = date_format("%Y-%m"), breaks = '2 months') +
  theme(legend.position=c(.1,.85))			  

dev.off()		   


png(file='WalmartNHMSales.png', width=700, height=320)

Walmart.df= data.frame(Dates=tr.date, WM_NHM_Atlantic=tr.x$Store78, WM_NHM_NorthCentral=tr.x$Store79
                       ,WM_NHM_Northeast=tr.x$Store80,WM_NHM_SouthCentral=tr.x$Store81
                       ,WM_NHM_Southeast=tr.x$Store82,WM_NHM_West=tr.x$Store83)

ggplot(data = Walmart.df, aes(x=Dates, y=value, color=variable))             +
  ylab('Weekly Sales of Walmart Neighborhood Markets')                      +
  geom_line(aes(y=WM_NHM_Atlantic , col='WM_NHM_Atlantic'),  size=1, alpha=.5)  +
  geom_line(aes(y=WM_NHM_NorthCentral, col='WM_NHM_NorthCentral'), size=1, alpha=.5)    +
  geom_line(aes(y=WM_NHM_Northeast , col='WM_NHM_Northeast'),  size=1, alpha=.5)  +
  geom_line(aes(y=WM_NHM_SouthCentral, col='WM_NHM_SouthCentral'), size=1, alpha=.5)    +
  geom_line(aes(y=WM_NHM_Southeast , col='WM_NHM_Southeast'),  size=1, alpha=.5)  +
  geom_line(aes(y=WM_NHM_West, col='WM_NHM_West'), size=1, alpha=.5)    +
  scale_x_date(labels = date_format("%Y-%m"), breaks = '2 months') +
  theme(legend.position=c(.1,.85))			  

dev.off()		   


png(file='KrogerSales.png', width=700, height=320)

Kroger.df= data.frame(Dates=tr.date, Kroger_Atlanta=tr.x$Store33, Kroger_Central=tr.x$Store34
                      ,Kroger_Cincinnati=tr.x$Store35,Kroger_Columbus=tr.x$Store36
                      ,Kroger_Dallas=tr.x$Store38,WM_NHM_West=tr.x$Store83)

ggplot(data = Walmart.df, aes(x=Dates, y=value, color=variable))             +
  ylab('Weekly Sales of Walmart Neighborhood Markets')                      +
  geom_line(aes(y=WM_NHM_ATLN , col='WM_NHM_ATLN'),  size=1, alpha=.5)  +
  geom_line(aes(y=WM_NHM_NorthCentral, col='WM_NHM_NorthCentral'), size=1, alpha=.5)    +
  geom_line(aes(y=WM_NHM_Northeast , col='WM_NHM_Northeast'),  size=1, alpha=.5)  +
  geom_line(aes(y=WM_NHM_SouthCentral, col='WM_NHM_SouthCentral'), size=1, alpha=.5)    +
  geom_line(aes(y=WM_NHM_Southeast , col='WM_NHM_Southeast'),  size=1, alpha=.5)  +
  geom_line(aes(y=WM_NHM_West, col='WM_NHM_West'), size=1, alpha=.5)    +
  scale_x_date(labels = date_format("%Y-%m"), breaks = '2 months') +
  theme(legend.position=c(.1,.85))			  

dev.off()		   


### We can use ets() to detect seasonality  ###
ets.seasonality.vector<-character()
for (i in 1:ncol(tr.sales))
{ets.model1<-ets(tr.sales[,i])
 ets.method<-ets.model1$method
 ets.seasonality.vector[i]<-ets.method
}

## Give the name of seasonality.vector
names(ets.seasonality.vector)<-names(tr.sales)
ets.seasonality.vector
## which(ets.seasonality.vector=="ETS(M,Ad,N)") is store 56. 
## Store 56 is ROUNDY'S TOTAL CENSUS TRADING AREA


ets.report<-table(ets.seasonality.vector)
ets.report

sink('ETSDetectSeasonality.txt')
cat("\n")
cat("=============================\n")
cat("\n")
ets.report
cat("=============================\n")
cat("\n")
cat("\n")
sink()
