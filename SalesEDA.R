require(plyr)
require(reshape)
library(ggplot2)
library(zoo)
library(lubridate)
library(scales)
library(forecast)
#library(gridExtra)
library(fpp2)

#TODO: set data, submit and r paths to correct directories, if needed
paths = list(data='./DATA/',    # ./ is relative path
             submit='./Submissions/',
             r='../R/')

# Loads the training data with correct classes
#cls <- c('factor', 'factor','Date', 'numeric', 'logical')
#train <- read.csv(paste0(paths$data, 'Cliftrainclean.csv'), 
#                    colClasses=cls)

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
head(tr.sales)

tr.x<-cbind(tr.date,tr.sales)  
colnames(tr.x)[1] <- "Date"
head(tr.x[,1:6])

myvar<-c("Date","Store9")
ALB.Portland<-tr.x[myvar]
head(ALB.Portland)

ts.ALB.Portland<-ts(ALB.Portland$Store9,frequency = 365.25/7,start=decimal_date(ymd("2014-07-19")))
plot(ALB.Portland$Date,ts.ALB.Portland,type="l",xlab="Date",ylim=c(0,40000))

ggseasonplot(ts.ALB.Portland, year.labels=TRUE,col=rainbow(3),continuous=TRUE,labelgap = 0.15)+
  ylab("Weekly Sales")+
  ggtitle("Clif Bar Sales of Albertsons Supermarkets in Portland")


myvar.2<-c("Date","Store38")
Kroger.Dallas<-tr.x[myvar.2]
head(Kroger.Dallas)

ts.Kroger.Dallas<-ts(Kroger.Dallas$Store38,frequency = 365.25/7,start=decimal_date(ymd("2014-07-19")))
plot(Kroger.Dallas$Date,ts.Kroger.Dallas,type="l",xlab="Date",ylim=c(0,20000))

ggseasonplot(ts.Kroger.Dallas, year.labels=TRUE,col=rainbow(3),continuous=TRUE,labelgap = 0.15)+
  ylab("Weekly Sales")+
  ggtitle("Clif Bar Sales of Kroger Supermarkets in Dallas")

myvar.3<-c("Date","Store81")
WM.NHM.SouthCentral<-tr.x[myvar.3]
head(WM.NHM.SouthCentral)

ts.WM.NHM.SouthCentral<-ts(WM.NHM.SouthCentral$Store81,frequency = 365.25/7,start=decimal_date(ymd("2014-07-19")))
plot(WM.NHM.SouthCentral$Date,ts.WM.NHM.SouthCentral,type="l",xlab="Date",ylim=c(0,4500))

ggseasonplot(ts.WM.NHM.SouthCentral, year.labels=TRUE,col=rainbow(3),continuous=TRUE,labelgap = 0.15)+
  ylab("Weekly Sales")+
  ggtitle("Clif Bar Sales of Walmart Neighborhood in Southcentral Area")

## Because (ets.seasonality.vector=="ETS(M,Ad,N)") is store 56, I choose store 56 to see its seasonality.
myvar.4<-c("Date","Store56")
Roundy<-tr.x[myvar.4]
head(Roundy)
ts.Roundy<-ts(Roundy$Store56,frequency = 365.25/7,start=decimal_date(ymd("2014-07-19"))) ###range(ts.Roundy)=(3973, 42822)
plot(Roundy$Date,ts.Roundy,type="l",xlab="Date",ylab="Weekly Clif Bar Sales",ylim=c(0,45000))

ggseasonplot(ts.Roundy, year.labels=TRUE,col=rainbow(3),continuous=TRUE,labelgap = 0.15)+
  ylab("Weekly Sales")+
  ggtitle("Clif Bar Sales of Roundy's Supermarkets")


food <- read.csv(paste0(paths$data, "Cliffoodclean.csv"),header = TRUE)
food$Date<-as.Date(as.character(food$Date),format="%m/%d/%Y")

WM.NHM.SouthCentral.food<-food[food$ID==81,]
WM.NHM.SouthCentral.train<-subset(WM.NHM.SouthCentral.food,Date<as.Date("2017-01-07"))
WM.NHM.SouthCentral.df<-data.frame(Dates=WM.NHM.SouthCentral.train$Date, Sales=WM.NHM.SouthCentral.train$WeeklySales, Prices=WM.NHM.SouthCentral.train$price)

summary(WM.NHM.SouthCentral.df)


Roundy.food<-food[food$ID==56,]
Roundy.train<-subset(Roundy.food,Date<as.Date("2017-01-07"))
Roundy.df<-data.frame(Dates=Roundy.train$Date, Sales=Roundy.train$WeeklySales, SalesUnits=Roundy.train$SalesUnits, Prices=Roundy.train$price)
summary(Roundy.df)
## To set Sales 0-45000 and Prices 0-2, scale prices by 45000/2 to fit the range of sales 
cor(Roundy.df$SalesUnits,Roundy.df$Prices)
cor(Roundy.df$Sales,Roundy.df$Prices)


png(file='RoundySales.png', width=700, height=320)
theme_set(theme_gray())
ggplot(Roundy.df, aes(x=Dates))+
  geom_line(aes(y=Roundy.df$Sales, colour="Weely Sales"),size=1)  +
  geom_line(aes(y=Roundy.df$Prices*45000/2, colour="Unit Price"),size=1)    +
  scale_y_continuous(name = 'Weekly Sales', sec.axis = sec_axis(~.*2/45000, name="Price of Clif Bar"), limits = c(0,45000))   +
  scale_colour_manual(values = c("blue","red"))+
  theme_classic()+
  labs(x="Dates",colour="Variables")+
  theme(legend.position = c(0.1,0.85),legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black"),
    axis.title.y = element_text(color = 'red'),
    axis.title.y.right = element_text(color = 'blue'))
dev.off()  