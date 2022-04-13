data_raw <- read.csv("/Users/Hello/Downloads/beijingpm.csv") summary(data_raw)
str(data_raw) library(dplyr) library(ggplot2)
#43824 variables of 13 variables
dataint <- select(data_raw, c(month, cbwd, pm2.5, year, TEMP, PRES, Iws, No)) #select 8 variables + 1 variable ('no' column included for reference),
#others for data analysis
data <- na.omit(dataint) #removing all "NA" str(data)
summary(data) ##
hist(data$pm2.5) boxplot(data$pm2.5) hist(log(data$pm2.5)) boxplot(log(data$pm2.5)) summary(log(data$pm2.5))
data <- mutate(data, logpm2.5 = log(pm2.5)) Q1logpm2.5 <- quantile(data$logpm2.5, .25) Q3logpm2.5 <- quantile(data$logpm2.5, .75) IQR <- IQR(data$logpm2.5)
data <- subset(data, data$logpm2.5 > (Q1logpm2.5-1.5*IQR) & data$logpm2.5 < (Q3logpm2.5+1.5*IQR))
hist(log(data$pm2.5)) boxplot(log(data$pm2.5)) summary(log(data$pm2.5))

## month
plot(as.factor(data$month), main="Number of entries from each month", xlab="month", ylab="frequency")
### year
plot(as.factor(data$year), main="Number of entries from year", xlab="year", ylab="frequency") ### cbwd
plot(as.factor(data$cbwd), main="Number of entries with each type of wind direction", xlab="Wind direction", ylab="frequency")
## temp hist(data$TEMP) boxplot(data$TEMP) ## pres hist(data$PRES) boxplot(data$PRES) hist(log(data$PRES))
boxplot(log(data$PRES))
data <- mutate(data, logPRES = log(PRES)) ## Iws
hist(data$Iws) boxplot(data$Iws) hist(log(data$Iws)) boxplot(log(data$Iws))
data <- mutate(data, logIws = log(Iws)) ##corr plot
data_continuous <- data[,c(9,5,10,11)] install.packages("psych") library(psych)
pairs.panels(data_continuous, method = "pearson", # correlation method hist.col = "steelblue",
             pch = 21,
             
             density = TRUE, ellipses = FALSE)

#corr without importing new library
variable_list <- c("month", "DEWP", "TEMP", "PRES","Iws") for(i in colnames(data)[3:ncol(data)-2]) {
  #print(length(data[,variable_list[i]])) COR=cor.test(data[,"pm2.5"],data[,i],method="pearson")}
  
  ##
  ##relationship between pm2.5 and year
  boxplot(data$logpm2.5~data$year, ylab="log(pm2.5)", xlab="Year", main="Boxplot of log(pm2.5) vs year")
  summary(aov(data$logpm2.5~factor(data$year))) pairwise.t.test(data$logpm2.5, data$year, p.adjust.method = "none") ##relationship between pm2.5 and month
  #boxplot
  print(ggplot(data, aes(factor(data$month), logpm2.5)) + geom_boxplot()+ xlab("month")
  )
  summary(aov(data$logpm2.5~factor(data$month))) #t-test between winter and summer
  summerdata = data[data$month %in% c(6,7,8),] winterdata = data[data$month %in% c(12, 1, 2),] var.test(summerdata$logpm2.5,winterdata$logpm2.5)
  t.test(summerdata$logpm2.5,winterdata$logpm2.5,alt='greater',var.equal = FALSE)
  
  
  ##relationship between pm2.5 and cbwd #pre-process data
  # change "cv"" in cbwd to "SW"
  
  levels(data$cbwd)[1] <- "SW" # sort it to NE, NW, SE, SW
  data$cbwd <- factor(data$cbwd, levels = c("NE", "NW", "SE", "SW")) summary(data$cbwd)
  
  #visualization boxplot
  print(ggplot(data, aes(data$cbwd, logpm2.5)) + geom_boxplot() +
          xlab("cbwd")) summary(aov(data$logpm2.5~factor(data$cbwd)))
  
  #relationship between pm2.5 and TEMP modelT<-lm(data$logpm2.5~data$TEMP) summary(modelT)
  ggplot(data,aes(x=TEMP,y=logpm2.5))+geom_point()+geom_smooth(method="lm")
  
  
  ##
  #relationship between pm2.5 and PRES modelp<-lm(data$logpm2.5~ data$logPRES) summary(modelp)
  plot(modelp) ggplot(data,aes(x=logIws,y=logPRES))+geom_point()+geom_smooth(method="lm")
  
  
  
  ##
  #relationship between pm2.5 and Iws modelws<-lm(data$logpm2.5~ data$logIws) summary(modelws)
  plot(modelws) ggplot(data,aes(x=logIws,y=logpm2.5))+geom_point()+geom_smooth(method="lm")
  
  
  ##
  #which factor affects pm2.5 most in winter #seasonal plot
  library(xts) library(seasonal) #pre-process data
  data <- mutate(data,dates=paste(year,month,day,sep="-"))
  
  
  #covert to xct type
  dataxts <- as.xts(data$pm2.5, order.by = as.POSIXct(data$dates)) #calculate the mean of pm2,5 for every month
  datamonth <- apply.monthly(dataxts, FUN = "mean")
  
  
  #time series
  inds <- seq(as.Date("2010-1-1"), as.Date("2014-12-31"), by = "day") datamonthts <- ts(as.numeric(datamonth),
                                                                                        frequency = 12,
                                                                                        start=c(2010, as.numeric(format(inds[1], "%j")))) p2 <- ggseasonplot(datamonthts,
                                                                                                                                                             polar = T, year.labels = T,
                                                                                                                                                             year.labels.left = T) + ggtitle("Seasonal Plot: PM2.5") + ylab('ug/m^3')
  
  #find the most important factor
  dataw<-data%>%filter(month==1|month==2|month==12) modelT1<-lm(dataw$logpm2.5~dataw$TEMP) summary(modelT1)
  
  plot(modelT1)
  modelws1<-lm(dataw$logpm2.5~ dataw$logIws) summary(modelws1)
  plot(modelws1)
  modelp1<-lm(dataw$logpm2.5~ dataw$logPRES) summary(modelp1)
  plot(modelp1)
  