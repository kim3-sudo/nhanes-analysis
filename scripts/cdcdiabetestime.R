#################################################
# R code to evaluate the CDC's timed diabetes dataset
# author: S Kim
#################################################

#################################################
## do prelim work
## load packages
library(mosaic)
## Hmisc is for numerical correlation matrix

#################################################
## load the nhanes dataset from the T drive
## this dataset is also found on the GitHub repo
diabetes <- read.csv("T:/datasets/DiabetesAtlasData.csv")
View(diabetes)
head(diabetes)
#################################################

#################################################
## some EDA
boxplot(diabetes$TotalPercentage, ylab = "Total Percentage")
plot(TotalPercentage~Year, data = diabetes)
favstats(diabetes$TotalPercentage)

#################################################
## consider linear regression
diabetesmod <- lm(TotalPercentage~t, data = diabetes)
summary(diabetesmod)
## new regression model
newmod = lm(TotalPercentage~t + I(t^2), data = diabetes)
summary(newmod)
plot(TotalPercentage~t, data = diabetes)
curve(predict(newmod,data.frame(t=x), type = "response"), col = "blue", add = TRUE)
plot(newmod, c(1:2,4))

#################################################
## let's use a time series instead
Per.ts = ts(diabetes$TotalPercentage, start=1980)
plot(Per.ts,
     lwd=2,
     col="darkblue",
     xlab="Year",
     ylab="Total Percentage with Diabetes",
     main="U.S. Population Percentage with Diabetes by Year")
points(diabetes$Year, diabetes$TotalPercentage, pch = 19, col = "darkblue")

#################################################
## make lagplots
plot(TotalPercentage[2:38]~TotalPercentage[1:37], data = diabetes, xlab = "previous percentage", ylab = "total percentage")
diff = diabetes$TotalPercentage[2:38] - diabetes$TotalPercentage[1:37]
plot(diff[2:37]~diff[1:36], xlab = "previous difference", ylab = "difference")

#################################################
## calculate autocorrelations and make plot

sluacf=function(series,lags=1,maxlag=NULL,ndiff=0){
  if(ndiff>0){series=diff(series,differences=ndiff)}
  seriesacf=acf(series,lag.max=maxlag,plot=F)
  seriesacf$acf[1]=NA
  if(lags>1){seriesacf$lag=seriesacf$lag*lags}
  seriesacf
}

PerACF = sluacf(Per.ts)

PerACF[1]

plot(PerACF,
     main = "ACF Plot of Total Percentage with Diabetes",
     lwd=2,
     xlib=c(1,12),
     ci.type = "ma")
