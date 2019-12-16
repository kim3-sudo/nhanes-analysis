#################################################
# R code to evaluate the modified NHANES dataset
# author: S Kim
#################################################

#################################################
## do prelim work
## load packages
library(mosaic)
library(Stat2Data)
library(car)
library(agricolae)
library(asbio)
library(leaps)
library(Hmisc)
library(ggplot2)
  ## Hmisc is for numerical correlation matrix

#################################################
## load the nhanes dataset from the T drive
## this dataset is also found on the GitHub repo
nhanes <- read.csv("T:/datasets/NHANES.csv")
View(nhanes)
head(nhanes)
#################################################

#################################################
## some EDA
tally(~SurveyYr, data = nhanes)

## make some scatter or box plots
plot(Diabetes~Gender, data = nhanes)
plot(Diabetes~Age, data = nhanes)
plot(Diabetes~Race1, data = nhanes)
plot(Diabetes~HHIncomeMid, data = nhanes)
plot(Diabetes~HomeRooms, data = nhanes)
plot(Diabetes~HomeOwn, data = nhanes)
plot(Diabetes~Work, data = nhanes)
plot(Diabetes~BMI, data = nhanes)
plot(Diabetes~Pulse, data = nhanes)
plot(Diabetes~BPSysAve, data = nhanes)
plot(Diabetes~BPDiaAve, data = nhanes)
plot(Diabetes~DirectChol, data = nhanes)
plot(Diabetes~TotChol, data = nhanes)
plot(Diabetes~UrineVol1, data = nhanes)
plot(Diabetes~UrineFlow1, data = nhanes)
plot(Diabetes~HealthGen, data = nhanes)
plot(Diabetes~PhysActive, data = nhanes)
plot(Diabetes~AlcoholDay, data = nhanes)

## find correlations
# load numerical data into matrix
nummat <- nhanes[, c("Age",
                     "HHIncomeMid",
                     "HomeRooms",
                     "Weight",
                     "Height",
                     "BMI",
                     "Pulse",
                     "BPSysAve",
                     "BPDiaAve",
                     "DirectChol",
                     "TotChol",
                     "UrineVol1",
                     "UrineFlow1",
                     "Diabetes",
                     "DaysPhysHlthBad",
                     "DaysMentHlthBad",
                     "SleepHrsNight",
                     "PhysActiveDays",
                     "AlcoholDay",
                     "AlcoholYear")]
mat1 <- rcorr(as.matrix(nummat))
mat1
