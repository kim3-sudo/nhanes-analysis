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

#################################################
## load the nhanes dataset from the T drive
## this dataset is also found on the GitHub repo
nhanes <- read.csv("T:/datasets/NHANES.csv")
View(nhanes)
head(nhanes)

#################################################
## some EDA
tally(~SurveyYr, data = nhanes)
plot(Diabetes~Gender, data = nhanes)
plot(Diabetes~Race1, data = nhanes)
plot(Diabetes~DaysMentHlthBad, data = nhanes)
plot(Diabetes~DaysPhysHlthBad, data = nhanes)
