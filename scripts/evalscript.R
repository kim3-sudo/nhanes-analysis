#################################################
# R code to evaluate the modified NHANES dataset
# author: S Kim
#################################################

#################################################
## do prelim work
## load packages
library(mosaic)
library(Stat2Data)
library(leaps)
library(Hmisc)
library(HH)
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
ncol(nhanes)
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

#################################################
## Try building some models
## automated backwards procedure
fullback <- lm(Diabetes~Gender+
                 Age+
                 Race1+
                 HHIncomeMid+
                 HomeRooms+
                 HomeOwn+
                 Work+
                 Weight+
                 Height+
                 BMI+
                 Pulse+
                 BPSysAve+
                 BPDiaAve+
                 DirectChol+
                 TotChol+
                 UrineVol1+
                 UrineFlow1+
                 HealthGen+
                 DaysPhysHlthBad+
                 DaysMentHlthBad+
                 SleepHrsNight+
                 PhysActive+
                 PhysActiveDays+
                 AlcoholDay+
                 AlcoholYear,
               data = nhanes)
MSE = (summary(fullback)$sigma)^2
step(fullback, scale = MSE, direction = "backward")

## automated forwards procedure
noneforw <- lm(Diabetes~1, data = nhanes)
step(noneforw, scope = list(upper=fullback), scale = MSE, direction = "forward")

## automated stepwise procedure
nonestep <- lm(Diabetes~1, data = nhanes)
step(nonestep, scope = list(upper=fullback), scale = MSE)

## best subsets procedure
all = regsubsets(Diabetes~Gender+
                   Age+
                   Race1+
                   HHIncomeMid+
                   HomeRooms+
                   HomeOwn+
                   Work+
                   Weight+
                   Height+
                   BMI+
                   Pulse+
                   BPSysAve+
                   BPDiaAve+
                   DirectChol+
                   TotChol+
                   UrineVol1+
                   UrineFlow1+
                   HealthGen+
                   DaysPhysHlthBad+
                   DaysMentHlthBad+
                   SleepHrsNight+
                   PhysActive+
                   PhysActiveDays+
                   AlcoholDay+
                   AlcoholYear,
                 data = nhanes)
  ## explore adjusted R^2 values
summary(all)$adjr2
plot(all, scale = "adjr2")
  ## restricted subset
newrestrict = regsubsets(Diabetes~Gender+
                           Age+
                           Race1+
                           HHIncomeMid+
                           HomeRooms+
                           HomeOwn+
                           Work+
                           Weight+
                           Height+
                           BMI+
                           Pulse+
                           BPSysAve+
                           BPDiaAve+
                           DirectChol+
                           TotChol+
                           UrineVol1+
                           UrineFlow1+
                           HealthGen+
                           DaysPhysHlthBad+
                           DaysMentHlthBad+
                           SleepHrsNight+
                           PhysActive+
                           PhysActiveDays+
                           AlcoholDay+
                           AlcoholYear,
                         nbest = 2,
                         data = nhanes)
summary(newrestrict)
  ## explore Cp values, smaller is better
plot(newrestrict, scale = "Cp")
  ## get the Cp for any model
newfull = lm(Diabetes~Gender+
               Age+
               Race1+
               HHIncomeMid+
               HomeRooms+
               HomeOwn+
               Work+
               Weight+
               Height+
               BMI+
               Pulse+
               BPSysAve+
               BPDiaAve+
               DirectChol+
               TotChol+
               UrineVol1+
               UrineFlow1+
               HealthGen+
               DaysPhysHlthBad+
               DaysMentHlthBad+
               SleepHrsNight+
               PhysActive+
               PhysActiveDays+
               AlcoholDay+
               AlcoholYear,
             data = nhanes)
MSE=(summary(newfull)$sigma)^2
extractAIC(lm(Diabetes~Gender+
                Age+
                Race1+
                HHIncomeMid+
                HomeRooms+
                HomeOwn+
                Work+
                Weight+
                Height+
                BMI+
                Pulse+
                BPSysAve+
                BPDiaAve+
                DirectChol+
                TotChol+
                UrineVol1+
                UrineFlow1+
                HealthGen+
                DaysPhysHlthBad+
                DaysMentHlthBad+
                SleepHrsNight+
                PhysActive+
                PhysActiveDays+
                AlcoholDay+
                AlcoholYear,
              data = nhanes),
           scale = MSE)
summaryHH(all)

# best model uses:
## Age, HomeOwnOwn, BMI, Pulse, TotChol, HealthGen, DaysPhysHlthBad, DaysMentHlthBad

#################################################
## Validation by training and holding out
favstats(~Diabetes, data = nhanes)[c("n")]
(trainn <- 0.67*2167) # 1452
(holdn <- 0.33*2167) # 715
## break the dataset
trainnhanes <- nhanes[c(1:1452),]
holdnhanes <- nhanes[c(1453:2167),]
## train the model
diabetestrainmod <- glm(Diabetes~Age+HomeOwn+BMI+Pulse+TotChol+HealthGen+DaysPhysHlthBad+DaysMentHlthBad, data = trainnhanes)
summary(diabetestrainmod)
