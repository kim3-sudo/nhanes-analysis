#################################################
# R code to evaluate the modified NHANES dataset
# author: S Kim
#################################################

## For all tests, will use alpha = .05
## For all model building, will use alpha = .1

#################################################
## do prelim work
## load packages
library(mosaic)
library(PerformanceAnalytics)
library(psych)
library(Hmisc)
library(HH)
library(caret)
library(car)
library(rcompanion)
library(dplyr)
library(lmtest)
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
favstats(~Diabetes, data = nhanes)
ncol(nhanes)
## find correlations
# load numerical data into matrix
nummat <- nhanes[, c("Age","HHIncomeMid","HomeRooms","Weight","Height",
                     "BMI","Pulse","BPSysAve","BPDiaAve","DirectChol",
                     "TotChol", "UrineVol1","UrineFlow1","Diabetes","DaysPhysHlthBad",
                     "DaysMentHlthBad","SleepHrsNight","PhysActiveDays","AlcoholDay","AlcoholYear")]
mat1 <- rcorr(as.matrix(nummat))
mat1
chart.Correlation(nummat, histogram = TRUE, pch=16)
corr.test(nummat, use = "pairwise", adjust="none", alpha=0.05)

## make some scatter or box plots
plot(Diabetes~Gender, main = "Diabetes by Gender", data = nhanes)
plot(Diabetes~Age, data = nhanes)
plot(Diabetes~Race1, main = "Diabetes by Race", xlab = "Race", data = nhanes)
plot(Diabetes~HHIncomeMid, data = nhanes)
plot(Diabetes~HomeRooms, data = nhanes)
plot(Diabetes~HomeOwn, data = nhanes)
plot(Diabetes~Work, main = "Diabetes by Work Status", xlab = "Work Status", data = nhanes)
plot(Diabetes~BMI, data = nhanes)
plot(Diabetes~Pulse, data = nhanes)
plot(Diabetes~BPSysAve, data = nhanes)
plot(Diabetes~BPDiaAve, data = nhanes)
plot(Diabetes~DirectChol, data = nhanes)
plot(Diabetes~TotChol, data = nhanes)
plot(Diabetes~UrineVol1, data = nhanes)
plot(Diabetes~UrineFlow1, data = nhanes)
plot(Diabetes~HealthGen, main = "Diabetes by General Health Status", xlab = "General Health", data = nhanes)
plot(Diabetes~PhysActive, data = nhanes)
plot(Diabetes~AlcoholDay, data = nhanes)

#################################################
## Try building some models
#################################################
## Record influences at the univariate level as a first round elimination
glmgend <- glm(Diabetes~Gender, data = nhanes)
summary(glmgend) #**
glmage <- glm(Diabetes~Age, data = nhanes)
summary(glmage) #***
glmrace <- glm(Diabetes~Race1, data = nhanes)
summary(glmrace) #NOT SIGNIFICANT - ELIMINATE
glmhhin <- glm(Diabetes~HHIncomeMid, data = nhanes)
summary(glmhhin) #**
glmhroo <- glm(Diabetes~HomeRooms, data = nhanes)
summary(glmhroo) #NOT SIGNIFICANT - ELIMINATE
glmhown <- glm(Diabetes~HomeOwn, data = nhanes)
summary(glmhown) #NOT SIGNIFICANT - ELIMINATE
glmwork <- glm(Diabetes~Work, data = nhanes)
summary(glmwork) #Not working is significant at *, consider eliminating
glmweig <- glm(Diabetes~Weight, data = nhanes)
summary(glmweig) #***
glmheig <- glm(Diabetes~Height, data = nhanes)
summary(glmheig) # NOT SIGNIFICANT - ELIMINATE
glmbmi <- glm(Diabetes~BMI, data = nhanes)
summary(glmbmi) #***
glmpuls <- glm(Diabetes~Pulse, data = nhanes)
summary(glmpuls) #NOT SIGNIFICANT - ELIMINATE
glmbpsy <- glm(Diabetes~BPSysAve, data = nhanes)
summary(glmbpsy) #***
glmbpdi <- glm(Diabetes~BPDiaAve, data = nhanes)
summary(glmbpdi) #NOT SIGNIFICANT - ELIMINATE
glmdire <- glm(Diabetes~DirectChol, data = nhanes)
summary(glmdire) #***
glmtotc <- glm(Diabetes~TotChol, data = nhanes)
summary(glmtotc) #***
glmuvol <- glm(Diabetes~UrineVol1, data = nhanes)
summary(glmuvol) #***
glmuflo <- glm(Diabetes~UrineFlow1, data = nhanes)
summary(glmuflo) #**
glmheal <- glm(Diabetes~HealthGen, data = nhanes)
summary(glmheal) #Only fair and good are significant at *, consider eliminating
glmdphy <- glm(Diabetes~DaysPhysHlthBad, data =nhanes)
summary(glmdphy) #***
glmdmen <- glm(Diabetes~DaysMentHlthBad, data = nhanes)
summary(glmdmen) #**
glmslee <- glm(Diabetes~SleepHrsNight, data = nhanes)
summary(glmslee) #NOT SIGNIFICANT - ELIMINATE
glmphys <- glm(Diabetes~PhysActive, data = nhanes)
summary(glmphys) #***
glmpday <- glm(Diabetes~PhysActiveDays, data = nhanes)
summary(glmpday) #**
glmalco <- glm(Diabetes~AlcoholDay, data = nhanes)
summary(glmalco) #NOT SIGNIFICANT - ELIMINATE
glmalcy <- glm(Diabetes~AlcoholYear, data = nhanes)
summary(glmalcy) #NOT SIGNIFICANT - ELIMINATE
##
## KEEP Gender, Age, HHIncomeMid, Work, Wieght, BMI, BPSysAve, DirectChol, TotChol, UrineVol1, UrineFlow1,
## HealthGen, DaysPhysHlthBad, DaysMentHlthBad, PhysActive, PhysActiveDays

#################################################
## run the actual logitic regression
## define full and null models and do the stepwise procedure
model.null = glm(Diabetes ~ 1,
                 data = nhanes,
                 family = binomial(link="logit"))
model.full = glm(Diabetes ~ Gender + Age + HHIncomeMid + Work + Weight +
                   BMI + BPSysAve + DirectChol + TotChol + UrineVol1 + 
                   UrineFlow1 + HealthGen + DaysPhysHlthBad + DaysMentHlthBad +
                   PhysActive + PhysActiveDays,
                 data = nhanes,
                 family = binomial(link="logit"))
step(model.null,
     scope = list(upper = model.full),
     direction = "both",
     test="Chisq",
     data = nhanes)
## test built model
model.next = glm(Diabetes ~ Age + BMI + TotChol + DaysPhysHlthBad +
                    HealthGen + UrineVol1 + DirectChol + DaysMentHlthBad +
                    Work + PhysActiveDays + BPSysAve,
                  data = nhanes,
                  family = binomial(link="logit"),
                  na.action(na.omit))
summary(model.next)
## test model with insigificant terms removed
model.final = glm(Diabetes ~ Age + BMI + TotChol + DaysPhysHlthBad + 
                    HealthGen + UrineVol1 + DirectChol + DaysMentHlthBad +
                    Work,
                  data = nhanes,
                  family = binomial(link="logit"),
                  na.action(na.omit))
summary(model.final)
## analysis of variance for individual terms - uses lib(car)
Anova(model.next, type = "II", test = "Wald")
Anova(model.final, type = "II", test = "Wald")
## pseudo-R-squared
nagelkerke(model.final)
##overall p-value for the model
#### create data frame with variables in final model and NA's omitted
Data.final = select(nhanes,
                    Diabetes, 
                    Age, 
                    BMI, 
                    TotChol, 
                    DaysPhysHlthBad, 
                    HealthGen, 
                    UrineVol1, 
                    DirectChol, 
                    DaysMentHlthBad, 
                    Work)
Data.final = na.omit(Data.final)
#### define null models and compare to the final model
new.model.null <- glm(Diabetes~1,
                  data = Data.final,
                  family = binomial(link="logit")
                  )
anova(model.final, new.model.null, test="Chisq")
lrtest(model.final)
## plot of standardized residuals
plot(fitted(model.final), rstandard(model.final))
## simple plot of predicted values
plot(Diabetes ~ Age,
     data = Data.final,
     pch = 16,
     xlab = "Predicted probability of 1 response",
     ylab = "Actual response",
     main = "Diabetes ~ Age Predicted Values Plot")
## check for overdispersion
summary(model.final)$deviance / summary(model.final)$df.residual
  ## ratio is < 1.5, so no worries about overdispersion

#################################################
## Alternative (or double-checking) the model assessement using compareGLM
#### define models to compare
model.1 <- glm(Diabetes~1,
               data = Data.final, family=binomial())
model.2 <- glm(Diabetes ~ Age,
               data = Data.final, family=binomial())
model.3 <- glm(Diabetes ~ Age + BMI,
               data = Data.final, family=binomial())
model.4 <- glm(Diabetes ~ Age + BMI + TotChol,
               data = Data.final, family=binomial())
model.5 <- glm(Diabetes ~ Age + BMI + TotChol + DaysPhysHlthBad,
               data = Data.final, family=binomial())
model.6 <- glm(Diabetes ~ Age + BMI + TotChol + DaysPhysHlthBad + HealthGen,
               data = Data.final, family=binomial())
model.7 <- glm(Diabetes ~ Age + BMI + TotChol + DaysPhysHlthBad + HealthGen + UrineVol1,
               data = Data.final, family=binomial())
model.8 <- glm(Diabetes ~ Age + BMI + TotChol + DaysPhysHlthBad + HealthGen + UrineVol1 + DirectChol,
               data = Data.final, family=binomial())
model.9 <- glm(Diabetes ~ Age + BMI + TotChol + DaysPhysHlthBad + HealthGen + UrineVol1 + DirectChol + DaysMentHlthBad,
               data = Data.final, family=binomial())
model.10 <- glm(Diabetes ~ Age + BMI + TotChol + DaysPhysHlthBad + HealthGen + UrineVol1 + DirectChol + DaysMentHlthBad + Work,
                data = Data.final, family=binomial())
#### use compare.glm to assess fit statistics
anova(model.1, model.2, model.3, model.4, model.5, model.6, model.7, model.8, model.9, model.10, test="Chisq")

#################################################
## training and holdout
favstats(~Diabetes, data = nhanes)[c("n")]
(ntrain = 0.67 * 2167)
(nholdout = 0.33 *2167)
traindiab <- nhanes[c(1:1452),]
holdoutdiab <- nhanes[c(1453:2167),]
## doublecheck math and dataset building
favstats(~Diabetes, data = traindiab)[c("n")]
favstats(~Diabetes, data = holdoutdiab)[c("n")]
trainmod <- model.final
summary(trainmod)
predictions <- trainmod %>% predict(holdoutdiab)
predictions
diabhat <- predict(trainmod, holdoutdiab)
residual = holdoutdiab$Diabetes - diabhat
residual
mean(residual)
sd(residual)
cor(holdoutdiab$Diabetes~diabhat)
