#DATA ANALYSIS FOR VR PROJECT - HAZARD RECOGNITION SKILL

#Set up working directory, download libraries and upload data file----
setwd("C:/Users/jazmi/OneDrive - UCB-O365/VR Project/Data Analysis/Data Analysis Code")
getwd()

#Install libraries
library(psych)
library(car)
library(pwr)
library(lmSupport)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(MASS)
library(lme4)
library(lmerTest)
library(tidyr)
library(afex)
library(emmeans)
library(lmerTest)

#Download data file
data <-read.csv("C:/Users/jazmi/OneDrive - UCB-O365/Research Data Camp/VR Project Data.csv")

#Mean center variables--------------
#Descriptive statistics for HR Skill
describe(data$HR.Skill.B)
describe(data$HR.Skill.T)
describe(data$HR.Skill.VR)

#Mean center variables
data$HR.Skill.B_c <- data$HR.Skill.B - mean(data$HR.Skill.B, na.rm = TRUE)
data$HR.Skill.T_c <- data$HR.Skill.T - mean(data$HR.Skill.T, na.rm = TRUE)
data$HR.Skill.VR_c <- data$HR.Skill.VR - mean(data$HR.Skill.VR, na.rm = TRUE)

#Check the mean of the centered variables (it should be very close to 0)
mean(data$HR.Skill.B_c, na.rm = TRUE)
mean(data$HR.Skill.T_c, na.rm = TRUE)
mean(data$HR.Skill.VR_c, na.rm = TRUE)

#Calculate changes in hazard recognition skill due to interventions---
#Mean centered
data$HR.Skill.BvsT <- ((-1*data$HR.Skill.B_c)+(1*data$HR.Skill.T_c)+(0*data$HR.Skill.VR_c))
data$HR.Skill.TvsVR <- ((0*data$HR.Skill.B_c)+(-1*data$HR.Skill.T_c)+(1*data$HR.Skill.VR_c))

#Not mean centered
data$HR.Skill.BvsT.NM <- ((-1*data$HR.Skill.B)+(1*data$HR.Skill.T)+(0*data$HR.Skill.VR))
data$HR.Skill.TvsVR.NM <- ((0*data$HR.Skill.B)+(-1*data$HR.Skill.T)+(1*data$HR.Skill.VR))

#Summary statistics 
describe(data$HR.Skill.BvsT)
describe(data$HR.Skill.TvsVR)
describe(data$HR.Skill.BvsT.NM)
describe(data$HR.Skill.TvsVR.NM)

#Paired sample t-tests---
#Mean centered 
t.test(data$HR.Skill.B_c, data$HR.Skill.T_c, paired = TRUE, conf.level = 0.95)
t.test(data$HR.Skill.T_c, data$HR.Skill.VR_c, paired = TRUE, conf.level = 0.95)

#Not mean centered 
t.test(data$HR.Skill.B, data$HR.Skill.T, paired = TRUE, conf.level = 0.95)
t.test(data$HR.Skill.T, data$HR.Skill.VR, paired = TRUE, conf.level = 0.95)

#Perform linear regression analysis controlling for the assessment used-----
#Change in Hazard Recognition skill from baseline to post-passive training, controlling for picture identity at each assessment phase
HR.Skill.BvsT<- lm(HR.Skill.BvsT~factor(Picture.B)+factor(Picture.T), data=data)
summary(HR.Skill.BvsT)
anova(HR.Skill.BvsT)

#Change in Hazard Recognition skill from post-passive training to post-VR
HR.Skill.TvsVR <- lm(HR.Skill.TvsVR ~factor(Picture.T)+factor(Picture.VR), data=data)
summary(HR.Skill.TvsVR)
anova(HR.Skill.TvsVR)

#Effect of picture identity at every assessment
HR.B <- lm(HR.Skill.B ~ factor(Picture.B), data=data)
anova(HR.B)

HR.T <- lm(HR.Skill.T ~ factor(Picture.T), data=data)
anova(HR.T)

HR.VR <- lm(HR.Skill.VR ~ factor(Picture.VR), data=data)
anova(HR.VR)

#Obtain Coefficients
summary(HR.B)$coef
summary(HR.T)$coef
summary(HR.VR)$coef

#There is a significant effect of picture identity on all three phases, which is different on all three phases. This indicates a moderation effect from the type of assessment used.
tapply(data$HR.Skill.B, data$Picture.B, mean)
tapply(data$HR.Skill.T, data$Picture.T, mean)
tapply(data$HR.Skill.VR, data$Picture.VR, mean)


