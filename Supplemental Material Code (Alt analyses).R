setwd("~/Desktop")

library(tidyverse)
library(lme4)
library(lmerTest)
library(ggplot2)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(dplyr)
library(contrast)
library(emmeans)
library(svglite)
library(ggeffects)

#read in the data 
pitch_org <- read.table('master.txt', header=T)

#clean data and re-code factors to numeric, binary codes
pitch <- pitch_org %>% 
  #filter(noisy==1)
  mutate(no_topic = if_all(convo:vocalplay, `==`,0)) %>% 
  filter(!(no_topic & noisy == 1)) %>% # exclude 107 rows that are annotated exclusively as noisy (final count = 3620)
  mutate(register = recode(register, ADS = '0', IDS = '1' ), #binary code register
         register = as.numeric(register)) %>%
  mutate(adu_gender = recode(adu_gender, female = '0', male = '1' ), 
         adu_gender = as.numeric(adu_gender)) %>%
  mutate(chi_gender = recode(chi_gender, female = '0', male = '1' ), 
         chi_gender = as.numeric(chi_gender)) %>%
  drop_na(adu_gender)


#convert variables to numeric values for centering and scaling
pitch$convo <- as.numeric(pitch$convo)
pitch$comfort <- as.numeric(pitch$comfort)
pitch$sing <- as.numeric(pitch$sing)
pitch$read <- as.numeric(pitch$read)
pitch$inform <- as.numeric(pitch$inform)
pitch$imperative <- as.numeric(pitch$imperative)
pitch$question <- as.numeric(pitch$question)
pitch$vocalplay <- as.numeric(pitch$vocalplay)
pitch$noisy <- as.numeric(pitch$noisy)

#check the structure of the dataset
str(pitch)

#center and scale predictors 
pitch[,8:16]<-scale(pitch[,8:16], center = TRUE, scale = TRUE)
pitch[,18] <- scale(pitch[,18], center = TRUE, scale = TRUE)
pitch[,21] <- scale(pitch[,21], center = TRUE, scale = TRUE)

#center and scale register 
pitch[,2] <- scale(pitch[,2], center = TRUE, scale = TRUE)

#log-transform pitch variables 
pitch[,3:7] <- log(pitch[,3:7])

#center and scale log-transformed pitch variables 
pitch[,3:7] <- scale(pitch[,3:7], center = TRUE, scale = TRUE)


######################Supplemental Data Analyses################################################

#--Adult Speaker Gender and Register Analysis: Alternative Model 1-----#

mpitch_all <- lmer(m_pitch ~ adu_gender*convo*register + adu_gender*sing*register + adu_gender*inform*register + adu_gender*read*register + adu_gender*imperative*register + adu_gender*question*register + register*adu_gender*comfort+ register*adu_gender*vocalplay + (1|ID) + (1|coder), data = pitch)
summary(mpitch_all)

tab_model(mpitch_all, show.re.var= TRUE, show.se = TRUE, file = "mpitch_all.html",
          pred.labels =c("(Intercept)", "Adult Gender", "Conversational Basics", "Register", "Sing", "Inform", "Read", "Imperative", "Question", "Comfort", "Vocal Play", 
                         "Adult Gender*Conversational Basics", "Adult Gender*Register","Conversational Basics*Register", "Adult Gender*Sing", "Register*Sing", "Adult Gender*Inform","Register*Inform", "Adult Gender*Read", "Adult Gender*Imperative", 
                         "Register*Imperative", "Adult Gender*Question", "Register*Question", "Register*Comfort", "Adult Gender*Comfort", "Adult Gender*Vocal Play", "Adult Gender*Conversational Basics*Register",
                         "Adult Gender*Register*Sing", "Adult Gender*Register*Inform", "Adult Gender*Register*Imperative", "Adult Gender*Register*Question"),
          dv.labels= "Mean Pitch")

sdpitch_all <- lmer(sd_pit ~ adu_gender*convo*register + adu_gender*sing*register + adu_gender*inform*register + adu_gender*read*register + adu_gender*imperative*register + adu_gender*question*register + register*adu_gender*comfort+ register*adu_gender*vocalplay + (1|ID) + (1|coder), data = pitch)
summary(sdpitch_all)

tab_model(sdpitch_all, show.re.var= TRUE, show.se = TRUE, file = "sdpitch_all.html",
          pred.labels =c("(Intercept)", "Adult Gender", "Conversational Basics", "Register", "Sing", "Inform", "Read", "Imperative", "Question", "Comfort", "Vocal Play", 
                         "Adult Gender*Conversational Basics", "Adult Gender*Register","Conversational Basics*Register", "Adult Gender*Sing", "Register*Sing", "Adult Gender*Inform","Register*Inform", "Adult Gender*Read", "Adult Gender*Imperative", 
                         "Register*Imperative", "Adult Gender*Question", "Register*Question", "Register*Comfort", "Adult Gender*Comfort", "Adult Gender*Vocal Play", "Adult Gender*Conversational Basics*Register",
                         "Adult Gender*Register*Sing", "Adult Gender*Register*Inform", "Adult Gender*Register*Imperative", "Adult Gender*Register*Question"),
          dv.labels= "Pitch Variability")


#--Adult Speaker Gender and Register Analysis: Alternative Model 2-----#

#mean pitch 

mpitch_four= lmer(m_pitch ~ adu_gender*register*convo + adu_gender*register*inform + adu_gender*register*imperative 
               + adu_gender*register*question + (1 | ID) + (1 | coder), data = pitch, REML = TRUE, lmerControl(optimizer = "bobyqa", 
                                                                                                  optCtrl = list(maxfun = 2e5)))
summary(mpitch_four)

tab_model(mpitch_four, show.re.var= TRUE, show.se = TRUE, file = "mpitch_four.html",
         pred.labels =c("(Intercept)", "Adult Gender", "Register", "Conversational Basics", "Inform", "Imperative", "Question",
                        "Adult Gender*Register", "Adult Gender*Conversational Basics", "Register*Conversational Basics","Adult Gender*Inform","Register*Inform", "Adult Gender*Imperative", 
                        "Register*Imperative", "Adult Gender*Question", "Register*Question", "Adult Gender*Register*Conversational Basics",
                        "Adult Gender*Register*Inform", "Adult Gender*Register*Imperative", "Adult Gender*Register*Question"),
         dv.labels= "Mean Pitch")


sdpitch_four= lmer(sd_pit ~ adu_gender*register*convo + adu_gender*register*inform + adu_gender*register*imperative 
                  + adu_gender*register*question + (1 | ID) + (1 | coder), data = pitch) 
                                                                                                            
summary(sdpitch_four)

tab_model(sdpitch_four, show.re.var= TRUE, show.se = TRUE, file = "sdpitch_four.html",
          pred.labels =c("(Intercept)", "Adult Gender", "Register", "Conversational Basics", "Inform", "Imperative", "Question",
                         "Adult Gender*Register", "Adult Gender*Conversational Basics", "Register*Conversational Basics","Adult Gender*Inform","Register*Inform", "Adult Gender*Imperative", 
                         "Register*Imperative", "Adult Gender*Question", "Register*Question", "Adult Gender*Register*Conversational Basics",
                         "Adult Gender*Register*Inform", "Adult Gender*Register*Imperative", "Adult Gender*Register*Question"),
          dv.labels= "Pitch Variability")


#-------------IDS and Infant Gender: Alternative Model ----------#

IDS_data <- pitch[which(pitch$register > 0),]

infant_model_mean = lmer(m_pitch ~ adu_gender*chi_gender*convo + adu_gender*chi_gender*inform + 
              adu_gender*chi_gender*question + adu_gender*chi_gender*imperative + adu_gender*chi_gender*read + adu_gender*chi_gender*comfort +
              (1|ID)+ (1|coder), data = IDS_data) 

summary(infant_model_mean)

tab_model(infant_model_mean, show.re.var= TRUE, show.se = TRUE, file = "infantmean1.html",
          pred.labels =c("(Intercept)", "Adult Gender", "Child Gender", "Conversational Basics", "Inform", "Question", "Imperative", "Read", "Comfort",
                         "Adult Gender*Child Gender", "Adult Gender*Conversational Basics", "Child Gender*Conversational Basics","Adult Gender*Inform","Child Gender*Inform", "Adult Gender*Question", 
                         "Child Gender*Question","Adult Gender*Imperative", "Child Gender*Imperative", "Adult Gender*Read", "Child Gender*Read", "Adult Gender*Comfort", "Child Gender*Comfort", 
                         "Adult Gender*Child Gender*Conversational Basics", "Adult Gender*Child Gender*Inform", "Adult Gender*Child Gender*Question",
                         "Adult Gender*Child Gender*Imperative", "Adult Gender*Child Gender*Read", "Adult Gender*Child Gender*Comfort"),
          dv.labels= "Mean Pitch")


infant_model_sd = lmer(sd_pit ~ adu_gender*chi_gender*convo + adu_gender*chi_gender*inform + 
                           adu_gender*chi_gender*question + adu_gender*chi_gender*imperative + adu_gender*chi_gender*read + adu_gender*chi_gender*comfort 
                         + (1|ID)+ (1|coder), data = IDS_data) 

summary(infant_model_sd)

tab_model(infant_model_sd, show.re.var= TRUE, show.se = TRUE, file = "infantsd1.html",
          pred.labels =c("(Intercept)", "Adult Gender", "Child Gender", "Conversational Basics", "Inform", "Question", "Imperative", "Read", "Comfort",
                         "Adult Gender*Child Gender", "Adult Gender*Conversational Basics", "Child Gender*Conversational Basics","Adult Gender*Inform","Child Gender*Inform", "Adult Gender*Question", 
                         "Child Gender*Question","Adult Gender*Imperative", "Child Gender*Imperative", "Adult Gender*Read", "Child Gender*Read", "Adult Gender*Comfort", "Child Gender*Comfort", 
                         "Adult Gender*Child Gender*Conversational Basics", "Adult Gender*Child Gender*Inform", "Adult Gender*Child Gender*Question",
                         "Adult Gender*Child Gender*Imperative", "Adult Gender*Child Gender*Read", "Adult Gender*Child Gender*Comfort"),
          dv.labels= "Pitch Variability")

