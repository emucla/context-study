setwd("~/Desktop/Desktop - Emily’s MacBook Pro/Winter 2022/EOC/Pitch Analyses")

library(tidyverse)
library(lme4)
library(lmerTest)
library(ggplot2)
library(sjPlot)
library(plyr)

#read in the data 
pitch_org <- read.table('pitchpilot.txt', header=T)

# excluding exclusively noisy rows:exclude rows in which rows look like this: columns 8-15 are coded as 0 AND 16 is coded as 1 

pitch <- pitch_org[!(!apply(pitch_org[,8:15], 1,any) & pitch_org[,16] == 1),]
#3 SubIDs were excluded from original coding because all files were coded as noisy only- check in on this

#renaming speech column to addressee in pitch file 
names(pitch)[names(pitch) == 'speech'] <- 'addressee'

#raw value of pitch: creates histogram checking distribution of mean pitch and pitch variability 

hist(pitch$m_pitch)
hist(pitch$sd_pitch)

#log pitch: creates histogram checking distribution of mean pitch and pitch variability 
logp<- log(pitch$m_pitch)
hist(logp) 

logsd<- log(pitch$sd_pit)
hist(logsd) 

#linear mixed effects regression for Analysis 1: Comfort IDS v other IDS contexts

#subset data to get IDS speech utterances only 
IDScom <- subset(pitch, addressee == "CDS")

#run the linear mixed effects regression - MEAN PITCH
mpitch_comfort = lmer(log(m_pitch) ~ comfort + (1 | ID), data = IDScom)
summary(mpitch_comfort)

#create formatted regression table
library(sjPlot)

tab_model(mpitch_comfort, show.re.var= TRUE, 
pred.labels =c("(Intercept)", "Comfort"),
dv.labels= "log(Mean Pitch)")

#run the linear mixed effects regression - PITCH VARIABILITY
sdpit_comfort = lmer(log(sd_pit) ~ comfort + (1 | ID), data = IDScom)
summary(sdpit_comfort)

#create regression table 
tab_model(sdpit_comfort, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Comfort"),
          dv.labels= "Pitch Variability")

#Analysis 2 - IDS v ADS inform utterances 
#linear mixed effects regression for IDS v ADS inform utterances - this extracts all inform utterances 
inform <- subset(pitch, inform =="1")

#selects audio files coded exclusively as inform 
inform_only <-inform[!apply(inform[,c(8:10, 12:16)], 1,any) & inform[,11] == 1,]

#linear mixed effects for MEAN PITCH
mpitch_informonly = lmer(log(m_pitch) ~ addressee + (1 | ID), data = inform_only)
summary(mpitch_informonly)

#create regression table 
tab_model(mpitch_informonly, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "addressee"),
          dv.labels= "log(Mean Pitch)")

#linear mixed effects regression - PITCH VARIABILITY
sdpit_informonly = lmer(log(sd_pit) ~ addressee + (1 | ID), data = inform_only)
summary(sdpit_informonly)

tab_model(sdpit_informonly, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "addressee"),
          dv.labels= "Pitch Variablity")

#Analysis 3: mixed effects logistic regression for frequency of comfort utterances in IDS v ADS

#dummy code ADS and CDS - binary code CDS and ADS in order to run logistic regression 

pitch$cds <- revalue(pitch$addressee, c("CDS"="1", "ADS"="0"))
addressee_com <- as.factor(pitch$cds)

#Logistic regression for comfort utterance frequency by addressee- not working and need to figure out 

addressee_comfort = glmer(comfort ~ addressee_com + (1 | ID), data = pitch, family=binomial)
summary(addressee_comfort)

#formatting regression table
tab_model(addressee_comfort, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Comfort"),
          dv.labels= "Comfort Utterances in IDS v ADS")


#mixed effects linear regression

#summing number of observations in each category in the pitch file 
#excluding contexts that have less than 20 instances 
sum(pitch[,8])
sum(pitch[,9]) #exclude
sum(pitch[,10]) #exclude
sum(pitch[,11])
sum(pitch[,12])
sum(pitch[,13])
sum(pitch[,14])
sum(pitch[,15]) #exclude
sum(pitch[,16]) #exclude 

#checking on interaction terms (context category*addressee) and excluding ones with less than 10 instances from model 
convo_addressee <- tapply(pitch2$convo, pitch2$addressee, sum)
inform_addressee <- tapply(pitch2$inform, pitch2$addressee, sum)
read_addressee <- tapply(pitch2$read, pitch2$addressee, sum) #less than 10 ADS instances
imperative_addressee <- tapply(pitch2$imperative, pitch2$addressee, sum) #less than 10 ADS instances
question_addressee <- tapply(pitch2$question, pitch2$addressee, sum)

#creating new dataset to run mixed effects linear regression - excluding categories from above 
pitch2 = subset(pitch, select = -c(comfort, sing, vocalplay, noisy))

#running mixed effects linear regression 
mpitch_model = lmer(log(m_pitch) ~ addressee + convo +convo*addressee + inform + inform*addressee + read 
                   + imperative + question + question*addressee + 
                    (1 | ID) + (1 | coder), data = pitch2)
summary(mpitch_model)

#creating regression table 
tab_model(mpitch_model, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Addressee", "Conversational Basics","Inform",
                         "Reading", "Imperative","Questions","Addressee*Conversational Basics",
                         "Addressee*Inform","Addressee*Questions"),
          dv.labels= "Mean Pitch")


#doing the same as above except for pitch variability 
#run the model 
sdpitch_model = lmer(log(sd_pit) ~ addressee + convo +convo*addressee + inform + inform*addressee + read + 
       imperative + question + question*addressee + (1 | ID) + (1 | coder), data = pitch2)
summary(sdpitch_model)

#format regression table
tab_model(sdpitch_model, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Addressee", "Conversational Basics","Inform",
                         "Reading", "Imperative","Questions","Addressee*Conversational Basics",
                         "Addressee*Inform","Addressee*Questions"),
          dv.labels= "Pitch Variability")

#mean pitch linear mixed effects model with just ADS utterances 

#create dataset that just includes ADS utterances - CHECK: do we need to 
ADScontexts <- subset(pitch, addressee == "ADS") 

#run same model as above except without interaction terms and only with categories > 20 observations 
mpitch_modelADS = lmer(log(m_pitch) ~ convo + inform + question + (1 | ID) + (1 | coder), data = ADScontexts)
summary(mpitch_modelADS)

#format regression table 
tab_model(mpitch_modelADS, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Conversational Basics","Inform",
                         "Questions"),
          dv.labels= "Mean Pitch")


#pitch variability linear mixed effects model with just ADS utterances 
sdpitch_modelADS = lmer(log(sd_pit) ~ convo + inform + question + (1 | ID) + (1 | coder), data = ADScontexts)
summary(sdpitch_modelADS)

#format regression table 
tab_model(sdpitch_modelADS, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Conversational Basics","Inform","Questions"),
          dv.labels= "Pitch Variability")

#mean pitch linear mixed effects model with just CDS utterances 

CDScontexts <- subset(pitch, addressee == "CDS") 

sum(CDScontexts[,8])
sum(CDScontexts[,9]) #exclude comfort 
sum(CDScontexts[,10]) #exclude sing
sum(CDScontexts[,11])
sum(CDScontexts[,12])
sum(CDScontexts[,13])
sum(CDScontexts[,14])
sum(CDScontexts[,15]) #exclude vocal play
sum(CDScontexts[,16]) #exclude noisy

mpitch_modelCDS = lmer(log(m_pitch) ~ convo + inform + read + imperative + question + (1 | ID) + (1 | coder), data = CDScontexts)
summary(mpitch_modelCDS)

tab_model(mpitch_modelCDS, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Conversational Basics","Inform", "Reading", "Imperative",
                         "Questions"),
          dv.labels= "Mean Pitch")


#pitch variability linear mixed effects model with just CDS utterances 
sdpitch_modelCDS = lmer(log(sd_pit) ~ convo + inform + read + imperative + question + (1 | ID) + (1 | coder), data = CDScontexts)
summary(sdpitch_modelCDS)

tab_model(sdpitch_modelCDS, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Conversational Basics","Inform", "Reading", "Imperative",
                         "Questions"),
          dv.labels= "Pitch Variability")


#logistic mixed effects model for addressee and context 


#renaming speech column in pitch_org  to addressee 
names(pitch_org)[names(pitch_org) == 'speech'] <- 'addressee'

#dummy coding CDS and ADS in pitch_org 
library(plyr)
pitch_org$cds <- revalue(pitch_org$addressee, c("CDS"="1", "ADS"="0"))
addresseecon <- as.factor(pitch_org$cds)

contextsbyadd = glmer(addresseecon ~ convo + comfort + sing + inform 
                      + question + vocalplay + noisy + (1 | ID), data = pitch_org, family=binomial)
summary(contextsbyadd)
tab_model(contextsbyadd, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Conversational Basics", "Comfort", "Singing", "Inform", "Questions",
                         "Vocal Play", "Noisy"),
          dv.labels= "Contexts by Addressee Model")

