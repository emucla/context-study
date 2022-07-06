setwd("~/Desktop/")

library(tidyverse)
library(lme4)
library(lmerTest)

#read in the data 
pitch_org <- read.table('pitchpilot.txt', header=T)

# exclude rows in which rows look like this: columns 8-15 are coded as 0 AND 16 is coded as 1 

pitch <- pitch_org[!(!apply(pitch_org[,8:15], 1,any) & pitch_org[,16] == 1),]

#renaming speech column to addressee 
names(pitch)[names(pitch) == 'speech'] <- 'addressee'

#checking distribution of mean pitch and pitch variability - raw value of pitch 

hist(pitch$m_pitch)
hist(pitch$sd_pitch)

#checking distribution of mean pitch and pitch variability - log pitch
logp<- log(pitch$m_pitch)
hist(logp) 

logsd<- log(pitch$sd_pit)
hist(logsd) 

###-----------Hypothesis 1: Comfort IDS v other IDS contexts - linear mixed effects regression-----------###

#subset data to get IDS addressee utterances only 
IDScom <- subset(pitch, addressee == "CDS")

#run the linear mixed effects regression - MEAN PITCH
mpitch_comfort = lmer(log(m_pitch) ~ comfort + (1 | ID), data = IDScom)
summary(mpitch_comfort)

#create regression table
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


###---------Hypothesis 2: IDS v ADS inform utterances- linear mixed effects regression----------#### 
inform <- subset(pitch, inform =="1")

#linear mixed effects regression - MEAN PITCH
mpitch_inform = lmer(log(m_pitch) ~ addressee + (1 | ID), data = inform)
summary(mpitch_inform)

#create regression table 
tab_model(mpitch_inform, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "addressee"),
          dv.labels= "log(Mean Pitch)")

#linear mixed effects regression - PITCH VARIABILITY
sdpit_inform = lmer(log(sd_pit) ~ addressee + (1 | ID), data = inform)
summary(sdpit_inform)

tab_model(sdpit_inform, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "addressee"),
          dv.labels= "Pitch Variablity")


###----------Hypothesis 3: Frequency of comfort utterances in IDS v ADS -mixed effects logistic regression---------###

#dummy code ADS and CDS 
library(plyr)
pitch$cds <- revalue(pitch$addressee, c("CDS"="1", "ADS"="0"))
addressee_com <- as.factor(pitch$cds)

#Logistic regression for comfort utterance frequency by addressee

addressee_comfort = glmer(comfort ~ pitch$cds + (1 | ID), data = pitch, family=binomial)
summary(addressee_comfort)
tab_model(addressee_comfort, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Comfort"),
          dv.labels= "Comfort Utterances in IDS v ADS")

###------------Exploratory Analyses-----------------###

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

###---------Linear Mixed Effects Model for Mean Pitch and Pitch Variability - ADS Utterances ONLY----------####

ADScontexts <- subset(pitch, addressee == "ADS") 

mpitch_modelADS = lmer(log(m_pitch) ~ convo + inform + question + (1 | ID) + (1 | coder), data = ADScontexts)
summary(mpitch_modelADS)
tab_model(mpitch_modelADS, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Conversational Basics","Inform",
                         "Questions"),
          dv.labels= "Mean Pitch")

sdpitch_modelADS = lmer(log(sd_pit) ~ convo + inform + question + (1 | ID) + (1 | coder), data = ADScontexts)
summary(sdpitch_modelADS)
tab_model(sdpitch_modelADS, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Conversational Basics","Inform","Questions"),
          dv.labels= "Pitch Variability")

###---------Linear Mixed Effects Model for Mean Pitch and Pitch Variability - CDS Utterances ONLY---------####

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

sdpitch_modelCDS = lmer(log(sd_pit) ~ convo + inform + read + imperative + question + (1 | ID) + (1 | coder), data = CDScontexts)
summary(sdpitch_modelCDS)

tab_model(sdpitch_modelCDS, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Conversational Basics","Inform", "Reading", "Imperative",
                         "Questions"),
          dv.labels= "Pitch Variability")


####----------Context by Addressee Model------------#### 

#renaming speech column in pitch_org  to addressee 
names(pitch)[names(pitch) == 'speech'] <- 'addressee'

#dummy coding IDS and ADS in pitch_org 
library(plyr)
pitch$cds <- revalue(pitch$addressee, c("CDS"="1", "ADS"="0"))
addresseecon <- as.factor(pitch$cds)

contextsbyadd = glmer(addresseecon ~ convo + comfort + sing + inform 
                      + question + vocalplay + noisy + (1 | ID), data = pitch,family=binomial)
summary(contextsbyadd)
tab_model(contextsbyadd, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Conversational Basics", "Comfort", "Singing", "Inform", "Questions",
                         "Vocal Play", "Noisy"),
          dv.labels= "Contexts by Addressee Model")

meanpit = lmer(log(m_pitch) ~ convo + inform + imperative + question + (1 | ID) + (1 | coder), data = pitch)
summary(meanpit)
tab_model(meanpit, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Conversational Basics", "Inform", "Imperative", "Question"),
          dv.labels= "Mean Pitch")

###----------------- Mean Pitch and Pitch Variability Mixed Effects Models by Context ---------###

#Convo- MEAN PITCH
convodata <- pitch[ which(pitch$convo==1),]

mpitchconvo = lmer(log(m_pitch) ~ addressee + (1 | ID) + (1 | coder), data = convodata)
summary(mpitchconvo)
tab_model(mpitchconvo, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Conversational Basics"),
          dv.labels= "Mean Pitch")

#Convo- VARIABILITY 
sdpitchconvo = lmer(log(sd_pit) ~ addressee + (1 | ID) + (1 | coder), data = convodata)
summary(sdpitchconvo)
tab_model(sdpitchconvo, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Conversational Basics"),
          dv.labels= "Pitch Variability")

#Comfort- MEAN PITCH
comfortdata <- pitch[ which(pitch$comfort==1),]

mpitchcomfort = lmer(log(m_pitch) ~ addressee + (1 | ID) + (1 | coder), data = comfortdata)
summary(mpitchcomfort)
tab_model(mpitchcomfort, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Comfort"),
          dv.labels= "Mean Pitch")

#Comfort- VARIABILITY 
sdpitchcomfort = lmer(log(sd_pit) ~ addressee + (1 | ID) + (1 | coder), data = comfortdata)
summary(sdpitchcomfort)
tab_model(sdpitchcomfort, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Comfort"),
          dv.labels= "Pitch Variability")

#Singing- MEAN PITCH
singdata <- pitch[ which(pitch$sing==1),]

mpitchsing = lmer(log(m_pitch) ~ addressee + (1 | ID) + (1 | coder), data = singdata)
summary(mpitchsing)
tab_model(mpitchsing, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Sing"),
          dv.labels= "Mean Pitch")

#Singing- VARIABILITY 
sdpitchsing = lmer(log(sd_pit) ~ addressee + (1 | ID) + (1 | coder), data = singdata)
summary(sdpitchsing)
tab_model(sdpitchsing, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Sing"),
          dv.labels= "Pitch Variability")

#Inform _ MEAN PITCH
informdata <- pitch[ which(pitch$inform==1),]
informdata2 = lmer(log(m_pitch) ~ addressee + (1 | ID) + (1 | coder), data = informdata)
summary(informdata2)
tab_model(informdata2, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Inform"),
          dv.labels= "Mean Pitch")

#Inform -VARIABILITY 
sdpitchinform = lmer(log(sd_pit) ~ addressee + (1 | ID) + (1 | coder), data = informdata)
summary(sdpitchinform)
tab_model(sdpitchinform, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Inform"),
          dv.labels= "Pitch Variability")

#imperative - MEAN PITCH
imp <- pitch[ which(pitch$imperative==1),]
impdat = lmer(log(m_pitch) ~ addressee + (1 | ID) + (1 | coder), data = imp)
tab_model(impdat, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Imperative"),
          dv.labels= "Mean Pitch")

#imperative - VARIABILITY 
sdpitchimp = lmer(log(sd_pit) ~ addressee + (1 | ID) + (1 | coder), data = imp)
summary(sdpitchimp)
tab_model(sdpitchimp, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Imperative"),
          dv.labels= "Pitch Variability")

#Qusetions - MEAN PITCH
quest <- pitch[ which(pitch$question==1),]
questdat = lmer(log(m_pitch) ~ addressee + (1 | ID) + (1 | coder), data = quest)
summary(questdat)
tab_model(questdat, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Question"),
          dv.labels= "Mean Pitch")

#Qusetions- VARIABILITY 
sdpitchquest = lmer(log(sd_pit) ~ addressee + (1 | ID) + (1 | coder), data = quest)
summary(sdpitchquest)
tab_model(sdpitchquest, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Questions"),
          dv.labels= "Pitch Variability")

#Vocal Play - MEAN PITCH
vpdata <- pitch[ which(pitch$question==1),]
vpdat = lmer(log(m_pitch) ~ addressee + (1 | ID) + (1 | coder), data = vpdata)
summary(vpdat)
tab_model(vpdat, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Vocal Play"),
          dv.labels= "Mean Pitch")

#Vocal Play- VARIABILITY 
sdpitchvp = lmer(log(sd_pit) ~ addressee + (1 | ID) + (1 | coder), data = vpdata)
summary(sdpitchvp)
tab_model(sdpitchvp, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Vocal Play"),
          dv.labels= "Pitch Variability")

