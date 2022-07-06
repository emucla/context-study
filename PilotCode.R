setwd("~/Desktop/")

library(tidyverse)
library(lme4)
library(lmerTest)
library(ggplot2)
library(sjPlot)
#read in the data 
pitch_org <- read.table('recode.txt', header=T)

# exclude rows in which rows look like this: columns 8-15 are coded as 0 AND 16 is coded as 1 

pitch <- pitch_org[!(!apply(pitch_org[,8:15], 1,any) & pitch_org[,16] == 1),]

#checking distribution of mean pitch and pitch variability - raw value of pitch 

hist(pitch$m_pitch)
hist(pitch$sd_pitch)

#checking distribution of mean pitch and pitch variability - log pitch
logp<- log(pitch$m_pitch)
hist(logp) 

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

#excluding the above categories
pitch2 = subset(pitch, select = -c(comfort, sing, vocalplay, noisy, read))
newdata <- pitch2[which(pitch2$speech=='CDS'),]

##----------------- Proportion of Context Occurrences Logistic Mixed Effects Model ---------###

library(plyr)
pitch2$cds <- revalue(pitch2$speech, c("CDS"="1", "ADS"="0"))
speechcon <- as.factor(pitch2$cds)
prop <- glmer(speechcon ~ convo + inform + imperative + question + (1 | ID), data = pitch2, family=binomial)
summary(prop)
tab_model(prop, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "convo", "inform", "imperative", "question"),
          dv.labels= "Proportion")

convocount <- tapply(pitch2$convo, pitch2$speech, count)
informcount <- tapply(pitch2$inform, pitch2$speech, count)
impcount <- tapply(pitch2$imperative, pitch2$speech, count)
questcount <- tapply(pitch2$question, pitch2$speech, count)

##----------------- Proportion of Context Occurrences Bar Plot ---------###
#reading in datafile with proportion of occurrences
#proportions calculated by taking number of occurrences in IDS or ADS/total IDS or ADS occurrences
prop <- read.table('prop.txt', header=T)

#ggplot of proportion of occurrences
prop %>% 
  mutate(context = factor(context, labels = c("Conversational\n Basics","Imperative", "Inform", "Questions"))) %>%
  ggplot(aes(x= context, y= prop, fill= addressee)) + 
  geom_bar(stat='identity', position ="dodge")+ 
  labs(x = "Context", y = "Proportion of Utterances per Addressee", fill = "Addressee")+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .2)) +
  theme_bw()+
  theme(text = element_text(face = "bold", size = 12)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,face = "bold", size = 12))+
  scale_fill_manual(values=c( "#56B4E9","#E69F00"))

###----------------- Mean Pitch and Pitch Variability Mixed Effects Models by Context ---------###

meanpit = lmer(log(m_pitch) ~ convo + inform + imperative + question + (1 | ID) + (1 | coder), data = pitch2)
summary(meanpit)
tab_model(meanpit, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Conversational Basics", "Inform", "Imperative", "Question"),
          dv.labels= "Mean Pitch")
#Convo- MEAN PITCH
convodata <- pitch2[ which(pitch2$convo==1),]

mpitchconvo = lmer(log(m_pitch) ~ speech + (1 | ID) + (1 | coder), data = convodata)
summary(mpitchconvo)
tab_model(mpitchconvo, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Conversational Basics"),
          dv.labels= "Mean Pitch")

#Convo- VARIABILITY 
sdpitchconvo = lmer(log(sd_pit) ~ speech + (1 | ID) + (1 | coder), data = convodata)
summary(sdpitchconvo)
tab_model(sdpitchconvo, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Conversational Basics"),
          dv.labels= "Pitch Variability")


#Inform _ MEAN PITCH
informdata <- pitch2[ which(pitch2$inform==1),]
informdata2 = lmer(log(m_pitch) ~ speech + (1 | ID) + (1 | coder), data = informdata)
summary(informdata2)
tab_model(informdata2, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Inform"),
          dv.labels= "Mean Pitch")
#Inform -VARIABILITY 
sdpitchinform = lmer(log(sd_pit) ~ speech + (1 | ID) + (1 | coder), data = informdata)
summary(sdpitchinform)
tab_model(sdpitchinform, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Inform"),
          dv.labels= "Pitch Variability")

#imperative - MEAN PITCH
imp <- pitch2[ which(pitch2$imperative==1),]
impdat = lmer(log(m_pitch) ~ speech + (1 | ID) + (1 | coder), data = imp)
tab_model(impdat, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Imperative"),
          dv.labels= "Mean Pitch")

#imperative - VARIABILITY 
sdpitchimp = lmer(log(sd_pit) ~ speech + (1 | ID) + (1 | coder), data = imp)
summary(sdpitchimp)
tab_model(sdpitchimp, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Imperative"),
          dv.labels= "Pitch Variability")

#Qusetions - MEAN PITCH
quest <- pitch2[ which(pitch2$question==1),]
questdat = lmer(log(m_pitch) ~ speech + (1 | ID) + (1 | coder), data = quest)
summary(questdat)
tab_model(questdat, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Question"),
          dv.labels= "Mean Pitch")

#Qusetions- VARIABILITY 
sdpitchquest = lmer(log(sd_pit) ~ speech + (1 | ID) + (1 | coder), data = quest)
summary(sdpitchquest)
tab_model(sdpitchquest, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Questions"),
          dv.labels= "Pitch Variability")

###----------------- Mean Pitch and Pitch Variability Violin Plots ---------###
#read in re-formatted data for violin plot 
data <- read.table('violin.txt', header=T)

#mean pitch model vilion plot 
data %>% 
  group_by(context,speech) %>% # looking for outliers within each group (not overall - comment out if you just want to look for outliers in entire dataset)
  mutate(outlier = case_when(is_outlier(logpit, coef=3) ~ 1,  # checking using boxplot method - identifying points that are above Q3 + 1.5xIQR or below Q1 - 1.5xIQR 
                             TRUE ~0)) %>%
  # if you want to remove only EXTREME outliers, use code is_outlier(logpit, coef = 3)
  ungroup() %>% 
  #filter(outlier == 0 ) %>% # comment out this line if you want to put outliers back in 
  mutate(context = factor(context, labels = c("Conversational\n Basics","Imperative", "Inform", "Questions"))) %>%
  ggplot(aes(x= context, y= logpit, fill= speech)) + 
  geom_violin()+ 
  geom_point(aes(fill=speech), colour = "white",pch=21, position=position_dodge(width=0.9)) +
  labs(x = "Context", y = "Mean Pitch", fill = "Addressee")+
  theme_bw()+
  theme(text = element_text(face = "bold", size = 12)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,face = "bold", size = 12))+
  scale_fill_manual(values=c( "#56B4E9","#E69F00")) # just change the hex code for the specific color you're looking for - google for color hex codes

#pitch variability model violin plot
data %>% 
  group_by(context,speech) %>% # looking for outliers within each group (not overall - comment out if you just want to look for outliers in entire dataset)
  mutate(outlier = case_when(is_outlier(log_sd, coef=3) ~ 1,  # checking using boxplot method - identifying points that are above Q3 + 1.5xIQR or below Q1 - 1.5xIQR 
                             TRUE ~0)) %>%
  # if you want to remove only EXTREME outliers, use code is_outlier(logpit, coef = 3)
  ungroup() %>% 
  #filter(outlier == 0 ) %>% # comment out this line if you want to put outliers back in 
  mutate(context = factor(context, labels = c("Conversational\n Basics","Imperative", "Inform", "Questions"))) %>%
  ggplot(aes(x= context, y= log_sd, fill= speech)) + 
  geom_violin()+ 
  geom_point(aes(fill=speech), colour = "white",pch=21, position=position_dodge(width=0.9)) +
  labs(x = "Context", y = "Pitch Variability", fill = "Addressee")+
  theme_bw()+
  theme(text = element_text(face = "bold", size = 12)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,face = "bold", size = 12))+
  
  scale_fill_manual(values=c( "#56B4E9","#E69F00")) # just change the hex code for the specific color you're looking for - google for color hex codes

#bar plot for context frequencies 

count(data$context)
countprop <- tapply(data$context, data$speech, count)


data %>% 
  group_by(context,speech) %>% # looking for outliers within each group (not overall - comment    out if you just want to look for outliers in entire dataset)
  mutate(outlier = case_when(is_outlier(log_sd, coef=3) ~ 1,  # checking using boxplot method - identifying points that are above Q3 + 1.5xIQR or below Q1 - 1.5xIQR 
                             TRUE ~0)) %>%
  # if you want to remove only EXTREME outliers, use code is_outlier(logpit, coef = 3)
  ungroup() %>% 
  #filter(outlier == 0 ) %>% # comment out this line if you want to put outliers back in 
  mutate(context = factor(context, labels = c("Conversational\n Basics","Imperative", "Inform", "Questions"))) %>%
  ggplot(aes(x= context, y= log_sd, fill= speech)) + 
  geom_bar()+ 
  labs(x = "Context", y = "Pitch Variability", fill = "Addressee")+
  theme_bw()+
  theme(text = element_text(face = "bold", size = 12)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,face = "bold", size = 12))+
  scale_fill_manual(values=c( "#56B4E9","#E69F00"))

