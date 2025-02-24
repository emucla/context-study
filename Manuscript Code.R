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
library(broom.mixed)
library(brms)

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

#get the counts for each pragmatic category, adult gender, and register
ADS_male <- subset(pitch, adu_gender == 1 & register == 0)
sum(ADS_male[,8]) #convo - 74
sum(ADS_male[,9]) #comfort - 0
sum(ADS_male[,10]) #sing - 1
sum(ADS_male[,11]) #inform - 252
sum(ADS_male[,12]) #reading - 0
sum(ADS_male[,13]) #imperative - 8
sum(ADS_male[,14]) #question - 58
sum(ADS_male[,15]) #Vocal play - 0

IDS_male <- subset(pitch, adu_gender == 1 & register == 1)
sum(IDS_male[,8]) #convo - 111
sum(IDS_male[,9]) #comfort - 21
sum(IDS_male[,10]) #sing - 65
sum(IDS_male[,11]) #inform - 248
sum(IDS_male[,12]) #reading - 63
sum(IDS_male[,13]) #imperative - 80
sum(IDS_male[,14]) #question - 132
sum(IDS_male[,15]) #Vocal play - 12

ADS_female <- subset(pitch, adu_gender == 0 & register == 0)
sum(ADS_female[,8]) #convo - 252
sum(ADS_female[,9]) #comfort - 2
sum(ADS_female[,10]) #sing - 1
sum(ADS_female[,11]) #inform - 888
sum(ADS_female[,12]) #reading - 0
sum(ADS_female[,13]) #imperative - 35
sum(ADS_female[,14]) #question - 154
sum(ADS_female[,15]) #Vocal play - 0

IDS_female <- subset(pitch, adu_gender == 0 & register == 1)
sum(IDS_female[,8]) #convo - 248
sum(IDS_female[,9]) #comfort - 19
sum(IDS_female[,10]) #sing - 159
sum(IDS_female[,11]) #inform - 652
sum(IDS_female[,12]) #reading - 169
sum(IDS_female[,13]) #imperative - 269
sum(IDS_female[,14]) #question - 401
sum(IDS_female[,15]) #Vocal play - 25

sum(pitch[,8]) #convo - 685
sum(pitch[,9]) #comfort - 42
sum(pitch[,10]) #sing - 226
sum(pitch[,11]) #inform - 2040
sum(pitch[,12]) #reading - 232
sum(pitch[,13]) #imperative - 392
sum(pitch[,14]) #question - 745
sum(pitch[,15]) #Vocal play - 37
sum(pitch[,16]) #noisy - 69

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

#-----------------------frequency model-----------------------------------------------------------------# 

# bayesmodel <- brm(register ~ adu_gender*convo + adu_gender*comfort + adu_gender*sing + adu_gender*inform + adu_gender*imperative + adu_gender*question + adu_gender*read + adu_gender*vocalplay + (1|coder)+ (1|ID),
#                   data = pitch,
#                   family = bernoulli(link="logit"))
# summary(bayes_model)
# 
# bayesmodel_enhanced <- brm(register ~ adu_gender*convo + adu_gender*comfort + adu_gender*sing + adu_gender*inform + adu_gender*imperative + adu_gender*question + adu_gender*read + adu_gender*vocalplay + (1|coder)+ (1|ID),
#                   data = pitch,
#                   family = bernoulli(link="logit"),
#                   iter = 10000,
#                   control=list(adapt_delta=0.9))
# summary(bayesmodel_enhanced)
# 
# bayesmodel_contextsonly <- brm(register ~ convo + comfort + sing + inform + imperative + question + read + vocalplay + (1|coder)+ (1|ID),
#                   data = pitch,
#                   family = bernoulli(link="logit"))
# summary(bayesmodel_contextsonly)
# 
# bayesmodel_contextsonly_enhanced <- brm(register ~ convo + comfort + sing + inform + imperative + question + read + vocalplay + (1|coder)+ (1|ID),
#                                          data = pitch,
#                                          family = bernoulli(link="logit"),
#                                          iter = 10000,
#                                          chains = 10,
#                                          control=list(adapt_delta=0.9,max_treedepth=12))
# summary(bayesmodel_contextsonly_enhanced)

bayesmodel_contextsonly_enhanced2 <- brm(register ~ convo + comfort + sing + inform + imperative + question + read + vocalplay + (1|coder)+ (1|ID),
                                        data = pitch,
                                        family = bernoulli(link="logit"),
                                        iter = 10000,
                                        chains = 10,
                                        control=list(adapt_delta=0.9,max_treedepth=15))
summary(bayesmodel_contextsonly_enhanced2)

#------------- Acoustic Analysis 1: Adult Speaker Gender and Register Analysis (4 contexts)-------------#

#log transform pitch variables 
pitch[,3:7] <- log(pitch[,3:7])

#center and scale log-transformed pitch variables 
pitch[,3:7] <- scale(pitch[,3:7], center = TRUE, scale = TRUE)

#center and scale register 
pitch[,2] <- scale(pitch[,2], center = TRUE, scale = TRUE)

#subset data for just inform utterances
informdat <- pitch[which(pitch$inform > 0),]

#mean pitch inform model
mpitchinform_int = lmer(m_pitch ~ adu_gender*register + (1 | ID) + (1 | coder), data = informdat)

summary(mpitchinform_int)
confint(mpitchinform_int) #check for tab_model outputs

tab_model(mpitchinform_int, show.re.var= TRUE, show.se = TRUE, df.method="satterthwaite", file = "inform.html",
          pred.labels =c("(Intercept)", "Adult Gender", "Register", "Adult Gender*Register"),
          dv.labels= "Mean Pitch")

#mean pitch inform post-hoc pairwise comparisons - revpairwise for register contrast 

inform_register <- as.data.frame(emmeans(mpitchinform_int, ~ adu_gender * register ) %>%
  contrast("pairwise", by = c( "register")) %>% 
  summary(by = NULL, adjust = "holm", infer =c(TRUE, TRUE)))

#reverse pairwise contrast so the estimate reflects ADS as second
inform_adu <- as.data.frame(emmeans(mpitchinform_int, ~ adu_gender * register ) %>%
  contrast("revpairwise", by = c( "adu_gender")) %>% 
  summary(by = NULL, adjust = "holm",infer =c(TRUE, TRUE)))

#interaction plots 
result <- ggemmeans(mpitchinform_int, c("register", "adu_gender"))
plot(result) + 
  labs(
    x = "Register",
    y = "Mean Pitch",
    title = "Mean Pitch of Register by Adult Speaker Gender",
    colour = "Adult Speaker Gender") +
  scale_x_continuous(breaks = -0.5:0.5, limits = c(-1.5, 1.5))
ggsave("inform_meanpitch_register.svg")

result2 <- ggemmeans(mpitchinform_int, c("adu_gender", "register"))
plot(result2) + 
  labs(
    x = "Adult Speaker Gender",
    y = "Mean Pitch",
    title = "Mean Pitch of Adult Speaker Gender by Register",
    colour = "Register") +
  scale_x_continuous(breaks = -0.5:1.5, limits = c(-1.0, 2.0))
ggsave("inform_meanpitch_gender.svg")


#inform pitch variability 
sdpitchinform_int = lmer(sd_pit ~ adu_gender*register + (1 | ID) + (1 | coder), data = informdat)
summary(sdpitchinform_int)
confint(sdpitchinform_int)

tab_model(sdpitchinform_int, show.re.var= TRUE, show.se = TRUE, df.method="satterthwaite", file = "informvar.html",
          pred.labels =c("(Intercept)", "Adult Gender", "Register", "Adult Gender*Register"),
          dv.labels= "Pitch Variability")

#subset data for just convo utterances
convodat <- pitch[which(pitch$convo > 0),]

mpitchconvo_int = lmer(m_pitch ~ adu_gender*register + (1 | ID) + (1 | coder), data = convodat)
summary(mpitchconvo_int)
confint(mpitchconvo_int)

tab_model(mpitchconvo_int, show.re.var= TRUE, show.se = TRUE, df.method="satterthwaite", file = "convomean.html",
          pred.labels =c("(Intercept)", "Adult Gender", "Register", "Adult Gender*Register"),
          dv.labels= "Mean Pitch")

#post-hoc comparisons 
emmeans(mpitchconvo_int, ~ adu_gender * register ) %>%
  contrast("pairwise", by = c( "register")) %>% 
  summary(by = NULL, adjust = "holm", infer =c(TRUE, TRUE))

#post-hoc comparisons 
emmeans(mpitchconvo_int, ~ adu_gender * register ) %>%
  contrast("revpairwise", by = c( "adu_gender")) %>% 
  summary(by = NULL, adjust = "holm", infer =c(TRUE, TRUE))

#interaction plots 
resultconvo <- ggemmeans(mpitchconvo_int, c("register", "adu_gender"))
plot(resultconvo) + 
  labs(
    x = "Register",
    y = "Mean Pitch",
    title = "Mean Pitch of Register by Adult Speaker Gender",
    colour = "Adult Speaker Gender") +
  scale_x_continuous(breaks = -0.5:0.5, limits = c(-1.5, 1.5))
ggsave("convo_meanpitch_register.svg")

resultconvo2 <- ggemmeans(mpitchconvo_int, c("adu_gender", "register"))
plot(resultconvo2) + 
  labs(
    x = "Adult Speaker Gender",
    y = "Mean Pitch",
    title = "Mean Pitch of Adult Speaker Gender by Register",
    colour = "Register") +
  scale_x_continuous(breaks = -0.5:1.5, limits = c(-1.0, 2.0))
ggsave("convo_meanpitch_gender.svg")

#convo pitch variability model 
sdpitchconvo_int = lmer(sd_pit ~ adu_gender + register + adu_gender*register + (1 | ID) + (1 | coder), data = convodat)
summary(sdpitchconvo_int)
confint(sdpitchconvo_int)

tab_model(sdpitchconvo_int, show.re.var= TRUE, show.se = TRUE, df.method="satterthwaite", file = "convovar.html",
          pred.labels =c("(Intercept)", "Adult Gender", "Register", "Adult Gender*Register"),
          dv.labels= "Pitch Variability")

#subset data for just question utterances
questdat <- pitch[which(pitch$question > 0),]
 
mpitchq = lmer(m_pitch ~ adu_gender + register + adu_gender*register + (1 | ID) + (1 | coder), data = questdat)
summary(mpitchq)
confint(mpitchq)

tab_model(mpitchq, show.re.var= TRUE, show.se = TRUE, df.method="satterthwaite", file = "questmean.html",
          pred.labels =c("(Intercept)", "Adult Gender", "Register", "Adult Gender*Register"),
          dv.labels= "Mean Pitch")

 #questions pitch variability
 sdpitchq_int = lmer(sd_pit ~ adu_gender + register + adu_gender*register +(1 | ID) + (1 | coder), data = questdat)
 summary(sdpitchq_int)
 confint(sdpitchq_int)
 
 tab_model(sdpitchq_int, show.re.var= TRUE, show.se = TRUE, df.method="satterthwaite", file = "questvar.html",
           pred.labels =c("(Intercept)", "Adult Gender", "Register", "Adult Gender*Register"),
           dv.labels= "Pitch Variability")

#subset data for just imperative utterances
impdat <- pitch[which(pitch$imperative > 0),]

mpitchimp = lmer(m_pitch ~ adu_gender + register + adu_gender*register + (1 | ID) + (1 | coder), data = impdat)
summary(mpitchimp)
confint(mpitchimp)

tab_model(mpitchimp, show.re.var= TRUE, show.se = TRUE, df.method="satterthwaite", file = "impmean.html",
          pred.labels =c("(Intercept)", "Adult Gender", "Register", "Adult Gender*Register"),
          dv.labels= "Mean Pitch")

#imperative pitch variability 
sdpitchimp_int = lmer(sd_pit ~ adu_gender + register + adu_gender*register + (1 | ID) + (1 | coder), data = impdat)
summary(sdpitchimp_int)
confint(sdpitchimp_int)

tab_model(sdpitchimp_int, show.re.var= TRUE, show.se = TRUE, df.method="satterthwaite", file = "impvar.html",
          pred.labels =c("(Intercept)", "Adult Gender", "Register", "Adult Gender*Register"),
          dv.labels= "Pitch Variability")


#---------Adult Speaker Gender and Pragmatic Context in IDS -----------------#
#subset IDS data 
IDS_data <- pitch[which(pitch$register > 0),]

IDSmeanpitch = lmer(m_pitch ~ adu_gender*convo + adu_gender*sing + adu_gender*inform + adu_gender*read + adu_gender*imperative + adu_gender*question + adu_gender*comfort +(1|ID) + (1 | coder), data = IDS_data)
summary(IDSmeanpitch)
confint(IDSmeanpitch)

tab_model(IDSmeanpitch, show.re.var= TRUE, show.se = TRUE, df.method="satterthwaite", file = "IDSmean.html",
          pred.labels =c("(Intercept)", "Adult Gender", "Conversational Basics", "Singing","Inform", "Reading", "Imperative", "Questions",
                         "Comfort", "Adult Gender*Conversational Basics","Adult Gender*Singing","Adult Gender*Inform", "Adult Gender*Reading",
                         "Adult Gender*Imperative", "Adult Gender*Questions", "Adult Gender*Comfort"),
          dv.labels= "Mean Pitch")

#pairwise comparisons for gender and sing interaction 
emmeans(IDSmeanpitch, ~ adu_gender*sing ) %>%
  contrast("revpairwise", by = c("adu_gender")) %>% 
  summary(by = NULL, adjust = "holm", infer =c(TRUE, TRUE))

emmeans(IDSmeanpitch, ~ adu_gender*sing ) %>%
  contrast("pairwise", by = c("sing")) %>% 
  summary(by = NULL, adjust = "holm", infer =c(TRUE, TRUE))

#IDS mean pitch - singing and adult speaker gender interaction plots 
IDSsing <- ggemmeans(IDSmeanpitch, c("sing", "adu_gender"))
plot(IDSsing ) + 
  labs(
    x = "Pragmatic Context",
    y = "Mean Pitch",
    title = "Mean Pitch of Context by Adult Speaker Gender",
    colour = "Adult Speaker Gender")
ggsave("IDSsing_meanpitch_gender.svg")

IDSsing2 <- ggemmeans(IDSmeanpitch, c("adu_gender", "sing"))
plot(IDSsing2) + 
  labs(
    x = "Adult Speaker Gender",
    y = "Mean Pitch",
    title = "Mean Pitch of Adult Speaker Gender by Context",
    colour = "Context") 
  #scale_x_continuous(breaks = -0.5:1.5, limits = c(-1.0, 2.0))
ggsave("IDSsing_meanpitch_context.svg")


#pitch variability for IDS 
IDSsdpitch = lmer(sd_pit ~ adu_gender*convo + adu_gender*sing + adu_gender*inform + adu_gender*read + adu_gender*imperative + adu_gender*question + adu_gender*comfort +(1|ID) + (1 | coder), data = IDS_data)
summary(IDSsdpitch)
confint(IDSsdpitch)

tab_model(IDSsdpitch, show.re.var= TRUE, show.se = TRUE, df.method="satterthwaite", file = "IDSsd.html",
          pred.labels =c("(Intercept)", "Adult Gender", "Conversational Basics", "Singing","Inform", "Reading", "Imperative", "Questions",
                         "Comfort", "Adult Gender*Conversational Basics","Adult Gender*Singing","Adult Gender*Inform", "Adult Gender*Reading",
                         "Adult Gender*Imperative", "Adult Gender*Questions", "Adult Gender*Comfort"),
          dv.labels= "Pitch Variability")

#pairwise comparisions for gender and sing interaction 
emmeans(IDSsdpitch, ~ adu_gender*sing ) %>%
  contrast("revpairwise", by = c("adu_gender")) %>% 
  summary(by = NULL, adjust = "holm", infer =c(TRUE, TRUE))

emmeans(IDSsdpitch, ~ adu_gender*sing ) %>%
  contrast("pairwise", by = c("sing")) %>% 
  summary(by = NULL, adjust = "holm", infer =c(TRUE, TRUE))

#interaction plots for singing and variability 

IDSsingvar <- ggemmeans(IDSsdpitch, c("sing", "adu_gender"))
plot(IDSsingvar) + 
  labs(
    x = "Pragmatic Context",
    y = "Pitch Variability",
    title = "Pitch Variability of Context by Adult Speaker Gender",
    colour = "Adult Speaker Gender")
ggsave("IDSsing_var_gender.svg")

IDSsingvar2 <- ggemmeans(IDSsdpitch, c("adu_gender", "sing"))
plot(IDSsingvar2) + 
  labs(
    x = "Adult Speaker Gender",
    y = "Pitch Variability",
    title = "Pitch Variability of Adult Speaker Gender by Context",
    colour = "Context") 
#scale_x_continuous(breaks = -0.5:1.5, limits = c(-1.0, 2.0))
ggsave("IDSsing_var_context.svg")

##########################Adult Speaker Gender and Infant Gender in IDS#################################### 

#inform
informIDSdat <-informdat[which(informdat$register > 0),]

mpitchinformIDS = lmer(m_pitch ~ adu_gender*chi_gender + (1 | ID)+ (1|coder), data = informIDSdat)
summary(mpitchinformIDS)
confint(mpitchinformIDS)

tab_model(mpitchinformIDS, show.re.var= TRUE, show.se = TRUE, df.method="satterthwaite", file = "inform_mean_infant.html",
          pred.labels =c("(Intercept)", "Adult Gender", "Infant Gender", "Adult Gender*Infant Gender"),
          dv.labels= "Mean Pitch")

#pitch variability for inform IDS 
sdpitchinformIDS = lmer(sd_pit ~ adu_gender*chi_gender + (1 | ID)+ (1|coder), data = informIDSdat)
summary(sdpitchinformIDS)
confint(sdpitchinformIDS)

tab_model(sdpitchinformIDS, show.re.var= TRUE, show.se = TRUE, df.method="satterthwaite", file = "inform_var_infant.html",
          pred.labels =c("(Intercept)", "Adult Gender", "Infant Gender", "Adult Gender*Infant Gender"),
          dv.labels= "Pitch Variability")

#follow up on interaction 

emmeans(sdpitchinformIDS, ~ adu_gender * chi_gender ) %>%
  contrast("pairwise", by = c( "chi_gender")) %>% 
  summary(by = NULL, adjust = "holm", infer =c(TRUE, TRUE))

emmeans(sdpitchinformIDS, ~ adu_gender * chi_gender ) %>%
  contrast("pairwise", by = c( "adu_gender")) %>% 
  summary(by = NULL, adjust = "holm",infer =c(TRUE, TRUE))

#estimate and line plot for inform standard deviation of pitch (Fig 3) 
p<-plot_model(sdpitchinformIDS, type = "int", terms = c(chi_gender,adu_gender), ci.lvl = 0.95)
p+
  labs(x = "Adult Speaker Gender", y = "Predicted Standard Deviation of Pitch ") +
  scale_y_continuous(limits = c(-0.45, 0.6))+
  scale_x_discrete(limits=c( 0, 1), labels=c("Female", "Male"))+
  scale_color_manual(values = c( "#CC5500","#702963"))+
  theme_bw() 
ggsave("plotinform2.svg")

#interaction plots for adult gender and infant gender 

adu_in_inform <- ggemmeans(sdpitchinformIDS, c("chi_gender", "adu_gender"))
plot(adu_in_inform) + 
  labs(
    x = "Infant Gender",
    y = "Pitch Variability",
    title = "Pitch Variability of Infant Gender by Adult Speaker Gender",
    colour = "Adult Speaker Gender")
ggsave("adult_var_inform_infant.svg")

adu_in_inform2 <- ggemmeans(sdpitchinformIDS, c("adu_gender", "chi_gender"))
plot(adu_in_inform2 ) + 
  labs(
    x = "Adult Speaker Gender",
    y = "Pitch Variability",
    title = "Pitch Variability of Infant Gender by Adult Speaker Gender",
    colour = "Infant Gender") 
ggsave("adult_var_inform_infant2.svg")

#conversational basics
convoIDSdat <- convodat[which(convodat$register > 0),]

mpitchconvo_inf = lmer(m_pitch ~ adu_gender*chi_gender + (1 | ID) + (1|coder), data = convoIDSdat)
summary(mpitchconvo_inf)
confint(mpitchconvo_inf)

tab_model(mpitchconvo_inf, show.re.var= TRUE, show.se = TRUE, df.method="satterthwaite", file = "convo_mean_infant.html",
          pred.labels =c("(Intercept)", "Adult Gender", "Infant Gender", "Adult Gender*Infant Gender"),
          dv.labels= "Mean Pitch")

sdpitchconvo_inf = lmer(sd_pit ~ adu_gender*chi_gender + (1 | ID)+ (1|coder), data = convoIDSdat)
summary(sdpitchconvo_inf)
confint(sdpitchconvo_inf)

tab_model(sdpitchconvo_inf, show.re.var= TRUE, show.se = TRUE, df.method="satterthwaite", file = "convo_var_infant.html",
          pred.labels =c("(Intercept)", "Adult Gender", "Infant Gender", "Adult Gender*Infant Gender"),
          dv.labels= "Pitch Variability")

#reading
readdat <- pitch[which(pitch$read > 0),]
readIDSdat <-readdat[which(readdat$register > 0),]

mpitchreadIDS = lmer(m_pitch ~ adu_gender*chi_gender + (1 | ID)+ (1|coder), data = readIDSdat)
summary(mpitchreadIDS)
confint(mpitchreadIDS)

tab_model(mpitchreadIDS, show.re.var= TRUE, show.se = TRUE, df.method="satterthwaite", file = "read_,mean_infant.html",
          pred.labels =c("(Intercept)", "Adult Gender", "Infant Gender", "Adult Gender*Infant Gender"),
          dv.labels= "Mean Pitch")

sdpitchread = lmer(sd_pit ~ adu_gender*chi_gender + (1 | ID)+ (1|coder), data = readIDSdat)
summary(sdpitchread)
confint(sdpitchread)

tab_model(sdpitchread, show.re.var= TRUE, show.se = TRUE, df.method="satterthwaite", file = "read_var_infant.html",
          pred.labels =c("(Intercept)", "Adult Gender", "Infant Gender", "Adult Gender*Infant Gender"),
          dv.labels= "Pitch Variability")

#singing
singdat <- pitch[which(pitch$sing > 0),]
singIDSdat <-singdat[which(singdat$register > 0),]

mpitchsingIDS = lmer(m_pitch ~ adu_gender*chi_gender + (1 | ID)+ (1|coder), data = singIDSdat)
summary(mpitchsingIDS)
confint(mpitchsingIDS)

tab_model(mpitchsingIDS, show.re.var= TRUE, show.se = TRUE, df.method="satterthwaite", file = "sing_mean_infant.html",
          pred.labels =c("(Intercept)", "Adult Gender", "Infant Gender", "Adult Gender*Infant Gender"),
          dv.labels= "Mean Pitch")

#following up on interaction 
emmeans(mpitchsingIDS, ~ adu_gender*chi_gender) %>%
  contrast("pairwise", by = c("adu_gender")) %>% 
  summary(by = NULL, adjust = "holm", infer =c(TRUE, TRUE))

emmeans(mpitchsingIDS, ~ adu_gender*chi_gender) %>%
  contrast("pairwise", by = c("chi_gender")) %>% 
  summary(by = NULL, adjust = "holm", infer =c(TRUE, TRUE))

#singing interaction plot estimate and line plot (Fig 3)
p<-plot_model(mpitchsingIDS, type = "int", terms = c(chi_gender,adu_gender), ci.lvl = 0.95)
p+
  labs(x = "Adult Speaker Gender", y = "Predicted Mean Pitch") +
  scale_y_continuous(limits = c(-2, 1.5))+
  scale_x_discrete(limits=c( 0, 1), labels=c("Female", "Male"))+
  scale_color_manual(values = c( "#CC5500","#702963"))+
  theme_bw() 
ggsave("plotsing2.svg")

adu_inf_sing <- ggemmeans(mpitchsingIDS, c("chi_gender", "adu_gender"))
plot(adu_inf_sing) + 
  labs(
    x = "Infant Gender",
    y = "Mean Pitch",
    title = "Mean Pitch of Infant Gender by Adult Speaker Gender",
    colour = "Adult Speaker Gender")
ggsave("adult_mean_sing_infant.svg")

adu_inf_sing2 <- ggemmeans(mpitchsingIDS, c("adu_gender", "chi_gender"))
plot(adu_inf_sing2) + 
  labs(
    x = "Adult Speaker Gender",
    y = "Mean Pitch",
    title = "Mean Pitch of Infant Gender by Adult Speaker Gender",
    colour = "Infant Gender") 
ggsave("adult_mean_sing_infant2.svg")

sdpitchsing = lmer(sd_pit ~ adu_gender*chi_gender + (1 | ID)+ (1|coder), data = singIDSdat)
summary(sdpitchsing)
confint(sdpitchsing)

tab_model(sdpitchsing, show.re.var= TRUE, show.se = TRUE, df.method="satterthwaite", file = "sing_var_infant.html",
          pred.labels =c("(Intercept)", "Adult Gender", "Infant Gender", "Adult Gender*Infant Gender"),
          dv.labels= "Pitch Variability")

#questions
qIDSdat <- questdat[which(questdat$register > 0),]

mpitchqIDS = lmer(m_pitch ~ adu_gender*chi_gender + (1 | ID)+ (1|coder), data =qIDSdat)
summary(mpitchqIDS)
confint(mpitchqIDS)

tab_model(mpitchqIDS, show.re.var= TRUE, show.se = TRUE, df.method="satterthwaite", file = "quest_mean_infant.html",
          pred.labels =c("(Intercept)", "Adult Gender", "Infant Gender", "Adult Gender*Infant Gender"),
          dv.labels= "Mean Pitch")

sdpitchqIDS = lmer(sd_pit ~ adu_gender*chi_gender + (1 | ID)+ (1|coder), data =qIDSdat)
summary(sdpitchqIDS)
confint(sdpitchqIDS)

tab_model(sdpitchqIDS, show.re.var= TRUE, show.se = TRUE, df.method="satterthwaite", file = "quest_var_infant.html",
          pred.labels =c("(Intercept)", "Adult Gender", "Infant Gender", "Adult Gender*Infant Gender"),
          dv.labels= "Pitch Variability")

#imperative 

impIDSdat <- impdat[which(impdat$register > 0),]

mpitchimpIDS = lmer(m_pitch ~ adu_gender*chi_gender + (1 | ID)+ (1|coder), data =impIDSdat)
summary(mpitchimpIDS)
confint(mpitchimpIDS)

tab_model(mpitchimpIDS, show.re.var= TRUE, show.se = TRUE, df.method="satterthwaite", file = "imp_mean_infant.html",
          pred.labels =c("(Intercept)", "Adult Gender", "Infant Gender", "Adult Gender*Infant Gender"),
          dv.labels= "Mean Pitch")

sdpitchimpIDS = lmer(sd_pit ~ adu_gender*chi_gender + (1 | ID) + (1|coder), data =impIDSdat)
summary(sdpitchimpIDS)
confint(sdpitchimpIDS)

tab_model(sdpitchimpIDS, show.re.var= TRUE, show.se = TRUE, df.method="satterthwaite", file = "imp_var_infant.html",
          pred.labels =c("(Intercept)", "Adult Gender", "Infant Gender", "Adult Gender*Infant Gender"),
          dv.labels= "Pitch Variability")

#comfort
comfortdat <- pitch[which(pitch$comfort > 0),]
comfortIDSdat <-comfortdat[which(comfortdat$register > 0),]

mpitchcomfortIDS = lmer(m_pitch ~ adu_gender*chi_gender + (1 | ID)+ (1|coder), data = comfortIDSdat)
summary(mpitchcomfortIDS)
confint(mpitchcomfortIDS)

tab_model(mpitchcomfortIDS, show.re.var= TRUE, show.se = TRUE, df.method="satterthwaite", file = "comfort_mean_infant.html",
          pred.labels =c("(Intercept)", "Adult Gender", "Infant Gender", "Adult Gender*Infant Gender"),
          dv.labels= "Mean Pitch")

sdpitchcomfort = lmer(sd_pit ~ adu_gender*chi_gender + (1 | ID)+ (1|coder), data = comfortIDSdat)
summary(sdpitchcomfort)
confint(sdpitchcomfort)

tab_model(sdpitchcomfort, show.re.var= TRUE, show.se = TRUE, df.method="satterthwaite", file = "comfort_var_infant.html",
          pred.labels =c("(Intercept)", "Adult Gender", "Infant Gender", "Adult Gender*Infant Gender"),
          dv.labels= "Pitch Variability")

