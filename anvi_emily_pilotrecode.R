setwd("~/Desktop/Desktop - Emily’s MacBook Pro/Winter 2022/EOC/Re-training Reliability")

library(tidyverse)
library(ggplot2)
library(plyr)
library(irr)
library(compare)
library(arsenal)
library(tidyr)


#read in the data 

rel <- read.table('R data - Pilot Recode.txt', header=T)

#select coders and create dataframe

split_data <- split(rel, f = rel$coder)

#percent agreement requires that data be in long format

emily <- split_data$emily[,c("convo")]
anvi <- split_data$anvi[,c("convo")]


convoper <- data.frame(emily, anvi)
#how many observations across coders
sum(convoper)

#percent agreement 
agree(convoper, tolerance = 0)

#percent agreement and kappa for each pair of coders 
convo2 <- data.frame(emily, anvi)
agree(convo2, tolerance = 0)
kappa2(convo2)


#create dataframe for convo - splitting data, adding row numbers, and pivoting data (KA requires wide format)

convo_emily <- split_data$emily[,c("convo", "coder")]
convo_emily$row_num <- seq.int(nrow(convo_emily))
emily_wide <- pivot_wider(convo_emily, id_cols = coder, names_from = row_num, values_from = convo)


convo_anvi <- split_data$anvi[,c("convo", "coder")]
convo_anvi$row_num <- seq.int(nrow(convo_anvi))
anvi_wide <- pivot_wider(convo_anvi, id_cols = coder, names_from = row_num, values_from = convo)

#creating dataframe and getting rid of coder column to run Kripps Alpha
convo <- rbind(emily_wide, anvi_wide)

convo <- select(convo, -coder)

#kripps alpha requires that dataframe is matrix not a dataframe

convo <- as.matrix(convo)

#kripp alpha 

kripp.alpha(convo, method=c("nominal"))


#COMFORT - percent agreement requires that data be in long format

emily_com <- split_data$emily[,c("comfort")]
anvi_com <- split_data$anvi[,c("comfort")]


comfortper <- data.frame(emily_com, anvi_com)
#how many observations across coders
sum(comfortper)

#percent agreement 
agree(comfortper, tolerance = 0)

#percent agreement and kappa for each pair of coders 

comfort3 <- data.frame(emily_com, anvi_com)
agree(comfort3, tolerance = 0)
kappa2(comfort3)


#create dataframe for comfort - splitting data, adding row numbers, and pivoting data (KA requires wide format)

comfort_emily <- split_data$emily[,c("comfort", "coder")]
comfort_emily$row_num <- seq.int(nrow(comfort_emily))
emily_wide <- pivot_wider(comfort_emily, id_cols = coder, names_from = row_num, values_from = comfort)


comfort_anvi <- split_data$anvi[,c("comfort", "coder")]
comfort_anvi$row_num <- seq.int(nrow(comfort_anvi))
anvi_wide <- pivot_wider(comfort_anvi, id_cols = coder, names_from = row_num, values_from = comfort)

#creating dataframe and getting rid of coder column to run Kripps Alpha
comfort <- rbind(emily_wide, anvi_wide)

comfort <- select(comfort, -coder)

#kripps alpha requires that dataframe is matrix not a dataframe

comfort <- as.matrix(comfort)

#kripp alpha 

kripp.alpha(comfort, method=c("nominal"))

#SINGING - percent agreement requires that data be in long format

emily_sing <- split_data$emily[,c("sing")]
anvi_sing <- split_data$anvi[,c("sing")]


singper <- data.frame(emily_sing, anvi_sing)
#how many observations across coders
sum(singper)

#percent agreement 
agree(singper, tolerance = 0)

#percent agreement and kappa for each pair of coders 


sing3 <- data.frame(emily_sing, anvi_sing)
agree(sing3, tolerance = 0)
kappa2(sing3)



#create dataframe for sing - splitting data, adding row numbers, and pivoting data (KA requires wide format)

sing_emily <- split_data$emily[,c("sing", "coder")]
sing_emily$row_num <- seq.int(nrow(sing_emily))
emily_wide <- pivot_wider(sing_emily, id_cols = coder, names_from = row_num, values_from = sing)

sing_anvi <- split_data$anvi[,c("sing", "coder")]
sing_anvi$row_num <- seq.int(nrow(sing_anvi))
anvi_wide <- pivot_wider(sing_anvi, id_cols = coder, names_from = row_num, values_from = sing)

#creating dataframe and getting rid of coder column to run Kripps Alpha
sing <- rbind(emily_wide, anvi_wide)

sing <- select(sing, -coder)

#kripps alpha requires that dataframe is matrix not a dataframe

sing <- as.matrix(sing)

#kripp alpha 

kripp.alpha(sing, method=c("nominal"))

#INFORM - percent agreement requires that data be in long format

emily_inform <- split_data$emily[,c("inform")]
anvi_inform <- split_data$anvi[,c("inform")]


informper <- data.frame(emily_inform, anvi_inform)

#how many observations across coders
sum(informper)

#percent agreement 
agree(informper, tolerance = 0)

#percent agreement and kappa for each pair of coders 


inform3 <- data.frame(emily_inform, anvi_inform)
agree(inform3, tolerance = 0)
kappa2(inform3)


#create dataframe for inform - splitting data, adding row numbers, and pivoting data (KA requires wide format)

inform_emily <- split_data$emily[,c("inform", "coder")]
inform_emily$row_num <- seq.int(nrow(inform_emily))
emily_wide <- pivot_wider(inform_emily, id_cols = coder, names_from = row_num, values_from = inform)


inform_anvi <- split_data$anvi[,c("inform", "coder")]
inform_anvi$row_num <- seq.int(nrow(inform_anvi))
anvi_wide <- pivot_wider(inform_anvi, id_cols = coder, names_from = row_num, values_from = inform)

#creating dataframe and getting rid of coder column to run Kripps Alpha
inform <- rbind(emily_wide, anvi_wide)

inform <- select(inform, -coder)

#kripps alpha requires that dataframe is matrix not a dataframe

inform <- as.matrix(inform)

#kripp alpha 

kripp.alpha(inform, method=c("nominal"))


#READ - percent agreement requires that data be in long format

#how many instances of read across the coders 
sum(rel$read)

emily_read <- split_data$emily[,c("read")]
anvi_read <- split_data$anvi[,c("read")]


readper <- data.frame(emily_read, anvi_read)

#percent agreement 
agree(readper, tolerance = 0)

#percent agreement and kappa for each pair of coders 


read3 <- data.frame(emily_read, anvi_read)
agree(read3, tolerance = 0)
kappa2(read3)



#create dataframe for read - splitting data, adding row numbers, and pivoting data (KA requires wide format)

read_emily <- split_data$emily[,c("read", "coder")]
read_emily$row_num <- seq.int(nrow(read_emily))
emily_wide <- pivot_wider(read_emily, id_cols = coder, names_from = row_num, values_from = read)


read_anvi <- split_data$anvi[,c("read", "coder")]
read_anvi$row_num <- seq.int(nrow(read_anvi))
anvi_wide <- pivot_wider(read_anvi, id_cols = coder, names_from = row_num, values_from = read)

#creating dataframe and getting rid of coder column to run Kripps Alpha
read <- rbind(emily_wide, anvi_wide)

read <- select(read, -coder)

#kripps alpha requires that dataframe is matrix not a dataframe

read <- as.matrix(read)

#kripp alpha 

kripp.alpha(read, method=c("nominal"))

#Imperative - percent agreement requires that data be in long format

emily_imp <- split_data$emily[,c("imperative")]
anvi_imp <- split_data$anvi[,c("imperative")]

impper <- data.frame(emily_imp,anvi_imp)

#how many observations across coders
sum(impper)

#percent agreement 
agree(impper, tolerance = 0)

#percent agreement and kappa for each pair of coders 


imp3 <- data.frame(emily_imp, anvi_imp)
agree(imp3, tolerance = 0)
kappa2(imp3)


#create dataframe for imp - splitting data, adding row numbers, and pivoting data (KA requires wide format)

imp_emily <- split_data$emily[,c("imperative", "coder")]
imp_emily$row_num <- seq.int(nrow(imp_emily))
emily_wide <- pivot_wider(imp_emily, id_cols = coder, names_from = row_num, values_from = imperative)

imp_anvi <- split_data$anvi[,c("imperative", "coder")]
imp_anvi$row_num <- seq.int(nrow(imp_anvi))
anvi_wide <- pivot_wider(imp_anvi, id_cols = coder, names_from = row_num, values_from = imperative)

#creating dataframe and getting rid of coder column to run Kripps Alpha
imp <- rbind(emily_wide, anvi_wide)

imp <- select(imp, -coder)

#kripps alpha requires that dataframe is matrix not a dataframe

imp <- as.matrix(imp)

#kripp alpha 

kripp.alpha(imp, method=c("nominal"))

#Questions - percent agreement requires that data be in long format

emily_quest <- split_data$emily[,c("question")]
anvi_quest <- split_data$anvi[,c("question")]

questper <- data.frame(emily_quest, anvi_quest)

#how many observations across coders
sum(questper)

#percent agreement 
agree(questper, tolerance = 0)

#percent agreement and kappa for each pair of coders 

quest3 <- data.frame(emily_quest, anvi_quest)
agree(quest3, tolerance = 0)
kappa2(quest3)



#create dataframe for quest - splitting data, adding row numbers, and pivoting data (KA requires wide format)

quest_emily <- split_data$emily[,c("question", "coder")]
quest_emily$row_num <- seq.int(nrow(quest_emily))
emily_wide <- pivot_wider(quest_emily, id_cols = coder, names_from = row_num, values_from = question)

quest_anvi <- split_data$anvi[,c("question", "coder")]
quest_anvi$row_num <- seq.int(nrow(quest_anvi))
anvi_wide <- pivot_wider(quest_anvi, id_cols = coder, names_from = row_num, values_from = question)

#creating dataframe and getting rid of coder column to run Kripps Alpha
quest <- rbind(emily_wide, anvi_wide)

quest <- select(quest, -coder)

#kripps alpha requires that dataframe is matrix not a dataframe

quest <- as.matrix(quest)

#kripp alpha 

kripp.alpha(quest, method=c("nominal"))

#Vocal Play - percent agreement requires that data be in long format

emily_vocalplay <- split_data$emily[,c("vocalplay")]
anvi_vocalplay <- split_data$anvi[,c("vocalplay")]


vocalplayper <- data.frame(emily_vocalplay, anvi_vocalplay)

#how many observations across coders
sum(vocalplayper)

#percent agreement 
agree(vocalplayper, tolerance = 0)

#percent agreement and kappa for each pair of coders 

vocalplay3 <- data.frame(emily_vocalplay, anvi_vocalplay)
agree(vocalplay3, tolerance = 0)
kappa2(vocalplay3)


#create dataframe for vocalplay - splitting data, adding row numbers, and pivoting data (KA requires wide format)

vocalplay_emily <- split_data$emily[,c("vocalplay", "coder")]
vocalplay_emily$row_num <- seq.int(nrow(vocalplay_emily))
emily_wide <- pivot_wider(vocalplay_emily, id_cols = coder, names_from = row_num, values_from = vocalplay)

vocalplay_anvi <- split_data$anvi[,c("vocalplay", "coder")]
vocalplay_anvi$row_num <- seq.int(nrow(vocalplay_anvi))
anvi_wide <- pivot_wider(vocalplay_anvi, id_cols = coder, names_from = row_num, values_from = vocalplay)

#creating dataframe and getting rid of coder column to run Kripps Alpha
vocalplay <- rbind(emily_wide, anvi_wide)

vocalplay <- select(vocalplay, -coder)

#kripps alpha requires that dataframe is matrix not a dataframe

vocalplay <- as.matrix(vocalplay)

#kripp alpha 

kripp.alpha(vocalplay, method=c("nominal"))

#Noisy - percent agreement requires that data be in long format

emily_noisy <- split_data$emily[,c("noisy")]
anvi_noisy <- split_data$anvi[,c("noisy")]

noisyper <- data.frame(emily_noisy, anvi_noisy)

#how many observations across coders
sum(noisyper)

#percent agreement 
agree(noisyper, tolerance = 0)

#percent agreement and kappa for each pair of coders 

noisy3 <- data.frame(emily_noisy, anvi_noisy)
agree(noisy3, tolerance = 0)
kappa2(noisy3)


#create dataframe for noisy - splitting data, adding row numbers, and pivoting data (KA requires wide format)

noisy_emily <- split_data$emily[,c("noisy", "coder")]
noisy_emily$row_num <- seq.int(nrow(noisy_emily))
emily_wide <- pivot_wider(noisy_emily, id_cols = coder, names_from = row_num, values_from = noisy)

noisy_anvi <- split_data$anvi[,c("noisy", "coder")]
noisy_anvi$row_num <- seq.int(nrow(noisy_anvi))
anvi_wide <- pivot_wider(noisy_anvi, id_cols = coder, names_from = row_num, values_from = noisy)

#creating dataframe and getting rid of coder column to run Kripps Alpha
noisy <- rbind(emily_wide, anvi_wide)

noisy <- select(noisy, -coder)

#kripps alpha requires that dataframe is matrix not a dataframe

noisy <- as.matrix(noisy)

#kripp alpha 

kripp.alpha(noisy, method=c("nominal"))