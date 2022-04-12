install.packages(c("R.matlab", "stringr", "splitstackshape", "writexl", "dplyr", "tidyr", "tidyverse"))
library(R.matlab)
library(stringr)
library(splitstackshape)
library(writexl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(readxl)

## Input variables: 1)subjnum_DistractionTask.csv file (encoding phase) 2)subjnum_dataOut.mat (testing phase) file 3)stimuli_set.xlsx file(fixed values to use for all subjects. This file contains the parameters about stimuli (stimuli name, social vs unsocial, negative vs neutral).

## Data Arrangement

stimuli_set <- read_xlsx("/Users/neslihanonay/Desktop/Ulrike_internship/Test/stimuli_set.xlsx")
distraction <- read.csv("/Users/neslihanonay/Downloads/124_DistractionTask.csv", header = TRUE) #read the encoding file
test <- readMat("/Users/neslihanonay/Downloads/124_dataout.mat") #to read matlab-test file
data = lapply(test, unlist, use.names=FALSE) #to read matlab-test file
test_mem <- as.data.frame(data) #to revise data format
stimuli <- test_mem[c(1:96),] #to arrange the columns in test file
rkn <- test_mem[c(97:192),] #to arrange the columns in test file
confidence <- test_mem[c(193:288),] #to arrange the columns in test file
source <- test_mem[c(289:384),] #to arrange the columns in test file
test_mem <- test_mem[c(97:384),] #to arrange the columns in test file
new_test_mem <- cbind(stimuli, rkn, confidence, source) #to combine the columns in test file
new_test_mem <- as.data.frame(new_test_mem) #to revise data format
new_test_mem$stimuli <- str_replace(new_test_mem$stimuli, "JPG", "jpg") #to clean test file. We need all stimuli set exactly the same as encoding file. So that we can combine them and create just one data frame 
new_distraction <- distraction[,-c(9)] #delete the variable which we do not need in encoding file
new_distraction <- distraction[-c(1:7),] #delete the rows which are trials in encoding file
new_test_mem <- as.data.frame(new_test_mem) #update the format of data frame
new_distraction <- as.data.frame(new_distraction) #update the format of data frame
new_all <- cbind(new_test_mem, new_distraction [match(new_test_mem$stimuli, new_distraction$stimuliName),]) #combine encoding and test file
new_all <- cbind(new_all, stimuli_set [match(new_all$stimuli, stimuli_set$Stimuli_set), -1, drop = FALSE]) #combine stimuli set and test&encoding file. Now we have all information about stimulus characteristics(negative vs neutral events, social vs unsocial images)
new_all <- new_all[,-c(5)] #delete the column that we do not neet
names(new_all)[names(new_all) == "imgType_2"] <- "imgType" #update the name of the columns which are duplicate
new_all$imgType <- str_replace(new_all$imgType, "Négative", "Negative")
new_all$imgType <- as.factor(new_all$imgType)


## Creating the Variables Needed to Calculate Recognition Scores


new_all$new_old[(is.na(new_all$stimuliName))] <- "new" #if the stimulus is shown in encoding or #not. "new" shows that the respondent have first seen the stimulus in testing. 
new_all$new_old[(!is.na(new_all$stimuliName))] <- "old" #if the stimulus is shown in encoding or #not. "old" shows that the respondent have first seen the stimulus in testing. 
new_all$new_old <- as.factor(new_all$new_old) #update the vector structure/variable format
new_all$remember <- (new_all$new_old == "old")&new_all$rkn == "7" #"response" variable is including 7=remember, 8=know and 9=new. This created new vector enables us to to see the count of responses with as answered as "remember" for old stimuli. It is "old" stimuli because here we are testing the memory for encoded events.
new_all$remember <- as.integer(new_all$remember) #update variable structure
new_all$remember <- as.integer(new_all$remember)
new_all$know <- (new_all$new_old == "old")&new_all$rkn == "8" #This created new vector enables us to to see the count of responses with as answered as "know" for old stimuli. It is "old" stimuli because here we are testing the memory for encoded events.
new_all$know <- as.integer(new_all$know) #update variable structure
new_all$remknow <- (new_all$remember == "1") | (new_all$know == "1") #This created to see the sum of trials as answered either as "remember" or "know". Subject can select just "remember", "know" or "new". This is to remind that "response variable(7 = remember/8 = know/9 = new ) is a binary variable.
new_all$remknow <- as.integer(new_all$remknow)  #update variable structure
new_all$FA_rem <- (new_all$new_old == "new") & (new_all$rkn == "7") #this vector incicates that the subject is responsing as "remember" which actually a new stimuli (not seen in encoding part).
new_all$FA_know <- (new_all$new_old == "new") & (new_all$rkn == "8") #this vector incicates that the subject is responsing as "know" which actually a new stimuli (not seen in encoding part).
new_all$FA_rem <- as.integer(new_all$FA_rem)
new_all$FA_know<- as.integer(new_all$FA_know)
new_all$FA_total <- (new_all$FA_rem == "1") | (new_all$FA_know == "1") #here we are summing the al false alarms.
new_all$FA_total<- as.integer(new_all$FA_total)
new_all$correct_rej <- (new_all$new_old == "new") & (new_all$rkn == "9") #correct rejections mean that stimulus is new and respondent says "new" to it.
new_all$correct_rej <- as.integer(new_all$correct_rej)
new_all$miss <- (new_all$new_old == "old") & (new_all$rkn == "9") #missed trials are indicating that subject is responding as "new" to stimuli which is actually shown in encoding part.
new_all$miss <- as.integer(new_all$miss)
new_all$condition <- as.factor(new_all$condition)
new_all$imgType <- as.factor(new_all$imgType)
new_all <- new_all[,-c(5)]
new_all <- as.data.frame(unclass(new_all), stringsAsFactors = TRUE)

##Calculating Recognition Scores for All Stimuli


Hit_all_remknow <- sum(new_all$remknow)/length(which(new_all$new_old == "old")) #sum of all "Remember"s or "Know"s are divided by the total number of "old" stimuli
False.Alarm_remknow<- sum(new_all$FA_total)/length(which(new_all$new_old == "old")) #sum of all "False Alarm"s are divided by the total number of "old" stimuli
Correct.Rej_remknow <- sum(new_all$correct_rej)/length(which(new_all$new_old == "old")) #sum of all "Correct Rejections"s are divided by the total number of "old" stimuli
Miss_remknow <- sum(new_all$miss)/length(which(new_all$new_old == "old")) #sum of all "Miss"s are divided by the total number of "old" stimuli
Hit_rem <- sum(new_all$remember)/length(which(new_all$new_old == "old")) #sum of all "Remember"s are divided by the total number of "old" stimuli
False.Alarm_rem<- sum(new_all$FA_rem)/length(which(new_all$new_old == "old")) #sum of all "False Alarms"s for "Remember"s are divided by the total number of "old" stimuli
Hit_know<- sum(new_all$know)/length(which(new_all$new_old == "old")) #sum of all "Know"s for are divided by the total number of "old" stimuli
False.Alarm_know<- sum(new_all$FA_know)/length(which(new_all$new_old == "old")) #sum of all "False Alarm"s for "Know"s are divided by the total number of "old" stimuli
Recollection <- Hit_rem -False.Alarm_rem #Ratio of substraction of "Hit" for "Remember"s from "False Alarm"s for "Remember"s


#Calculating Recognition Scores for Emotional Stimuli

neg <- filter(new_all, imgType == "Negative") #We created data frame which include just "Negative" stimuli and below we do the same calculations that we did as before (refer line 67 if needed)
hit_remknow_neg <- sum(neg$remknow)/length(which(neg$new_old == "old"))
false.Alarm_remknow_neg<- sum(neg$FA_total)/length(which(neg$new_old == "old"))
correct.rej_remknow_neg <- sum(neg$correct_rej)/length(which(neg$new_old == "old"))
miss_remknow_neg <- sum(neg$miss)/length(which(neg$new_old == "old"))
hit_rem_neg <- sum(neg$remember)/length(which(neg$new_old == "old"))
false.alarm_rem_neg<- sum(neg$FA_rem)/length(which(neg$new_old == "old"))
hit_know_neg<- sum(neg$know)/length(which(neg$new_old == "old"))
recollection_neg <- hit_rem_neg -false.alarm_rem_neg


#Calculating Recognition Scores for Neutral Stimuli

neutral <- filter(new_all, imgType == "Neutral") #We created data frame which include just "Neutral" stimuli and below we do the same calculations that we did as before (refer line 67 if needed)
hit_remknow_neutr <- sum(neutral$remknow)/length(which(neutral$new_old == "old"))
false.Alarm_remknow_neutr<- sum(neutral$FA_total)/length(which(neutral$new_old == "old"))
correct.rej_remknow_neutr <- sum(neutral$correct_rej)/length(which(neutral$new_old == "old"))
miss_remknow_neutr <- sum(neutral$miss)/length(which(neutral$new_old == "old"))
hit_rem_neutr <- sum(neutral$remember)/length(which(neutral$new_old == "old"))
false.alarm_rem_neutr <- sum(neutral$FA_rem)/length(which(neutral$new_old == "old"))
hit_know_neutr<- sum(neutral$know)/length(which(neutral$new_old == "old"))
recollection_neutr <- hit_rem_neg -false.alarm_rem_neg


#Calculating Recognition Scores for Emotion Regulation Types

#Here we created a data frame contaning variable with emotion regulation types. We created a variable with three levels(look, distract and reappraise conditions). Afterward, we will be able to calculate recognition scores of conditions. 

new_all$condition <- as.factor(new_all$condition)
new_all <- as.data.frame(new_all)
emo_ef_clean <- cSplit(new_all, "condition", "_")
levels(emo_ef_clean$condition_1)[c("neglook", "neulook")] <- c("look")
emo_ef_clean$condition_1 <- str_replace(emo_ef_clean$condition_1, "neglook", "look")
emo_ef_clean$condition_1 <- str_replace(emo_ef_clean$condition_1, "neulook", "look")
emo_ef_clean$condition_1 <- as.factor(emo_ef_clean$condition_1)
emo_ef_clean <- emo_ef_clean[,-c(24)]
names(emo_ef_clean)[names(emo_ef_clean) == "condition_1"] <- "emo_reg_type"
names(emo_ef_clean)[names(emo_ef_clean) == "condition_2"] <- "img_social"
emo_ef_clean$imgType <- str_replace(emo_ef_clean$imgType, "Négative", "Negative")
emo_ef_clean$imgType <- as.factor(emo_ef_clean$imgType)
emo_ef_clean$content <- str_replace(emo_ef_clean$content, "non-social", "Non-social")
emo_ef_clean$content <- str_replace(emo_ef_clean$content, "social", "Social")
emo_ef_clean$content <- as.factor(emo_ef_clean$content)
#Here, we are coding what participants`s response to characters. The zeros here coded as NAs. They represent participant did not answer.
emo_ef_clean$source_resp[emo_ef_clean$source == "1"] <- "look" 
emo_ef_clean$source_resp[emo_ef_clean$source == "2"] <- "reappraise"
emo_ef_clean$source_resp[emo_ef_clean$source == "3"] <- "distract"
emo_ef_clean$source_resp[emo_ef_clean$source == "4"] <- "IDK"
emo_ef_clean$source_acc <- (emo_ef_clean$emo_reg_type == emo_ef_clean$source_resp)
emo_ef_clean$source_acc <- as.integer(emo_ef_clean$source_acc)
emo_ef_clean$source_resp <- as.factor(emo_ef_clean$source_resp)


###Calculating Recognition Scores for Look Condition for Negative Events

neg_emo_reg_look <- filter(emo_ef_clean, emo_reg_type == "look" & imgType =="Negative")
hit_remknow_look_neg <- sum(neg_emo_reg_look$remknow)/length(which(neg_emo_reg_look$new_old == "old"))
false.Alarm_remknow_look_neg<- sum(neg_emo_reg_look$FA_total)/length(which(neg_emo_reg_look$new_old == "old"))
correct.rej_remknow_look_neg <- sum(neg_emo_reg_look$correct_rej)/length(which(neg_emo_reg_look$new_old == "old"))
miss_remknow_look_neg <- sum(neg_emo_reg_look$miss)/length(which(neg_emo_reg_look$new_old == "old"))
hit_rem_look_neg <- sum(neg_emo_reg_look$remember)/length(which(neg_emo_reg_look$new_old == "old"))
false.alarm_rem_look_neg<- sum(neg_emo_reg_look$FA_rem)/length(which(neg_emo_reg_look$new_old == "old"))
hit_know_look_neg<- sum(neg_emo_reg_look$know)/length(which(neg_emo_reg_look$new_old == "old"))
recollection_look_neg <- hit_rem_look_neg -false.alarm_rem_look_neg


###Calculating Recognition Scores for Reappraise Condition for Negative Events

neg_emo_reg_reapp <- filter(emo_ef_clean, emo_reg_type == "reappraise" & imgType =="Negative")
hit_remknow_rapp_neg <- sum(neg_emo_reg_reapp$remknow)/length(which(neg_emo_reg_reapp$new_old == "old"))
false.Alarm_remknow_reapp_neg<- sum(neg_emo_reg_reapp$FA_total)/length(which(neg_emo_reg_reapp$new_old == "old"))
correct.rej_remknow_reapp_neg <- sum(neg_emo_reg_reapp$correct_rej)/length(which(neg_emo_reg_reapp$new_old == "old"))
miss_remknow_reapp_neg <- sum(neg_emo_reg_reapp$miss)/length(which(neg_emo_reg_reapp$new_old == "old"))
hit_rem_reapp_neg <- sum(neg_emo_reg_reapp$remember)/length(which(neg_emo_reg_reapp$new_old == "old"))
false.alarm_rem_reapp_neg<- sum(neg_emo_reg_reapp$FA_rem)/length(which(neg_emo_reg_reapp$new_old == "old"))
hit_know_reapp_neg<- sum(neg_emo_reg_reapp$know)/length(which(neg_emo_reg_reapp$new_old == "old"))
recollection_reapp_neg <- hit_rem_reapp_neg -false.alarm_rem_reapp_neg


###Calculating Recognition Scores for Distract Condition for Negative Events

neg_emo_reg_dist <- filter(emo_ef_clean, emo_reg_type == "distract" & imgType =="Negative")
hit_remknow_dist_neg <- sum(neg_emo_reg_dist$remknow)/length(which(neg_emo_reg_dist$new_old == "old"))
false.Alarm_remknow_dist_neg<- sum(neg_emo_reg_dist$FA_total)/length(which(neg_emo_reg_dist$new_old == "old"))
correct.rej_remknow_dist_neg <- sum(neg_emo_reg_dist$correct_rej)/length(which(neg_emo_reg_dist$new_old == "old"))
miss_remknow_dist_neg <- sum(neg_emo_reg_dist$miss)/length(which(neg_emo_reg_dist$new_old == "old"))
hit_rem_dist_neg <- sum(neg_emo_reg_dist$remember)/length(which(neg_emo_reg_dist$new_old == "old"))
false.alarm_rem_dist_neg<- sum(neg_emo_reg_dist$FA_rem)/length(which(neg_emo_reg_dist$new_old == "old"))
hit_know_dist_neg<- sum(neg_emo_reg_dist$know)/length(which(neg_emo_reg_dist$new_old == "old"))
recollection_dist_neg <- hit_rem_dist_neg -false.alarm_rem_dist_neg


###Calculating Recognition Scores for Look Condition for Neutral Events


neu_emo_reg_look <- filter(emo_ef_clean, emo_reg_type == "look" & imgType =="Neutral")
hit_remknow_look_neu <- sum(neu_emo_reg_look$remknow)/length(which(neu_emo_reg_look$new_old == "old"))
false.Alarm_remknow_look_neu<- sum(neu_emo_reg_look$FA_total)/length(which(neu_emo_reg_look$new_old == "old"))
correct.rej_remknow_look_neu <- sum(neu_emo_reg_look$correct_rej)/length(which(neu_emo_reg_look$new_old == "old"))
miss_remknow_look_neu <- sum(neu_emo_reg_look$miss)/length(which(neu_emo_reg_look$new_old == "old"))
hit_rem_look_neu <- sum(neu_emo_reg_look$remember)/length(which(neu_emo_reg_look$new_old == "old"))
false.alarm_rem_look_neu<- sum(neu_emo_reg_look$FA_rem)/length(which(neu_emo_reg_look$new_old == "old"))
hit_know_look_neu<- sum(neu_emo_reg_look$know)/length(which(neu_emo_reg_look$new_old == "old"))
recollection_look_neu <- hit_rem_look_neu  - false.alarm_rem_look_neu

###Below lines are the explanations of the variables created from line 239 to 274.

#Below calculations will cover below variables:
#Sum of "Remember" Responses for
#1)source memory is answered as "I don't know" and for correct "Remember" for all.
#2)source memory is answered as "I don't know" and for correct "Remember" for "negative" images for "look" condition.
#3)source memory is answered as "I don't know" and for correct "Remember" for "negative" images for "distract" condition.
#4)source memory is answered as "I don't know" and for correct "Remember" for "negative" images for "reappraise" condition.
#5)source memory is answered as "I don't know" and for correct "Remember" for "neutral" images.
#Sum of "Know" Responses for
#6)source memory is answered as "I don't know" and for correct "Know" for all.
#7)source memory is answered as "I don't know" and for correct "Know" for "negative" images for "look" condition.
#8)source memory is answered as "I don't know" and for correct "Know" for "negative" images for "distract" condition.
#9)source memory is answered as "I don't know" and for correct "Know" for "negative" images for "reappraise" condition.
#10)source memory is answered as "I don't know" and for correct "Know" for "neutral" images.
#Sum of "Remember or Know" Responses for
#11)source memory is answered as "I don't know" and for correct "Remember or Know" for all.
#12)source memory is answered as "I don't know" and for correct "Remember or Know" for "negative" images for "look" condition.
#13)source memory is answered as "I don't know" and for correct "Remember or Know" for "negative" images for "distract" condition.
#14)source memory is answered as "I don't know" and for correct "Remember or Know" for "negative" images for "reappraise" condition.
#15)source memory is answered as "I don't know" and for correct "Remember or Know" for "neutral" images.
#Sum of "Remember" Responses for
#16)source memory is answered as wrong and for correct "Remember" for all.
#17)source memory is answered as wrong and for correct "Remember" for "negative" images for "look" condition.
#18)source memory is answered as wrong and for correct "Remember" for "negative" images for "distract" condition.
#19)source memory is answered as wrong and for correct "Remember" for "negative" images for "reappraise" condition.
#20)source memory is answered as wrong and for correct "Remember" for "neutral" images.
#Sum of "Know" Responses for
#21)source memory is answered as wrong and for correct "Know" for all.
#22)source memory is answered as wrong and for correct "Know" for "negative" images for "look" condition.
#23)source memory is answered as wrong and for correct "Know" for "negative" images for "distract" condition.
#24)source memory is answered as wrong and for correct "Know" for "negative" images for "reappraise" condition.
#25)source memory is answered as wrong and for correct "Know" for "neutral" images.
#Sum of "Remember or Know" Responses for
#26)source memory is answered as wrong and for correct "Remember or Know" for all.
#27)source memory is answered as wrong and for correct "Remember or Know" for "negative" images for "look" condition.
#28)source memory is answered as wrong and for correct "Remember or Know" for "negative" images for "distract" condition.
#29)source memory is answered as wrong and for correct "Remember or Know" for "negative" images for "reappraise" condition.
#30)source memory is answered as wrong and for correct "Remember or Know" for "neutral" images.

#Sum of "Remember" Responses for
#thirtyone_crr_R source memory is answered as correct and for correct "Remember" for all.
#thirtytwo_crr_R source memory is answered as correct and for correct "Remember" for "negative" images for "look" condition.
#thirtythree_crr_R source memory is answered as correct and for correct "Remember" for "negative" images for "distract" condition.
#thirtyfour_crr_R source memory is answered as correct and for correct "Remember" for "negative" images for "reappraise" condition.
#total_source_negative_correct_R is % source memory answered as correct for all negative events. 
#thirtytwo_crr_R_per is total is % source memory answered as correct "Remember" for "negative" images for "look" condition.
#thirtythree_crr_R_per is total is % source memory answered as correct and for correct "Remember" for "negative" images for "distract" condition.
#thirtyfour_crr_R_per is total is % source memory as correct and for correct "Remember" for "negative" images for "reappraise" condition.
#thirtyfive_crr_R_per is total is % source memory as correct and for correct "Remember"for for "neutral" images.

#Sum of "Know" Responses for
#thirtysix_crr_K source memory is answered as correct and for correct "Know" for all.
#thirtyseven_crr_K source memory is answered as correct and for correct "Know" for "negative" images for "look" condition.
#thirtyeight_crrr_K source memory is answered as correct and for correct "Know" for "negative" images for "distract" condition.
#thirtynine_crr_K source memory is answered as correct and for correct "Know" for "negative" images for "reappraise" condition.
#total_source_negative_correct_K is % source memory answered as correct for all negative events.
#thirtyseven_crr_K_perc is % source memory answered as correct and for correct "Know" for "negative" images for "look" condition.
#thirtyeight_crr_K_perc is % source memory answered as correct and for correct "Know" for "negative" images for "distract" condition.
#thirtynine_crr_K_perc  is % source memory answered as correct and for correct "Know" for "negative" images for "reappraise" condition.
#fourtyfive_crr_RK_perc is % source memory answered as correct and for correct "Know" for "neutral" images.
#fourty_crr_K_perc is % source memory is answered as correct and for correct "Know" for "neutral" images.


#Sum of "Remember or Know" Responses for
# fourtyone_crr_RK source memory is answered as correct and for correct "Remember or Know" for all.
# fourtytwo_crr_RK source memory is answered as correct and for correct "Remember or Know" for "negative" images for "look" condition.
# fourtythree_crr_RK source memory is answered as correct and for correct "Remember or Know" for "negative" images for "distract" condition.
# fourtyfour_crr_RK source memory is answered as correct and for correct "Remember or Know" for "negative" images for "reappraise" condition.
# fourtyfive_crr_RK_perc source memory is % source memory is answered as correct for "Remember or Know" for "neutral" images.
# fourtytwo_crr_RK_perc is % source memory answered as correct and for correct "Remember or Know" for "negative" images for "look" condition.
# fourtythree_crr_R_perc is % source memory answered as correct and for correct "Remember or Know" for "negative"  images for "distract" condition.
# fourtyfour_crr_RK_perc is % source memory answered as correct and for correct "Remember or Know" for "negative"  images for "reappraise" condition.
# fourtyfive_crr_RK_perc is % source memory answered as correct and for correct "Remember or Know" for "neutral" images.


one_idk_R <- sum(emo_ef_clean[which(emo_ef_clean$source== "4"&emo_ef_clean$new_old =="old"),14])/48
two_idk_R <- sum(emo_ef_clean[which(emo_ef_clean$source== "4"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="look"),14])/12
three_idk_R <- sum(emo_ef_clean[which(emo_ef_clean$source== "4"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="distract"),14])/12
four_idk_R <- sum(emo_ef_clean[which(emo_ef_clean$source== "4"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="reappraise"),14])/12
five_idk_R <- sum(emo_ef_clean[which(emo_ef_clean$source== "4"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Neutral"),14])/12
six_idk_K <- sum(emo_ef_clean[which(emo_ef_clean$source== "4"&emo_ef_clean$new_old =="old"),15])/48
seven_idk_K <- sum(emo_ef_clean[which(emo_ef_clean$source== "4"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="look"),15])/12
eight_idk_K <- sum(emo_ef_clean[which(emo_ef_clean$source== "4"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="distract"),15])/12
nine_idk_K <- sum(emo_ef_clean[which(emo_ef_clean$source== "4"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="reappraise"),15])/12
ten_idk_K <- sum(emo_ef_clean[which(emo_ef_clean$source== "4"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Neutral"),15])/12
eleven_idk_RK <- sum(emo_ef_clean[which(emo_ef_clean$source== "4"&emo_ef_clean$new_old =="old"),16])/48
twelve_idk_RK <- sum(emo_ef_clean[which(emo_ef_clean$source== "4"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="look"),16])/12
thirteen_idk_RK <- sum(emo_ef_clean[which(emo_ef_clean$source== "4"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="distract"),16])/12
fourteen_idk_RK <- sum(emo_ef_clean[which(emo_ef_clean$source== "4"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="reappraise"),16])/12
fifteen_idk_RK <- sum(emo_ef_clean[which(emo_ef_clean$source== "4"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Neutral"),16])/12
sixteen_wr_R <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "0"&emo_ef_clean$new_old =="old"),14])/48
seventeen_wr_R <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "0"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="look"),14])/12
eighteen_wr_R <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "0"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="distract"),14])/12
nineteen_wr_R <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "0"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="reappraise"),14])/12
twenty_wr_R <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "0"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Neutral"),14])/12
twntone_wr_K <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "0"&emo_ef_clean$new_old =="old"),15])
twntwo_wr_K <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "0"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="look"),15])
twnthree_wr_K <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "0"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="distract"),15])
twntfour_wr_K <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "0"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="reappraise"),15])
twntfive_wr_K <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "0"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Neutral"),15])/12
twnsix_wr_RK <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "0"&emo_ef_clean$new_old =="old"),16])/48
twntsvn_wr_RK <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "0"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="look"),16])/12
twnteight_wr_RK <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "0"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="distract"),16])/12
twntnine_wr_RK <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "0"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="reappraise"),16])/12
thirty_wr_RK <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "0"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Neutral"),16])/12

thirtyone_crr_R_per <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "1"&emo_ef_clean$new_old =="old"),14])
thirtytwo_crr_R <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "1"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="look"),14])
thirtythree_crr_R <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "1"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="distract"),14])
thirtyfour_crr_R <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "1"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="reappraise"),14])
total_source_negative_correct_R <-(thirtytwo_crr_R+thirtythree_crr_R+thirtyfour_crr_R)/36
thirtytwo_crr_R_per <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "1"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="look"),14])/12
thirtythree_crr_R_per <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "1"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="distract"),14])/12
thirtyfour_crr_R_per <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "1"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="reappraise"),14])/12
thirtyfive_crr_R_per <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "1"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Neutral"),14])/12


thirtysix_crr_K <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "1"&emo_ef_clean$new_old =="old"),15])
thirtyseven_crr_K <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "1"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="look"),15])
thirtyeight_crrr_K <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "1"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="distract"),15])
thirtynine_crr_K <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "1"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="reappraise"),15])
total_source_negative_correct_K <- (thirtyseven_crr_K+thirtyeight_crrr_K+thirtynine_crr_K)/36
thirtyseven_crr_K_perc <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "1"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="look"),15])/12
thirtyeight_crr_K_perc <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "1"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="distract"),15])/12
thirtynine_crr_K_perc <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "1"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="reappraise"),15])/12
fourty_crr_K_perc <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "1"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Neutral"),15])/12


fourtyone_crr_RK <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "1"&emo_ef_clean$new_old =="old"),16])
fourtytwo_crr_RK <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "1"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="look"),16])
fourtythree_crr_RK <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "1"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="distract"),16])
fourtyfour_crr_RK <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "1"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="reappraise"),16])
total_source_negative_correct_RK <- (fourtytwo_crr_RK+fourtythree_crr_RK+fourtyfour_crr_RK)/36
fourtytwo_crr_RK_perc <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "1"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="look"),16])/12
fourtythree_crr_R_perc<- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "1"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="distract"),16])/12
fourtyfour_crr_RK_perc <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "1"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Negative"&emo_ef_clean$emo_reg_type=="reappraise"),16])/12
fourtyfive_crr_RK_perc <- sum(emo_ef_clean[which(emo_ef_clean$source_acc== "1"&emo_ef_clean$new_old =="old"&emo_ef_clean$imgType == "Neutral"),16])/12

###Calculating Stars Caught in Encoding Depending on Emotion Regulation Type

#Below codes, we are calculating the mean stars caught in encoding part for different conditions (look for negative events, reappraise for negative events, distract for negative events and look for neutral events, for all data, for negative events, for neutral events) 

emo_ef_clean$starsCaught <- as.numeric(emo_ef_clean$starsCaught)
neg$starsCaught <- as.numeric(neg$starsCaught)
neutral$starsCaught <- as.numeric(neutral$starsCaught)
neg_emo_reg_look$starsCaught <- as.numeric(neg_emo_reg_look$starsCaught)
neg_emo_reg_reapp$starsCaught <- as.numeric(neg_emo_reg_reapp$starsCaught)
neg_emo_reg_dist$starsCaught <- as.numeric(neg_emo_reg_dist$starsCaught)
neu_emo_reg_look$starsCaught <- as.numeric(neu_emo_reg_look$starsCaught)
mean_stars_all <- mean(emo_ef_clean$starsCaught, na.rm=TRUE)
mean_stars_neg <- mean(neg$starsCaught, na.rm = TRUE)
mean_stars_neutral <- mean(neutral$starsCaugh, na.rm = TRUE)
mean_stars_neg_emo_reg_look <- mean(neg_emo_reg_look$starsCaught, na.rm = TRUE)
mean_stars_neg_emo_reg_reapp <- mean(neg_emo_reg_reapp$starsCaught, na.rm = TRUE)
mean_stars_neg_emo_reg_dist <- mean(neg_emo_reg_dist$starsCaught, na.rm = TRUE)
mean_stars_neu_emo_reg_look <- mean(neu_emo_reg_look$starsCaught, na.rm = TRUE)

###Calculating Average Valence Rating in Encoding Depending on Emotion Regulation Type


neg_emo_reg_look$ratingNeg <- as.numeric(neg_emo_reg_look$ratingNeg)
neg_emo_reg_reapp$ratingNeg <- as.numeric(neg_emo_reg_reapp$ratingNeg)
neg_emo_reg_dist$ratingNeg <- as.numeric(neg_emo_reg_dist$ratingNeg)
neu_emo_reg_look$ratingNeg <- as.numeric(neu_emo_reg_look$ratingNeg)

mean_neg_look <- mean(neg_emo_reg_look$ratingNeg)
mean_neg_reapp <- mean(neg_emo_reg_reapp$ratingNeg)
mean_neg_dist <- mean(neg_emo_reg_dist$ratingNeg)
mean_neu_look <- mean(neu_emo_reg_look$ratingNeg)



###Saving All of Our Calculations with New Data Frame

#Let`s first combine every new variables that we have. Then this new data frame to our computer.
all_together<- cbind(Hit_all_remknow, False.Alarm_remknow, Correct.Rej_remknow, Miss_remknow,
Hit_rem, False.Alarm_rem, Hit_know, False.Alarm_know, Recollection, hit_remknow_neg,
false.Alarm_remknow_neg, correct.rej_remknow_neg, miss_remknow_neg, hit_rem_neg,
false.alarm_rem_neg, hit_know_neg, recollection_neg, hit_remknow_neutr, false.Alarm_remknow_neutr,
correct.rej_remknow_neutr, miss_remknow_neutr, hit_rem_neutr, false.alarm_rem_neutr, hit_know_neutr,
recollection_neutr, hit_remknow_look_neg, false.Alarm_remknow_look_neg, correct.rej_remknow_look_neg,
miss_remknow_look_neg, hit_rem_look_neg, false.alarm_rem_look_neg, hit_know_look_neg, recollection_look_neg,
hit_remknow_rapp_neg, false.Alarm_remknow_reapp_neg, correct.rej_remknow_reapp_neg,
miss_remknow_reapp_neg, hit_rem_reapp_neg, false.alarm_rem_reapp_neg, hit_know_reapp_neg, recollection_reapp_neg, 
hit_remknow_dist_neg, false.Alarm_remknow_dist_neg, correct.rej_remknow_dist_neg, miss_remknow_dist_neg,
hit_rem_dist_neg, false.alarm_rem_dist_neg, hit_know_dist_neg, recollection_dist_neg, hit_remknow_look_neu,
false.Alarm_remknow_look_neu, correct.rej_remknow_look_neu , miss_remknow_look_neu , hit_rem_look_neu,
false.alarm_rem_look_neu, hit_know_look_neu, recollection_look_neu, mean_stars_all, mean_stars_neg,
mean_stars_neutral, mean_stars_neg_emo_reg_look, mean_stars_neg_emo_reg_reapp, mean_stars_neg_emo_reg_dist,
mean_stars_neu_emo_reg_look, xx) #all variables should be merged

all_together <- as.data.frame(all_together)
write_xlsx(all_together, "/Users/neslihanonay/Desktop/output_129.xlsx") #two things to update: path(put your own path, and subject number)
