library(dplyr)
library(tidyr)
library(ggplot2)
library(tidytext)
library(tidyverse)
library(imputesTS)
library(naniar)
library(readxl)
library(hablar)
library(lubridate)
library(daff)
library(tibble)
library(stringr)

new <- read.csv("~/Downloads/_ZIC_Reserve_Review_Weekly_20211206_0000.csv", skip=1, header=TRUE)
FTE <- read.csv("T:/HKGI/Claims/Express Hub/Business Intelligence and Automation/Dashboard/Data source/FTE.csv")
EP <- read.csv("~/Downloads//EPFLOW_-_Claims_-_Case_follow-up_report_-_Daily_-_All__-__20211206.csv", skip = 9, header = TRUE)

old <- read.csv("~/Downloads/_ZIC_Reserve_Review_Weekly_20211129_0000.csv", skip=1, header=TRUE)
FTE <- read.csv("T:/HKGI/Claims/Express Hub/Business Intelligence and Automation/Dashboard/Data source/FTE.csv")
oldEP <- read.csv("~/Downloads//EPFLOW_-_Claims_-_Case_follow-up_report_-_Daily_-_All__-__20211129.csv", skip = 9, header = TRUE)


new <- new%>%
  mutate(date= as.Date('2021-12-06'))

old <- old%>%
  mutate(date= as.Date('2021-11-29'))
#new dataset -----------------------------------------
new <- head(new,-3)



EP <- EP %>% 
  rename(Epflow.Case.ID = Case.ID,
         Status = Case.status,
         Claim.ID = Claim.no.
  )

EP <- EP%>%
  select(c(Epflow.Case.ID, Status,Claim.ID))

FTE <- FTE%>%
  rename(Handler.Name = Name.1)

FTE <- FTE%>%
  select(-c(LOB))

new$DOA<- as.character(new$DOA)
new$Case.Registration.date<- as.character(new$Case.Registration.date)
new$Date.reserve.last.amended<- as.character(new$Date.reserve.last.amended)
new$Date.Confirm.Reserve.Button.Last.Triggered<- as.character(new$Date.Confirm.Reserve.Button.Last.Triggered)
new$Latest.Reserve.amendment.or.Confirm.Reserve.button.triggered<- as.character(new$Latest.Reserve.amendment.or.Confirm.Reserve.button.triggered)
new$Next.Review.Due.Date<- as.character(new$Next.Review.Due.Date)

#from character to date 
new$DOA<-mdy(new$DOA)
new$Case.Registration.date<-mdy(new$Case.Registration.date)
new$Date.reserve.last.amended<-mdy(new$Date.reserve.last.amended)
new$Date.Confirm.Reserve.Button.Last.Triggered<-mdy(new$Date.Confirm.Reserve.Button.Last.Triggered)
new$Latest.Reserve.amendment.or.Confirm.Reserve.button.triggered<-mdy(new$Latest.Reserve.amendment.or.Confirm.Reserve.button.triggered)
new$Next.Review.Due.Date<-mdy(new$Next.Review.Due.Date)

new <- new%>%
  mutate(date= as.Date('2021-12-06'))

new$date <- as.Date(new$date)

new <- right_join(FTE,new)

new <- right_join(EP,new)
new <- new %>% distinct(across(everything()))

new$Epflow.Case.ID <- as.character(new$Epflow.Case.ID)
new$Status <- as.character(new$Status)
new$Team <- as.character(new$Team)
new$Epflow.Case.ID[new$Epflow.Case.ID == ""] <- 'blank'
new <- new

new$Status[is.na(new$Status)] <- 'NA'
new <- new

new$Team[is.na(new$Team)] <- 'NA'
new <- new


new <- new %>% 
  mutate(action = case_when(Epflow.Case.ID == "blank" & Status == "NA"  ~ "MR",
                          Status == "NA" | Status == "Payment (including insured)"  ~ "RPA",
                          TRUE ~ "MR"))


check_Raven <- new%>% filter(str_detect(new$Handler.Name,"RAVEN LI"))


#Employee status

new <- new %>%
  mutate(`Emplyee status` = case_when(str_detect(Team, 'NA') ~ 'F',
                                      TRUE ~'T'))

#Day since latest reserve amendment or confirm reserve button triggered 

new$Date.Confirm.Reserve.Button.Last.Triggered <- as.Date(new$Date.Confirm.Reserve.Button.Last.Triggered)


new <- new%>%
  mutate(`Day since latest reserve amendment or confirm reserve button triggered`=difftime(date,Latest.Reserve.amendment.or.Confirm.Reserve.button.triggered, units = "days"))

# Age case different 
new <- new%>%
  mutate(`Age case date diff`= difftime(date,DOA))

new <- new %>%
  mutate(`Age case` = case_when(`Age case date diff` >= 1095 ~ 'Age case'
                                ,TRUE ~ 'NA'))

#Day since registration
new <- new%>%
  mutate(`Day since registration`=difftime(date,Case.Registration.date))

#Day till next review
new <- new%>%
  mutate(`Day till next review`=difftime( Next.Review.Due.Date,date, units="days"))



# Day diff of  Latest Reserve amendment or Confirm Reserve button triggered


new <- new%>%
  mutate(Day_diff_Laste_Reseve_or_confir_button_triggered =difftime( new$date, new$Latest.Reserve.amendment.or.Confirm.Reserve.button.triggered))

#review status new 
new$Latest.Reserve.amendment.or.Confirm.Reserve.button.triggered <- as.character(new$Latest.Reserve.amendment.or.Confirm.Reserve.button.triggered)

new$Review.Status <- as.character(new$Review.Status)


new <- new%>%
  mutate (`Review status new` = case_when (Latest.Reserve.amendment.or.Confirm.Reserve.button.triggered >= 0 & Latest.Reserve.amendment.or.Confirm.Reserve.button.triggered <=7  ~ 'Due this week',
                                           TRUE                      ~ Review.Status))

#find out pegky case RI.Flag  
new$RI.Flag <- as.character(new$RI.Flag)

peg <- new%>% filter(str_detect(new$RI.Flag,"Y"))
non_peg <- new%>% filter(!str_detect(new$RI.Flag,"Y")) 

# pegky overdue #Review status pegky subset all RI flag = yes  overdue> 180days
peg <- peg%>%
  mutate(Review_status_pegky= case_when (Day_diff_Laste_Reseve_or_confir_button_triggered > 178 & Day_diff_Laste_Reseve_or_confir_button_triggered < 187  ~ 'Due this week',
                                         TRUE                      ~ Review.Status))

non_peg <- non_peg%>%
  mutate(Review_status_pegky = Review.Status )

new <- bind_rows(peg, non_peg)

# Review status retail property = [Day till next review]>=0,'Reserve review'[Day till next review]<7) rerturn due this week 
new <- new%>%
  mutate(Review_status_retail_property = case_when(`Day till next review` >=0 & `Day till next review` < 7 ~ 'Due this week',
                                                   TRUE   ~ Review.Status) )

#Flag 
new$Handler.Name <- as.character(new$Handler.Name)
new$Review.Status <- as.character(new$Review.Status)
new$`Emplyee status` <- as.character(new$`Emplyee status`)

new$Outstanding.reserve.amount <- as.numeric(new$Outstanding.reserve.amount)
new$Number.of.Days.Overdue <- as.numeric(new$Number.of.Days.Overdue)


#try 
new <- new %>% 
  mutate(Flag = case_when(Number.of.Days.Overdue > 60 & Handler.Name == "PEGKY SHEH"  ~ "Pegky's over 60 days overdue",
                          Number.of.Days.Overdue > 59 & Handler.Name == "PEGKY SHEH"  ~ "Pegky's 180 days overdue cases",
                          `Emplyee status` == "F" & Team =="NA" ~ "Non-existent handler-name",
                          Review.Status == "Overdue" ~ "120 days overdue cases",
                          Outstanding.reserve.amount == 0 ~ "HKD 0 reserve",
                          Outstanding.reserve.amount > 0 & Outstanding.reserve.amount <= 50  ~ " HKD 0 & <= HKD 50 reserve",
                          TRUE ~ "other"))

new$Flag <- as.factor(new$Flag)

#Overdue reason 

new$`Day since latest reserve amendment or confirm reserve button triggered` <- as.character(new$`Day since latest reserve amendment or confirm reserve button triggered`)
new$`Day since latest reserve amendment or confirm reserve button triggered`[is.na(new$`Day since latest reserve amendment or confirm reserve button triggered`)] <- 'NA'
new <- new


newisnan <- new%>% filter(str_detect(new$`Day since latest reserve amendment or confirm reserve button triggered` ,"NA"))
newnon <- new%>% filter(!str_detect(new$`Day since latest reserve amendment or confirm reserve button triggered` ,"NA")) 


newisnan <- newisnan%>%
  mutate(`Overdue Reason` = "No review for initial reserve over 5 days")

newnon$`Day since latest reserve amendment or confirm reserve button triggered` <- as.numeric(newnon$`Day since latest reserve amendment or confirm reserve button triggered`)

newnon <- newnon%>%
  mutate(`Overdue Reason`= case_when(`Day since latest reserve amendment or confirm reserve button triggered` > 60  ~ "Last review over 2 months",
                                     TRUE ~ "NA" ))

newisnan$`Day since latest reserve amendment or confirm reserve button triggered` <- as.numeric(newisnan$`Day since latest reserve amendment or confirm reserve button triggered`)


new <- bind_rows(newisnan,newnon)

##Pre aged case 

new <- new%>%
  mutate(DOA_Day_diff =difftime(new$date,new$DOA))


new <- new %>% 
  mutate(Pre_age_case = case_when(DOA_Day_diff > 913 & DOA_Day_diff < 1095  ~ "Age case",
                                  TRUE ~ "NA"))


#Reserve review cases flag 

new <- new %>% 
  mutate(Reserve_review_cases_flag = case_when(`Day since latest reserve amendment or confirm reserve button triggered` > 60   ~ "T",
                                               Number.of.Days.Overdue > 0   ~ "T",
                                               TRUE ~ "F"))

# RPA Flag


new <- new %>% 
  mutate(RPA_flag = case_when(Status == "Payment (including insured)" & Review.Status  == "Overdue - Others"  ~ "T",
                              Status == "Payment (including insured)" & Review.Status  == "To be reviewed in next 7 day"  ~ "T",
                              Status == "NA" & Review.Status  == "Overdue - Others"  ~ "T",
                              Status == "NA" & Review.Status  == "To be reviewed in next 7 days"  ~ "T",
                              Status == "NA" & Review.Status  == "To be reviewed in next 30 days"  ~ "T",
                              TRUE ~ "F"))


new <- bind_rows(newMR,newRPA)


new <- new %>% 
  mutate(`Non-existent handler-name` = case_when(`Emplyee status` == "F" & Team =="NA" ~ "Non-existent handler-name",
                                                     TRUE ~ "NA"))
new <- new %>% 
  mutate(`120 days overdue cases` = case_when(Review.Status == "Overdue" ~ "120 days overdue cases",
                                                     TRUE ~ "NA"))
new <- new %>% 
  mutate(`HKD 0 reserve` = case_when(Outstanding.reserve.amount == 0 ~ "HKD 0 reserve",
                                                     TRUE ~ "NA"))
new <- new %>% 
  mutate(`HKD 0 & <= HKD 50 reserve` = case_when(Outstanding.reserve.amount > 0 & Outstanding.reserve.amount <= 50  ~ " HKD 0 & <= HKD 50 reserve",
                                                     TRUE ~ "NA"))



write.csv(new,"T:\\HKGI\\Claims\\Express Hub\\Business Intelligence and Automation\\Dashboard\\Data source\\Wesley\\Reserve review\\reserve_new.csv", row.names = FALSE)



#last week dataset -----------------------------------
old <- head(old,-3)



oldEP <- oldEP %>% 
  rename(Epflow.Case.ID = Case.ID,
         Status = Case.status,
         Claim.ID = Claim.no.
  )

oldEP <- oldEP%>%
  select(c(Epflow.Case.ID, Status,Claim.ID))

FTE <- FTE%>%
  rename(Handler.Name = Name.1)

FTE <- FTE%>%
  select(-c(LOB))

old$DOA<- as.character(old$DOA)
old$Case.Registration.date<- as.character(old$Case.Registration.date)
old$Date.reserve.last.amended<- as.character(old$Date.reserve.last.amended)
old$Date.Confirm.Reserve.Button.Last.Triggered<- as.character(old$Date.Confirm.Reserve.Button.Last.Triggered)
old$Latest.Reserve.amendment.or.Confirm.Reserve.button.triggered<- as.character(old$Latest.Reserve.amendment.or.Confirm.Reserve.button.triggered)
old$Next.Review.Due.Date<- as.character(old$Next.Review.Due.Date)

#from character to date 
old$DOA<-mdy(old$DOA)
old$Case.Registration.date<-mdy(old$Case.Registration.date)
old$Date.reserve.last.amended<-mdy(old$Date.reserve.last.amended)
old$Date.Confirm.Reserve.Button.Last.Triggered<-mdy(old$Date.Confirm.Reserve.Button.Last.Triggered)
old$Latest.Reserve.amendment.or.Confirm.Reserve.button.triggered<-mdy(old$Latest.Reserve.amendment.or.Confirm.Reserve.button.triggered)
old$Next.Review.Due.Date<-mdy(old$Next.Review.Due.Date)



old$date <- as.Date(old$date)

old <- right_join(FTE,old)

old <- right_join(EP,old)
old <- old %>% distinct(across(everything()))

old$Epflow.Case.ID <- as.character(old$Epflow.Case.ID)
old$Status <- as.character(old$Status)
old$Team <- as.character(old$Team)
old$Epflow.Case.ID[old$Epflow.Case.ID == ""] <- 'blank'
old <- old

old$Status[is.na(old$Status)] <- 'NA'
old <- old

old$Team[is.na(old$Team)] <- 'NA'
old <- old

old <- old %>%
  mutate(action = case_when(str_detect(Epflow.Case.ID, 'blank') ~ 'MR',
                            str_detect(Status, 'NA') ~ 'RPA',
                            str_detect(Status, 'including') ~ 'RPA',
                            TRUE ~ 'MR'))


check_Raven <- old%>% filter(str_detect(old$Handler.Name,"RAVEN LI"))


#Employee status

old <- old %>%
  mutate(`Emplyee status` = case_when(str_detect(Team, 'NA') ~ 'F',
                                      TRUE ~'T'))

#Day since latest reserve amendment or confirm reserve button triggered 

old$Date.Confirm.Reserve.Button.Last.Triggered <- as.Date(old$Date.Confirm.Reserve.Button.Last.Triggered)


old <- old%>%
  mutate(`Day since latest reserve amendment or confirm reserve button triggered`=difftime(date,Latest.Reserve.amendment.or.Confirm.Reserve.button.triggered, units = "days"))

# Age case different 
old <- old%>%
  mutate(`Age case date diff`= difftime(date,DOA))

old <- old %>%
  mutate(`Age case` = case_when(`Age case date diff` >= 1095 ~ 'Age case'
                                ,TRUE ~ 'NA'))

#Day since registration
old <- old%>%
  mutate(`Day since registration`=difftime(date,Case.Registration.date))

#Day till next review
old <- old%>%
  mutate(`Day till next review`=difftime( Next.Review.Due.Date,date, units="days"))



# Day diff of  Latest Reserve amendment or Confirm Reserve button triggered


old <- old%>%
  mutate(Day_diff_Laste_Reseve_or_confir_button_triggered =difftime( old$date, old$Latest.Reserve.amendment.or.Confirm.Reserve.button.triggered))

#review status old 
old$Latest.Reserve.amendment.or.Confirm.Reserve.button.triggered <- as.character(old$Latest.Reserve.amendment.or.Confirm.Reserve.button.triggered)

old$Review.Status <- as.character(old$Review.Status)


old <- old%>%
  mutate (`Review status new` = case_when (Latest.Reserve.amendment.or.Confirm.Reserve.button.triggered >= 0 & Latest.Reserve.amendment.or.Confirm.Reserve.button.triggered <=7  ~ 'Due this week',
                                           TRUE                      ~ Review.Status))

#find out pegky case RI.Flag  
old$RI.Flag <- as.character(old$RI.Flag)

peg <- old%>% filter(str_detect(old$RI.Flag,"Y"))
non_peg <- old%>% filter(!str_detect(old$RI.Flag,"Y")) 

# pegky overdue #Review status pegky subset all RI flag = yes  overdue> 180days
peg <- peg%>%
  mutate(Review_status_pegky= case_when (Day_diff_Laste_Reseve_or_confir_button_triggered > 178 & Day_diff_Laste_Reseve_or_confir_button_triggered < 187  ~ 'Due this week',
                                         TRUE                      ~ Review.Status))

non_peg <- non_peg%>%
  mutate(Review_status_pegky = Review.Status )

old <- bind_rows(peg, non_peg)

# Review status retail property = [Day till next review]>=0,'Reserve review'[Day till next review]<7) rerturn due this week 
Overdue - Others
Overdue

old <- old %>%
  mutate(Review.Status_  = case_when(str_detect(Review.Status, 'Others') ~ 'Overdue - Others (last)',
                                     str_detect(Review.Status, 'Overdue') ~ 'Overdue (last)',
                                     TRUE ~ Review.Status))



old <- old%>%
  mutate(last_week_Review_status_retail_property  = case_when(`Day till next review` >=0 & `Day till next review` < 7 ~ 'Due this week(last)',
                                                              TRUE   ~ Review.Status_ ) )

#Flag 
old$Handler.Name <- as.character(old$Handler.Name)
old$Review.Status <- as.character(old$Review.Status)
old$`Emplyee status` <- as.character(old$`Emplyee status`)

old$Outstanding.reserve.amount <- as.numeric(old$Outstanding.reserve.amount)
old$Number.of.Days.Overdue <- as.numeric(old$Number.of.Days.Overdue)


#try 
old <- old %>% 
  mutate(Flag = case_when(Number.of.Days.Overdue > 60 & Handler.Name == "PEGKY SHEH"  ~ "Pegky's over 60 days overdue",
                          Number.of.Days.Overdue > 59 & Handler.Name == "PEGKY SHEH"  ~ "Pegky's 180 days overdue cases",
                          `Emplyee status` == "F" & Team =="NA" ~ "Non-existent handler-name",
                          Review.Status == "Overdue" ~ "120 days overdue cases",
                          Outstanding.reserve.amount == 0 ~ "HKD 0 reserve",
                          Outstanding.reserve.amount > 0 & Outstanding.reserve.amount <= 50  ~ " HKD 0 & <= HKD 50 reserve",
                          TRUE ~ "other"))

old$Flag <- as.factor(old$Flag)

#Overdue reason 

old$`Day since latest reserve amendment or confirm reserve button triggered` <- as.character(old$`Day since latest reserve amendment or confirm reserve button triggered`)
old$`Day since latest reserve amendment or confirm reserve button triggered`[is.na(old$`Day since latest reserve amendment or confirm reserve button triggered`)] <- 'NA'
old <- old


oldisnan <- old%>% filter(str_detect(old$`Day since latest reserve amendment or confirm reserve button triggered` ,"NA"))
oldnon <- old%>% filter(!str_detect(old$`Day since latest reserve amendment or confirm reserve button triggered` ,"NA")) 


oldisnan <- oldisnan%>%
  mutate(`Overdue Reason` = "No review for initial reserve over 5 days")

oldnon$`Day since latest reserve amendment or confirm reserve button triggered` <- as.numeric(oldnon$`Day since latest reserve amendment or confirm reserve button triggered`)

oldnon <- oldnon%>%
  mutate(`Overdue Reason`= case_when(`Day since latest reserve amendment or confirm reserve button triggered` > 60  ~ "Last review over 2 months",
                                     TRUE ~ "NA" ))

oldisnan$`Day since latest reserve amendment or confirm reserve button triggered` <- as.numeric(oldisnan$`Day since latest reserve amendment or confirm reserve button triggered`)


old <- bind_rows(oldisnan,oldnon)

##Pre aged case 

old <- old%>%
  mutate(DOA_Day_diff =difftime(old$date,old$DOA))


old <- old %>% 
  mutate(Pre_age_case = case_when(DOA_Day_diff > 913 & DOA_Day_diff < 1095  ~ "Age case",
                                  TRUE ~ "NA"))


#Reserve review cases flag 

old <- old %>% 
  mutate(Reserve_review_cases_flag = case_when(`Day since latest reserve amendment or confirm reserve button triggered` > 60   ~ "T",
                                               Number.of.Days.Overdue > 0   ~ "T",
                                               TRUE ~ "F"))

# RPA Flag


oldRPA <- old%>% filter(str_detect(old$action,"RPA"))
oldMR <- old%>% filter(!str_detect(old$action,"RPA")) 

oldMR <- oldMR%>%
  mutate(RPA_flag = 'F')



oldRPA <- oldRPA %>% mutate(RPA_flag = case_when(str_detect(Review.Status, 'Overdue - Others') ~ 'T',
                                                 str_detect(Review.Status, 'To be reviewed in next 7 days') ~ 'T',
                                                 str_detect(Review.Status, 'To be reviewed in next 30 days') ~ 'T',
                                                 TRUE ~ 'F'))

old <- bind_rows(oldMR,oldRPA)


old <- old %>% 
  mutate(`Non-existent handler-name` = case_when(`Emplyee status` == "F" & Team =="NA" ~ "Non-existent handler-name",
                                                 TRUE ~ "NA"))
old <- old %>% 
  mutate(`120 days overdue cases` = case_when(Review.Status == "Overdue" ~ "120 days overdue cases",
                                              TRUE ~ "NA"))
old <- old %>% 
  mutate(`HKD 0 reserve` = case_when(Outstanding.reserve.amount == 0 ~ "HKD 0 reserve",
                                     TRUE ~ "NA"))
old <- old %>% 
  mutate(`HKD 0 & <= HKD 50 reserve` = case_when(Outstanding.reserve.amount > 0 & Outstanding.reserve.amount <= 50  ~ " HKD 0 & <= HKD 50 reserve",
                                                 TRUE ~ "NA"))



write.csv(old,"T:\\HKGI\\Claims\\Express Hub\\Business Intelligence and Automation\\Dashboard\\Data source\\Wesley\\Reserve review\\reserve_old.csv", row.names = FALSE)




