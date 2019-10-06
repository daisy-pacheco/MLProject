library(dplyr)

# switching variable names to words vs codes
varNames <- read.csv("varNames.csv", header = FALSE)

names(trainData79) <- varNames[match(names(trainData79), varNames[ ,'V1']),'V3']


# making a tiny sample for testing feature engineering 
set.seed(456) 

tinySample79 <- sample_n(trainData79, 20)


### FEATURE ENGINEERING ###

# finding most recent job for 1979 - 1993
## *** DEFINITELY IMPROVE THIS ***

#1979
stopweeksTrain79 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1979'))

stopweeksTrain79[is.na(stopweeksTrain79)] <- 0

trainData79$LastJob79 <- colnames(stopweeksTrain79)[apply(stopweeksTrain79, 1, which.max)]
trainData79$JOB_SATISFACTION.01_1979 <- NA
trainData79$JOB_SATISFACTION.02_1979 <- NA
trainData79$JOB_SATISFACTION.03_1979 <- NA
trainData79$JOB_SATISFACTION.04_1979 <- NA
trainData79$JOB_SATISFACTION.05_1979 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"LastJob79"] =="EMPLOYERS_ALL_STOPWEEK_1979.01_E0050100"){
    trainData79[i,"JOB_SATISFACTION.01_1979"] <- trainData79[i,"JOB_SATISFACTION_R0050800"]
  }else if (trainData79[i,"LastJob79"] == "EMPLOYERS_ALL_STOPWEEK_1979.02_E0050200"){
    trainData79[i,"JOB_SATISFACTION.02_1979"] <- trainData79[i,"JOB_SATISFACTION_R0050800"]
  }else if (trainData79[i,"LastJob79"] == "EMPLOYERS_ALL_STOPWEEK_1979.03_E0050300"){
    trainData79[i,"JOB_SATISFACTION.03_1979"] <- trainData79[i,"JOB_SATISFACTION_R0050800"]
  }else if (trainData79[i,"LastJob79"] == "EMPLOYERS_ALL_STOPWEEK_1979.04_E0050400"){
    trainData79[i,"JOB_SATISFACTION.04_1979"] <- trainData79[i,"JOB_SATISFACTION_R0050800"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1979"] <- trainData79[i,"JOB_SATISFACTION_R0050800"]
  }
}



#1980
stopweeksTrain80 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1980'))

stopweeksTrain80[is.na(stopweeksTrain80)] <- 0

trainData79$LastJob80 <- colnames(stopweeksTrain80)[apply(stopweeksTrain80, 1, which.max)]
trainData79$JOB_SATISFACTION.01_1980 <- NA
trainData79$JOB_SATISFACTION.02_1980 <- NA
trainData79$JOB_SATISFACTION.03_1980 <- NA
trainData79$JOB_SATISFACTION.04_1980 <- NA
trainData79$JOB_SATISFACTION.05_1980 <- NA
trainData79$JOB_SATISFACTION.06_1980 <- NA
trainData79$JOB_SATISFACTION.07_1980 <- NA
trainData79$JOB_SATISFACTION.08_1980 <- NA
trainData79$JOB_SATISFACTION.09_1980 <- NA
trainData79$JOB_SATISFACTION.10_1980 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"LastJob80"] =="EMPLOYERS_ALL_STOPWEEK_1980.01_E0060100"){
    trainData79[i,"JOB_SATISFACTION.01_1980"] <- trainData79[i,"JOB_SATISFACTION_R0267800"]
  }else if (trainData79[i,"LastJob80"] == "EMPLOYERS_ALL_STOPWEEK_1980.02_E0060200"){
    trainData79[i,"JOB_SATISFACTION.02_1980"] <- trainData79[i,"JOB_SATISFACTION_R0267800"]
  }else if (trainData79[i,"LastJob80"] == "EMPLOYERS_ALL_STOPWEEK_1980.03_E0060300"){
    trainData79[i,"JOB_SATISFACTION.03_1980"] <- trainData79[i,"JOB_SATISFACTION_R0267800"]
  }else if (trainData79[i,"LastJob80"] == "EMPLOYERS_ALL_STOPWEEK_1980.04_E0060400"){
    trainData79[i,"JOB_SATISFACTION.04_1980"] <- trainData79[i,"JOB_SATISFACTION_R0267800"]
  }else if (trainData79[i,"LastJob80"] == "EMPLOYERS_ALL_STOPWEEK_1980.05_E0060500"){
    trainData79[i,"JOB_SATISFACTION.05_1980"] <- trainData79[i,"JOB_SATISFACTION_R0267800"]
  }else if (trainData79[i,"LastJob80"] == "EMPLOYERS_ALL_STOPWEEK_1980.06_E0060600"){
    trainData79[i,"JOB_SATISFACTION.06_1980"] <- trainData79[i,"JOB_SATISFACTION_R0267800"]
  }else if (trainData79[i,"LastJob80"] == "EMPLOYERS_ALL_STOPWEEK_1980.07_E0060700"){
    trainData79[i,"JOB_SATISFACTION.07_1980"] <- trainData79[i,"JOB_SATISFACTION_R0267800"]
  }else if (trainData79[i,"LastJob80"] == "EMPLOYERS_ALL_STOPWEEK_1980.08_E0060800"){
    trainData79[i,"JOB_SATISFACTION.08_1980"] <- trainData79[i,"JOB_SATISFACTION_R0267800"]
  }else if (trainData79[i,"LastJob80"] == "EMPLOYERS_ALL_STOPWEEK_1980.09_E0060900"){
    trainData79[i,"JOB_SATISFACTION.09_1980"] <- trainData79[i,"JOB_SATISFACTION_R0267800"]
  }else{
    trainData79[i,"JOB_SATISFACTION.10_1980"] <- trainData79[i,"JOB_SATISFACTION_R0267800"]
  }
}


#1981
stopweeksTrain81 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1981'))

stopweeksTrain81[is.na(stopweeksTrain81)] <- 0

trainData79$LastJob81 <- colnames(stopweeksTrain81)[apply(stopweeksTrain81, 1, which.max)]
trainData79$JOB_SATISFACTION.01_1981 <- NA
trainData79$JOB_SATISFACTION.02_1981 <- NA
trainData79$JOB_SATISFACTION.03_1981 <- NA
trainData79$JOB_SATISFACTION.04_1981 <- NA
trainData79$JOB_SATISFACTION.05_1981 <- NA
trainData79$JOB_SATISFACTION.06_1981 <- NA
trainData79$JOB_SATISFACTION.07_1981 <- NA
trainData79$JOB_SATISFACTION.08_1981 <- NA
trainData79$JOB_SATISFACTION.09_1981 <- NA
trainData79$JOB_SATISFACTION.10_1981 <- NA
trainData79$JOB_SATISFACTION.11_1981 <- NA
trainData79$JOB_SATISFACTION.12_1981 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"LastJob81"] =="EMPLOYERS_ALL_STOPWEEK_1981.01_E0070100"){
    trainData79[i,"JOB_SATISFACTION.01_1981"] <- trainData79[i,"JOB_SATISFACTION_R0449200"]
  }else if (trainData79[i,"LastJob81"] == "EMPLOYERS_ALL_STOPWEEK_1981.02_E0070200"){
    trainData79[i,"JOB_SATISFACTION.02_1981"] <- trainData79[i,"JOB_SATISFACTION_R0449200"]
  }else if (trainData79[i,"LastJob81"] == "EMPLOYERS_ALL_STOPWEEK_1981.03_E0070300"){
    trainData79[i,"JOB_SATISFACTION.03_1981"] <- trainData79[i,"JOB_SATISFACTION_R0449200"]
  }else if (trainData79[i,"LastJob81"] == "EMPLOYERS_ALL_STOPWEEK_1981.04_E0070400"){
    trainData79[i,"JOB_SATISFACTION.04_1981"] <- trainData79[i,"JOB_SATISFACTION_R0449200"]
  }else if (trainData79[i,"LastJob81"] == "EMPLOYERS_ALL_STOPWEEK_1981.05_E0070500"){
    trainData79[i,"JOB_SATISFACTION.05_1981"] <- trainData79[i,"JOB_SATISFACTION_R0449200"]
  }else if (trainData79[i,"LastJob81"] == "EMPLOYERS_ALL_STOPWEEK_1981.06_E0070600"){
    trainData79[i,"JOB_SATISFACTION.06_1981"] <- trainData79[i,"JOB_SATISFACTION_R0449200"]
  }else if (trainData79[i,"LastJob81"] == "EMPLOYERS_ALL_STOPWEEK_1981.07_E0070700"){
    trainData79[i,"JOB_SATISFACTION.07_1981"] <- trainData79[i,"JOB_SATISFACTION_R0449200"]
  }else if (trainData79[i,"LastJob81"] == "EMPLOYERS_ALL_STOPWEEK_1981.08_E0070800"){
    trainData79[i,"JOB_SATISFACTION.08_1981"] <- trainData79[i,"JOB_SATISFACTION_R0449200"]
  }else if (trainData79[i,"LastJob81"] == "EMPLOYERS_ALL_STOPWEEK_1981.09_E0070900"){
    trainData79[i,"JOB_SATISFACTION.09_1981"] <- trainData79[i,"JOB_SATISFACTION_R0449200"]
  }else if (trainData79[i,"LastJob81"] == "EMPLOYERS_ALL_STOPWEEK_1981.10_E0071000"){
    trainData79[i,"JOB_SATISFACTION.10_1981"] <- trainData79[i,"JOB_SATISFACTION_R0449200"]
  }else if (trainData79[i,"LastJob81"] == "EMPLOYERS_ALL_STOPWEEK_1981.10_E0071000"){
    trainData79[i,"JOB_SATISFACTION.11_1981"] <- trainData79[i,"JOB_SATISFACTION_R0449200"]
  }else{
    trainData79[i,"JOB_SATISFACTION.12_1981"] <- trainData79[i,"JOB_SATISFACTION_R0449200"]
  }
}


#1982
stopweeksTrain82 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1982'))

stopweeksTrain82[is.na(stopweeksTrain82)] <- 0

trainData79$LastJob82 <- colnames(stopweeksTrain82)[apply(stopweeksTrain82, 1, which.max)]
trainData79$JOB_SATISFACTION.01_1982 <- NA
trainData79$JOB_SATISFACTION.02_1982 <- NA
trainData79$JOB_SATISFACTION.03_1982 <- NA
trainData79$JOB_SATISFACTION.04_1982 <- NA
trainData79$JOB_SATISFACTION.05_1982 <- NA

unique(trainData79$LastJob84)


#1983
stopweeksTrain83 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1983'))

stopweeksTrain83[is.na(stopweeksTrain83)] <- 0

trainData79$LastJob83 <- colnames(stopweeksTrain83)[apply(stopweeksTrain83, 1, which.max)]
trainData79$JOB_SATISFACTION.01_1983 <- NA
trainData79$JOB_SATISFACTION.02_1983 <- NA
trainData79$JOB_SATISFACTION.03_1983 <- NA
trainData79$JOB_SATISFACTION.04_1983 <- NA
trainData79$JOB_SATISFACTION.05_1983 <- NA

#1984
stopweeksTrain84 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1984'))

stopweeksTrain84[is.na(stopweeksTrain84)] <- 0

trainData79$LastJob84 <- colnames(stopweeksTrain84)[apply(stopweeksTrain84, 1, which.max)]
trainData79$JOB_SATISFACTION.01_1984 <- NA
trainData79$JOB_SATISFACTION.02_1984 <- NA
trainData79$JOB_SATISFACTION.03_1984 <- NA
trainData79$JOB_SATISFACTION.04_1984 <- NA
trainData79$JOB_SATISFACTION.05_1984 <- NA

#1985
stopweeksTrain85 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1985'))

stopweeksTrain85[is.na(stopweeksTrain85)] <- 0

trainData79$LastJob85 <- colnames(stopweeksTrain85)[apply(stopweeksTrain85, 1, which.max)]
trainData79$JOB_SATISFACTION.01_1985 <- NA
trainData79$JOB_SATISFACTION.02_1985 <- NA
trainData79$JOB_SATISFACTION.03_1985 <- NA
trainData79$JOB_SATISFACTION.04_1985 <- NA
trainData79$JOB_SATISFACTION.05_1985 <- NA

#1986
stopweeksTrain86 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1986'))

stopweeksTrain86[is.na(stopweeksTrain86)] <- 0

trainData79$LastJob86 <- colnames(stopweeksTrain86)[apply(stopweeksTrain86, 1, which.max)]
trainData79$JOB_SATISFACTION.01_1986 <- NA
trainData79$JOB_SATISFACTION.02_1986 <- NA
trainData79$JOB_SATISFACTION.03_1986 <- NA
trainData79$JOB_SATISFACTION.04_1986 <- NA
trainData79$JOB_SATISFACTION.05_1986 <- NA

#1987
stopweeksTrain87 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1987'))

stopweeksTrain87[is.na(stopweeksTrain87)] <- 0

trainData79$LastJob87 <- colnames(stopweeksTrain87)[apply(stopweeksTrain87, 1, which.max)]
trainData79$JOB_SATISFACTION.01_1987 <- NA
trainData79$JOB_SATISFACTION.02_1987 <- NA
trainData79$JOB_SATISFACTION.03_1987 <- NA
trainData79$JOB_SATISFACTION.04_1987 <- NA
trainData79$JOB_SATISFACTION.05_1987 <- NA

#1988
stopweeksTrain88 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1988'))

stopweeksTrain88[is.na(stopweeksTrain88)] <- 0

trainData79$LastJob88 <- colnames(stopweeksTrain88)[apply(stopweeksTrain88, 1, which.max)]
trainData79$JOB_SATISFACTION.01_1988 <- NA
trainData79$JOB_SATISFACTION.02_1988 <- NA
trainData79$JOB_SATISFACTION.03_1988 <- NA
trainData79$JOB_SATISFACTION.04_1988 <- NA
trainData79$JOB_SATISFACTION.05_1988 <- NA

#1989
stopweeksTrain89 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1989'))

stopweeksTrain89[is.na(stopweeksTrain89)] <- 0

trainData79$LastJob89 <- colnames(stopweeksTrain89)[apply(stopweeksTrain89, 1, which.max)]
trainData79$JOB_SATISFACTION.01_1989 <- NA
trainData79$JOB_SATISFACTION.02_1989 <- NA
trainData79$JOB_SATISFACTION.03_1989 <- NA
trainData79$JOB_SATISFACTION.04_1989 <- NA
trainData79$JOB_SATISFACTION.05_1989 <- NA

#1990
stopweeksTrain90 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1990'))

stopweeksTrain90[is.na(stopweeksTrain90)] <- 0

trainData79$LastJob90 <- colnames(stopweeksTrain90)[apply(stopweeksTrain90, 1, which.max)]
trainData79$JOB_SATISFACTION.01_1990 <- NA
trainData79$JOB_SATISFACTION.02_1990 <- NA
trainData79$JOB_SATISFACTION.03_1990 <- NA
trainData79$JOB_SATISFACTION.04_1990 <- NA
trainData79$JOB_SATISFACTION.05_1990 <- NA

#1991
stopweeksTrain91 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1991'))

stopweeksTrain91[is.na(stopweeksTrain91)] <- 0

trainData79$LastJob91 <- colnames(stopweeksTrain91)[apply(stopweeksTrain91, 1, which.max)]
trainData79$JOB_SATISFACTION.01_1991 <- NA
trainData79$JOB_SATISFACTION.02_1991 <- NA
trainData79$JOB_SATISFACTION.03_1991 <- NA
trainData79$JOB_SATISFACTION.04_1991 <- NA
trainData79$JOB_SATISFACTION.05_1991 <- NA

#1992
stopweeksTrain92 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1992'))

stopweeksTrain92[is.na(stopweeksTrain92)] <- 0

trainData79$LastJob92 <- colnames(stopweeksTrain92)[apply(stopweeksTrain92, 1, which.max)]
trainData79$JOB_SATISFACTION.01_1992 <- NA
trainData79$JOB_SATISFACTION.02_1992 <- NA
trainData79$JOB_SATISFACTION.03_1992 <- NA
trainData79$JOB_SATISFACTION.04_1992 <- NA
trainData79$JOB_SATISFACTION.05_1992 <- NA

#1993
stopweeksTrain93 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1993'))

stopweeksTrain93[is.na(stopweeksTrain93)] <- 0

trainData79$LastJob93 <- colnames(stopweeksTrain93)[apply(stopweeksTrain93, 1, which.max)]
trainData79$JOB_SATISFACTION.01_1993 <- NA
trainData79$JOB_SATISFACTION.02_1993 <- NA
trainData79$JOB_SATISFACTION.03_1993 <- NA
trainData79$JOB_SATISFACTION.04_1993 <- NA
trainData79$JOB_SATISFACTION.05_1993 <- NA
