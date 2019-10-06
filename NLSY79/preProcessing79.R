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

#1980
stopweeksTrain80 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1980'))

stopweeksTrain80[is.na(stopweeksTrain80)] <- 0

trainData79$LastJob80 <- colnames(stopweeksTrain80)[apply(stopweeksTrain80, 1, which.max)]

#1981
stopweeksTrain81 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1981'))

stopweeksTrain81[is.na(stopweeksTrain81)] <- 0

trainData79$LastJob81 <- colnames(stopweeksTrain81)[apply(stopweeksTrain81, 1, which.max)]

#1982
stopweeksTrain82 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1982'))

stopweeksTrain82[is.na(stopweeksTrain82)] <- 0

trainData79$LastJob82 <- colnames(stopweeksTrain82)[apply(stopweeksTrain82, 1, which.max)]

#1983
stopweeksTrain83 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1983'))

stopweeksTrain83[is.na(stopweeksTrain83)] <- 0

trainData79$LastJob83 <- colnames(stopweeksTrain83)[apply(stopweeksTrain83, 1, which.max)]

#1984
stopweeksTrain84 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1984'))

stopweeksTrain84[is.na(stopweeksTrain84)] <- 0

trainData79$LastJob84 <- colnames(stopweeksTrain84)[apply(stopweeksTrain84, 1, which.max)]

#1985
stopweeksTrain85 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1985'))

stopweeksTrain85[is.na(stopweeksTrain85)] <- 0

trainData79$LastJob85 <- colnames(stopweeksTrain85)[apply(stopweeksTrain85, 1, which.max)]

#1986
stopweeksTrain86 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1986'))

stopweeksTrain86[is.na(stopweeksTrain86)] <- 0

trainData79$LastJob86 <- colnames(stopweeksTrain86)[apply(stopweeksTrain86, 1, which.max)]

#1987
stopweeksTrain87 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1987'))

stopweeksTrain87[is.na(stopweeksTrain87)] <- 0

trainData79$LastJob87 <- colnames(stopweeksTrain87)[apply(stopweeksTrain87, 1, which.max)]

#1988
stopweeksTrain88 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1988'))

stopweeksTrain88[is.na(stopweeksTrain88)] <- 0

trainData79$LastJob88 <- colnames(stopweeksTrain88)[apply(stopweeksTrain88, 1, which.max)]

#1989
stopweeksTrain89 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1989'))

stopweeksTrain89[is.na(stopweeksTrain89)] <- 0

trainData79$LastJob89 <- colnames(stopweeksTrain89)[apply(stopweeksTrain89, 1, which.max)]

#1990
stopweeksTrain90 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1990'))

stopweeksTrain90[is.na(stopweeksTrain90)] <- 0

trainData79$LastJob90 <- colnames(stopweeksTrain90)[apply(stopweeksTrain90, 1, which.max)]

#1991
stopweeksTrain91 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1991'))

stopweeksTrain91[is.na(stopweeksTrain91)] <- 0

trainData79$LastJob91 <- colnames(stopweeksTrain91)[apply(stopweeksTrain91, 1, which.max)]

#1992
stopweeksTrain92 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1992'))

stopweeksTrain92[is.na(stopweeksTrain92)] <- 0

trainData79$LastJob92 <- colnames(stopweeksTrain92)[apply(stopweeksTrain92, 1, which.max)]

#1993
stopweeksTrain93 <- trainData79 %>% 
  select(starts_with('EMPLOYERS_ALL_STOPWEEK_1993'))

stopweeksTrain93[is.na(stopweeksTrain93)] <- 0

trainData79$LastJob93 <- colnames(stopweeksTrain93)[apply(stopweeksTrain93, 1, which.max)]
