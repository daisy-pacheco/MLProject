library(dplyr)
library(tidyverse)

# switching variable names to words vs codes
varNames <- read.csv("varNames79.csv", header = FALSE)

names(trainData79) <- varNames[match(names(trainData79), varNames[ ,'V1']),'V3']


# making a tiny sample for testing feature engineering 
set.seed(456) 

tinySample79 <- sample_n(trainData79, 20)


##### FEATURE ENGINEERING #####

### job satisfaction ###

# replacing all NAs with 0 in the "currently working" columns
trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("CURRENTLY-WORKING")), funs(replace_na(., 0)))

#### EXAMPLE LOOP PSEUDOCODE ####

# 1979
trainData79$JOB_SATISFACTION.01_1979 <- NA
trainData79$JOB_SATISFACTION.02_1979 <- NA
trainData79$JOB_SATISFACTION.03_1979 <- NA
trainData79$JOB_SATISFACTION.04_1979 <- NA
trainData79$JOB_SATISFACTION.05_1979 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY-WORKING-01_1979_R0068000"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1979"] <- trainData79[i,"JOB_SATISFACTION_R0050800"]
  }else if (trainData79[i,"CURRENTLY-WORKING-02_1979_R0068100"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1979"] <- trainData79[i,"JOB_SATISFACTION_R0050800"]
  }else if (trainData79[i,"CURRENTLY-WORKING-03_1979_R0068200"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1979"] <- trainData79[i,"JOB_SATISFACTION_R0050800"]
  }else if (trainData79[i,"CURRENTLY-WORKING-04_1979_R0068300"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1979"] <- trainData79[i,"JOB_SATISFACTION_R0050800"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1979"] <- trainData79[i,"JOB_SATISFACTION_R0050800"]
  }
}

# 1980
trainData79$JOB_SATISFACTION.01_1980 <- NA
trainData79$JOB_SATISFACTION.02_1980 <- NA
trainData79$JOB_SATISFACTION.03_1980 <- NA
trainData79$JOB_SATISFACTION.04_1980 <- NA
trainData79$JOB_SATISFACTION.05_1980 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY-WORKING-01_1980_R0333100"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1980"] <- trainData79[i,"JOB_SATISFACTION_R0267800"]
  }else if (trainData79[i,"CURRENTLY-WORKING-02_1980_R0344600"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1980"] <- trainData79[i,"JOB_SATISFACTION_R0267800"]
  }else if (trainData79[i,"CURRENTLY-WORKING-03_1980_R0356100"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1980"] <- trainData79[i,"JOB_SATISFACTION_R0267800"]
  }else if (trainData79[i,"CURRENTLY-WORKING-04_1980_R0367600"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1980"] <- trainData79[i,"JOB_SATISFACTION_R0267800"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1980"] <- trainData79[i,"JOB_SATISFACTION_R0267800"]
  }
}

# 1981
trainData79$JOB_SATISFACTION.01_1981 <- NA
trainData79$JOB_SATISFACTION.02_1981 <- NA
trainData79$JOB_SATISFACTION.03_1981 <- NA
trainData79$JOB_SATISFACTION.04_1981 <- NA
trainData79$JOB_SATISFACTION.05_1981 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY-WORKING-01_1981_R0539100"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1981"] <- trainData79[i,"JOB_SATISFACTION_R0449200"]
  }else if (trainData79[i,"CURRENTLY-WORKING-02_1981_R0552200"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1981"] <- trainData79[i,"JOB_SATISFACTION_R0449200"]
  }else if (trainData79[i,"CURRENTLY-WORKING-03_1981_R0565300"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1981"] <- trainData79[i,"JOB_SATISFACTION_R0449200"]
  }else if (trainData79[i,"CURRENTLY-WORKING-04_1981_R0578400"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1981"] <- trainData79[i,"JOB_SATISFACTION_R0449200"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1981"] <- trainData79[i,"JOB_SATISFACTION_R0449200"]
  }
}

# 1982
trainData79$JOB_SATISFACTION.01_1982 <- NA
trainData79$JOB_SATISFACTION.02_1982 <- NA
trainData79$JOB_SATISFACTION.03_1982 <- NA
trainData79$JOB_SATISFACTION.04_1982 <- NA
trainData79$JOB_SATISFACTION.05_1982 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY-WORKING-01_1982_R0833500"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1982"] <- trainData79[i,"JOB_SATISFACTION_R0706500"]
  }else if (trainData79[i,"CURRENTLY-WORKING-02_1982_R0846600"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1982"] <- trainData79[i,"JOB_SATISFACTION_R0706500"]
  }else if (trainData79[i,"CURRENTLY-WORKING-03_1982_R0859700"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1982"] <- trainData79[i,"JOB_SATISFACTION_R0706500"]
  }else if (trainData79[i,"CURRENTLY-WORKING-04_1982_R0872800"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1982"] <- trainData79[i,"JOB_SATISFACTION_R0706500"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1982"] <- trainData79[i,"JOB_SATISFACTION_R0706500"]
  }
}

# 1983
trainData79$JOB_SATISFACTION.01_1983 <- NA
trainData79$JOB_SATISFACTION.02_1983 <- NA
trainData79$JOB_SATISFACTION.03_1983 <- NA
trainData79$JOB_SATISFACTION.04_1983 <- NA
trainData79$JOB_SATISFACTION.05_1983 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY-WORKING-01_1983_R1080700"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1983"] <- trainData79[i,"JOB_SATISFACTION_R0946500"]
  }else if (trainData79[i,"CURRENTLY-WORKING-02_1983_R1093900"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1983"] <- trainData79[i,"JOB_SATISFACTION_R0946500"]
  }else if (trainData79[i,"CURRENTLY-WORKING-03_1983_R1107100"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1983"] <- trainData79[i,"JOB_SATISFACTION_R0946500"]
  }else if (trainData79[i,"CURRENTLY-WORKING-04_1983_R1120300"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1983"] <- trainData79[i,"JOB_SATISFACTION_R0946500"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1983"] <- trainData79[i,"JOB_SATISFACTION_R0946500"]
  }
}

# 1984
trainData79$JOB_SATISFACTION.01_1984 <- NA
trainData79$JOB_SATISFACTION.02_1984 <- NA
trainData79$JOB_SATISFACTION.03_1984 <- NA
trainData79$JOB_SATISFACTION.04_1984 <- NA
trainData79$JOB_SATISFACTION.05_1984 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY-WORKING-01_1984_R1456400"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1984"] <- trainData79[i,"JOB_SATISFACTION_R1256900"]
  }else if (trainData79[i,"CURRENTLY-WORKING-02_1984_R1469500"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1984"] <- trainData79[i,"JOB_SATISFACTION_R1256900"]
  }else if (trainData79[i,"CURRENTLY-WORKING-03_1984_R1482600"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1984"] <- trainData79[i,"JOB_SATISFACTION_R1256900"]
  }else if (trainData79[i,"CURRENTLY-WORKING-04_1984_R1495700"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1984"] <- trainData79[i,"JOB_SATISFACTION_R1256900"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1984"] <- trainData79[i,"JOB_SATISFACTION_R1256900"]
  }
}

# 1985
trainData79$JOB_SATISFACTION.01_1985 <- NA
trainData79$JOB_SATISFACTION.02_1985 <- NA
trainData79$JOB_SATISFACTION.03_1985 <- NA
trainData79$JOB_SATISFACTION.04_1985 <- NA
trainData79$JOB_SATISFACTION.05_1985 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY-WORKING-01_1985_R1803200"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1985"] <- trainData79[i,"JOB_SATISFACTION_R1652000"]
  }else if (trainData79[i,"CURRENTLY-WORKING-02_1985_R1815900"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1985"] <- trainData79[i,"JOB_SATISFACTION_R1652000"]
  }else if (trainData79[i,"CURRENTLY-WORKING-03_1985_R1828600"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1985"] <- trainData79[i,"JOB_SATISFACTION_R1652000"]
  }else if (trainData79[i,"CURRENTLY-WORKING-04_1985_R1841300"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1985"] <- trainData79[i,"JOB_SATISFACTION_R1652000"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1985"] <- trainData79[i,"JOB_SATISFACTION_R1652000"]
  }
}

# 1986
trainData79$JOB_SATISFACTION.01_1986 <- NA
trainData79$JOB_SATISFACTION.02_1986 <- NA
trainData79$JOB_SATISFACTION.03_1986 <- NA
trainData79$JOB_SATISFACTION.04_1986 <- NA
trainData79$JOB_SATISFACTION.05_1986 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY-WORKING-01_1986_R2164800"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1986"] <- trainData79[i,"JOB_SATISFACTION_R1925100"]
  }else if (trainData79[i,"CURRENTLY-WORKING-02_1986_R2178400"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1986"] <- trainData79[i,"JOB_SATISFACTION_R1925100"]
  }else if (trainData79[i,"CURRENTLY-WORKING-03_1986_R2192000"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1986"] <- trainData79[i,"JOB_SATISFACTION_R1925100"]
  }else if (trainData79[i,"CURRENTLY-WORKING-04_1986_R2205600"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1986"] <- trainData79[i,"JOB_SATISFACTION_R1925100"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1986"] <- trainData79[i,"JOB_SATISFACTION_R1925100"]
  }
}

# 1987
trainData79$JOB_SATISFACTION.01_1987 <- NA
trainData79$JOB_SATISFACTION.02_1987 <- NA
trainData79$JOB_SATISFACTION.03_1987 <- NA
trainData79$JOB_SATISFACTION.04_1987 <- NA
trainData79$JOB_SATISFACTION.05_1987 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY-WORKING-01_1987_R2372200"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1987"] <- trainData79[i,"JOB_SATISFACTION_R2319900"]
  }else if (trainData79[i,"CURRENTLY-WORKING-02_1987_R2383500"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1987"] <- trainData79[i,"JOB_SATISFACTION_R2319900"]
  }else if (trainData79[i,"CURRENTLY-WORKING-03_1987_R2394800"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1987"] <- trainData79[i,"JOB_SATISFACTION_R2319900"]
  }else if (trainData79[i,"CURRENTLY-WORKING-04_1987_R2406100"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1987"] <- trainData79[i,"JOB_SATISFACTION_R2319900"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1987"] <- trainData79[i,"JOB_SATISFACTION_R2319900"]
  }
}

# 1988
trainData79$JOB_SATISFACTION.01_1988 <- NA
trainData79$JOB_SATISFACTION.02_1988 <- NA
trainData79$JOB_SATISFACTION.03_1988 <- NA
trainData79$JOB_SATISFACTION.04_1988 <- NA
trainData79$JOB_SATISFACTION.05_1988 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY-WORKING-01_1988_R2763100"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1988"] <- trainData79[i,"JOB_SATISFACTION_R2532900"]
  }else if (trainData79[i,"CURRENTLY-WORKING-02_1988_R2776000"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1988"] <- trainData79[i,"JOB_SATISFACTION_R2532900"]
  }else if (trainData79[i,"CURRENTLY-WORKING-03_1988_R2788900"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1988"] <- trainData79[i,"JOB_SATISFACTION_R2532900"]
  }else if (trainData79[i,"CURRENTLY-WORKING-04_1988_R2801800"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1988"] <- trainData79[i,"JOB_SATISFACTION_R2532900"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1988"] <- trainData79[i,"JOB_SATISFACTION_R2532900"]
  }
}

# 1989
trainData79$JOB_SATISFACTION.01_1989 <- NA
trainData79$JOB_SATISFACTION.02_1989 <- NA
trainData79$JOB_SATISFACTION.03_1989 <- NA
trainData79$JOB_SATISFACTION.04_1989 <- NA
trainData79$JOB_SATISFACTION.05_1989 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY-WORKING-01_1989_R3004900"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1989"] <- trainData79[i,"JOB_SATISFACTION_R2930500"]
  }else if (trainData79[i,"CURRENTLY-WORKING-02_1989_R3018000"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1989"] <- trainData79[i,"JOB_SATISFACTION_R2930500"]
  }else if (trainData79[i,"CURRENTLY-WORKING-03_1989_R3031100"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1989"] <- trainData79[i,"JOB_SATISFACTION_R2930500"]
  }else if (trainData79[i,"CURRENTLY-WORKING-04_1989_R3044200"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1989"] <- trainData79[i,"JOB_SATISFACTION_R2930500"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1989"] <- trainData79[i,"JOB_SATISFACTION_R2930500"]
  }
}

# 1990
trainData79$JOB_SATISFACTION.01_1990 <- NA
trainData79$JOB_SATISFACTION.02_1990 <- NA
trainData79$JOB_SATISFACTION.03_1990 <- NA
trainData79$JOB_SATISFACTION.04_1990 <- NA
trainData79$JOB_SATISFACTION.05_1990 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY-WORKING-01_1990_R3332300"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1990"] <- trainData79[i,"JOB_SATISFACTION_R3136600"]
  }else if (trainData79[i,"CURRENTLY-WORKING-02_1990_R3346300"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1990"] <- trainData79[i,"JOB_SATISFACTION_R3136600"]
  }else if (trainData79[i,"CURRENTLY-WORKING-03_1990_R3360300"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1990"] <- trainData79[i,"JOB_SATISFACTION_R3136600"]
  }else if (trainData79[i,"CURRENTLY-WORKING-04_1990_R3374300"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1990"] <- trainData79[i,"JOB_SATISFACTION_R3136600"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1990"] <- trainData79[i,"JOB_SATISFACTION_R3136600"]
  }
}

# 1991
trainData79$JOB_SATISFACTION.01_1991 <- NA
trainData79$JOB_SATISFACTION.02_1991 <- NA
trainData79$JOB_SATISFACTION.03_1991 <- NA
trainData79$JOB_SATISFACTION.04_1991 <- NA
trainData79$JOB_SATISFACTION.05_1991 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY-WORKING-01_1991_R3597300"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1991"] <- trainData79[i,"JOB_SATISFACTION_R3527000"]
  }else if (trainData79[i,"CURRENTLY-WORKING-02_1991_R3609400"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1991"] <- trainData79[i,"JOB_SATISFACTION_R3527000"]
  }else if (trainData79[i,"CURRENTLY-WORKING-03_1991_R3621500"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1991"] <- trainData79[i,"JOB_SATISFACTION_R3527000"]
  }else if (trainData79[i,"CURRENTLY-WORKING-04_1991_R3633600"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1991"] <- trainData79[i,"JOB_SATISFACTION_R3527000"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1991"] <- trainData79[i,"JOB_SATISFACTION_R3527000"]
  }
}

# 1992
trainData79$JOB_SATISFACTION.01_1992 <- NA
trainData79$JOB_SATISFACTION.02_1992 <- NA
trainData79$JOB_SATISFACTION.03_1992 <- NA
trainData79$JOB_SATISFACTION.04_1992 <- NA
trainData79$JOB_SATISFACTION.05_1992 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY-WORKING-01_1992_R3947400"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1992"] <- trainData79[i,"JOB_SATISFACTION_R3732100"]
  }else if (trainData79[i,"CURRENTLY-WORKING-02_1992_R3959600"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1992"] <- trainData79[i,"JOB_SATISFACTION_R3732100"]
  }else if (trainData79[i,"CURRENTLY-WORKING-03_1992_R3971800"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1992"] <- trainData79[i,"JOB_SATISFACTION_R3732100"]
  }else if (trainData79[i,"CURRENTLY-WORKING-04_1992_R3984000"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1992"] <- trainData79[i,"JOB_SATISFACTION_R3732100"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1992"] <- trainData79[i,"JOB_SATISFACTION_R3732100"]
  }
}

# 1993
trainData79$JOB_SATISFACTION.01_1993 <- NA
trainData79$JOB_SATISFACTION.02_1993 <- NA
trainData79$JOB_SATISFACTION.03_1993 <- NA
trainData79$JOB_SATISFACTION.04_1993 <- NA
trainData79$JOB_SATISFACTION.05_1993 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY-WORKING-01_1993_R4187900"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1993"] <- trainData79[i,"JOB_SATISFACTION_R4187200"]
  }else if (trainData79[i,"CURRENTLY-WORKING-02_1993_R4200800"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1993"] <- trainData79[i,"JOB_SATISFACTION_R4187200"]
  }else if (trainData79[i,"CURRENTLY-WORKING-03_1993_R4208400"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1993"] <- trainData79[i,"JOB_SATISFACTION_R4187200"]
  }else if (trainData79[i,"CURRENTLY-WORKING-04_1993_R4215400"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1993"] <- trainData79[i,"JOB_SATISFACTION_R4187200"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1993"] <- trainData79[i,"JOB_SATISFACTION_R4187200"]
  }
}

### sector ###
# 1979
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1979 = case_when(EMPLOYERS_ALL_COW_1979.01_E7820100 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1979.01_E7820100 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1979 = case_when(EMPLOYERS_ALL_COW_1979.02_E7820200 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1979.02_E7820200 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1979 = case_when(EMPLOYERS_ALL_COW_1979.03_E7820300 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1979.03_E7820300 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1979 = case_when(EMPLOYERS_ALL_COW_1979.04_E7820400 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1979.04_E7820400 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1979 = case_when(EMPLOYERS_ALL_COW_1979.05_E7820500 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1979.05_E7820500 == 2 ~ 1))

# 1980
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1980 = case_when(EMPLOYERS_ALL_COW_1980.01_E7830100 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1980.01_E7830100 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1980 = case_when(EMPLOYERS_ALL_COW_1980.02_E7830200 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1980.02_E7830200 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1980 = case_when(EMPLOYERS_ALL_COW_1980.03_E7830300 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1980.03_E7830300 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1980 = case_when(EMPLOYERS_ALL_COW_1980.04_E7830400 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1980.04_E7830400 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1980 = case_when(EMPLOYERS_ALL_COW_1980.05_E7830500 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1980.05_E7830500 == 2 ~ 1))

# 1981
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1981 = case_when(EMPLOYERS_ALL_COW_1981.01_E7840100 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1981.01_E7840100 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1981 = case_when(EMPLOYERS_ALL_COW_1981.02_E7840200 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1981.02_E7840200 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1981 = case_when(EMPLOYERS_ALL_COW_1981.03_E7840300 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1981.03_E7840300 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1981 = case_when(EMPLOYERS_ALL_COW_1981.04_E7840400 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1981.04_E7840400 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1981 = case_when(EMPLOYERS_ALL_COW_1981.05_E7840500 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1981.05_E7840500 == 2 ~ 1))

# 1982
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1982 = case_when(EMPLOYERS_ALL_COW_1982.01_E7850100 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1982.01_E7850100 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1982 = case_when(EMPLOYERS_ALL_COW_1982.02_E7850200 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1982.02_E7850200 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1982 = case_when(EMPLOYERS_ALL_COW_1982.03_E7850300 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1982.03_E7850300 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1982 = case_when(EMPLOYERS_ALL_COW_1982.04_E7850400 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1982.04_E7850400 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1982 = case_when(EMPLOYERS_ALL_COW_1982.05_E7850500 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1982.05_E7850500 == 2 ~ 1))

# 1983
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1983 = case_when(EMPLOYERS_ALL_COW_1983.01_E7860100 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1983.01_E7860100 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1983 = case_when(EMPLOYERS_ALL_COW_1983.02_E7860200 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1983.02_E7860200 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1983 = case_when(EMPLOYERS_ALL_COW_1983.03_E7860300  %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1983.03_E7860300  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1983 = case_when(EMPLOYERS_ALL_COW_1983.04_E7860400 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1983.04_E7860400 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1983 = case_when(EMPLOYERS_ALL_COW_1983.05_E7860500 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1983.05_E7860500 == 2 ~ 1))


# 1984
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1984 = case_when(EMPLOYERS_ALL_COW_1984.01_E7870100 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1984.01_E7870100 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1984 = case_when(EMPLOYERS_ALL_COW_1984.02_E7870200 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1984.02_E7870200 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1984 = case_when(EMPLOYERS_ALL_COW_1984.03_E7870300  %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1984.03_E7870300  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1984 = case_when(EMPLOYERS_ALL_COW_1984.04_E7870400 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1984.04_E7870400 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1984 = case_when(EMPLOYERS_ALL_COW_1984.05_E7870500 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1984.05_E7870500 == 2 ~ 1))

# 1985
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1985 = case_when(EMPLOYERS_ALL_COW_1985.01_E7880100 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1985.01_E7880100 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1985 = case_when(EMPLOYERS_ALL_COW_1985.02_E7880200 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1985.02_E7880200 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1985 = case_when(EMPLOYERS_ALL_COW_1985.03_E7880300  %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1985.03_E7880300  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1985 = case_when(EMPLOYERS_ALL_COW_1985.04_E7880400 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1985.04_E7880400 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1985 = case_when(EMPLOYERS_ALL_COW_1985.05_E7880500 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1985.05_E7880500 == 2 ~ 1))

# 1986
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1986 = case_when(EMPLOYERS_ALL_COW_1986.01_E7890100 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1986.01_E7890100 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1986 = case_when(EMPLOYERS_ALL_COW_1986.02_E7890200 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1986.02_E7890200 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1986 = case_when(EMPLOYERS_ALL_COW_1986.03_E7890300   %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1986.03_E7890300   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1986 = case_when(EMPLOYERS_ALL_COW_1986.04_E7890400  %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1986.04_E7890400  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1986 = case_when(EMPLOYERS_ALL_COW_1986.05_E7890500  %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1986.05_E7890500  == 2 ~ 1))

# 1987
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1987 = case_when(EMPLOYERS_ALL_COW_1987.01_E7900100 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1987.01_E7900100 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1987 = case_when(EMPLOYERS_ALL_COW_1987.02_E7900200 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1987.02_E7900200 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1987 = case_when(EMPLOYERS_ALL_COW_1987.03_E7900300   %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1987.03_E7900300   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1987 = case_when(EMPLOYERS_ALL_COW_1987.04_E7900400  %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1987.04_E7900400  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1987 = case_when(EMPLOYERS_ALL_COW_1987.05_E7900500  %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1987.05_E7900500  == 2 ~ 1))

# 1988
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1988 = case_when(EMPLOYERS_ALL_COW_1988.01_E7910100 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1988.01_E7910100 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1988 = case_when(EMPLOYERS_ALL_COW_1988.02_E7910200 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1988.02_E7910200 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1988 = case_when(EMPLOYERS_ALL_COW_1988.03_E7910300   %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1988.03_E7910300   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1988 = case_when(EMPLOYERS_ALL_COW_1988.04_E7910400  %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1988.04_E7910400  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1988 = case_when(EMPLOYERS_ALL_COW_1988.05_E7910500  %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1988.05_E7910500  == 2 ~ 1))

# 1989
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1989 = case_when(EMPLOYERS_ALL_COW_1989.01_E7920100 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1989.01_E7920100 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1989 = case_when(EMPLOYERS_ALL_COW_1989.02_E7920200 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1989.02_E7920200 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1989 = case_when(EMPLOYERS_ALL_COW_1989.03_E7920300   %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1989.03_E7920300   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1989 = case_when(EMPLOYERS_ALL_COW_1989.04_E7920400  %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1989.04_E7920400  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1989 = case_when(EMPLOYERS_ALL_COW_1989.05_E7920500  %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1989.05_E7920500  == 2 ~ 1))

# 1990
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1990 = case_when(EMPLOYERS_ALL_COW_1990.01_E7930100 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1990.01_E7930100 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1990 = case_when(EMPLOYERS_ALL_COW_1990.02_E7930200 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1990.02_E7930200 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1990 = case_when(EMPLOYERS_ALL_COW_1990.03_E7930300   %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1990.03_E7930300   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1990 = case_when(EMPLOYERS_ALL_COW_1990.04_E7930400  %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1990.04_E7930400  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1990 = case_when(EMPLOYERS_ALL_COW_1990.05_E7930500  %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1990.05_E7930500  == 2 ~ 1))

# 1991
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1991 = case_when(EMPLOYERS_ALL_COW_1991.01_E7940100 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1991.01_E7940100 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1991 = case_when(EMPLOYERS_ALL_COW_1991.02_E7940200 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1991.02_E7940200 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1991 = case_when(EMPLOYERS_ALL_COW_1991.03_E7940300   %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1991.03_E7940300   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1991 = case_when(EMPLOYERS_ALL_COW_1991.04_E7940400  %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1991.04_E7940400  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1991 = case_when(EMPLOYERS_ALL_COW_1991.05_E7940500  %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1991.05_E7940500  == 2 ~ 1))

# 1992
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1992 = case_when(EMPLOYERS_ALL_COW_1992.01_E7950100 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1992.01_E7950100 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1992 = case_when(EMPLOYERS_ALL_COW_1992.02_E7950200 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1992.02_E7950200 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1992 = case_when(EMPLOYERS_ALL_COW_1992.03_E7950300   %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1992.03_E7950300   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1992 = case_when(EMPLOYERS_ALL_COW_1992.04_E7950400  %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1992.04_E7950400  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1992 = case_when(EMPLOYERS_ALL_COW_1992.05_E7950500  %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1992.05_E7950500  == 2 ~ 1))     
       
# 1993
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1993 = case_when(EMPLOYERS_ALL_COW_1993.01_E7960100 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1993.01_E7960100 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1993 = case_when(EMPLOYERS_ALL_COW_1993.02_E7960200 %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1993.02_E7960200 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1993 = case_when(EMPLOYERS_ALL_COW_1993.03_E7960300   %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1993.03_E7960300   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1993 = case_when(EMPLOYERS_ALL_COW_1993.04_E7960400  %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1993.04_E7960400  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1993 = case_when(EMPLOYERS_ALL_COW_1993.05_E7960500  %in% c(1,3,4) ~ 0, 
                                           EMPLOYERS_ALL_COW_1993.05_E7960500  == 2 ~ 1))       
# 1994
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1994 = case_when(EMPLOYERS_ALL_COW_1994.01_E7970100 %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_1994.01_E7970100 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1994 = case_when(EMPLOYERS_ALL_COW_1994.02_E7970200 %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_1994.02_E7970200 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1994 = case_when(EMPLOYERS_ALL_COW_1994.03_E7970300   %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_1994.03_E7970300   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1994 = case_when(EMPLOYERS_ALL_COW_1994.04_E7970400  %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_1994.04_E7970400  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1994 = case_when(EMPLOYERS_ALL_COW_1994.05_E7970500  %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_1994.05_E7970500  == 2 ~ 1))
# 1996
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1996 = case_when(EMPLOYERS_ALL_COW_1996.01_E7980100 %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_1996.01_E7980100 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1996 = case_when(EMPLOYERS_ALL_COW_1996.02_E7980200 %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_1996.02_E7980200 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1996 = case_when(EMPLOYERS_ALL_COW_1996.03_E7980300   %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_1996.03_E7980300   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1996 = case_when(EMPLOYERS_ALL_COW_1996.04_E7980400  %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_1996.04_E7980400  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1996 = case_when(EMPLOYERS_ALL_COW_1996.05_E7980500  %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_1996.05_E7980500  == 2 ~ 1))

# 1998
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1998 = case_when(EMPLOYERS_ALL_COW_1998.01_E7990100 %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_1998.01_E7990100 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1998 = case_when(EMPLOYERS_ALL_COW_1998.02_E7990200 %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_1998.02_E7990200 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1998 = case_when(EMPLOYERS_ALL_COW_1998.03_E7990300   %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_1998.03_E7990300   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1998 = case_when(EMPLOYERS_ALL_COW_1998.04_E7990400  %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_1998.04_E7990400  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1998 = case_when(EMPLOYERS_ALL_COW_1998.05_E7990500  %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_1998.05_E7990500  == 2 ~ 1))
 
# 2000
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_2000 = case_when(EMPLOYERS_ALL_COW_2000.01_E8000100 %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2000.01_E8000100 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_2000 = case_when(EMPLOYERS_ALL_COW_2000.02_E8000200 %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2000.02_E8000200 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_2000 = case_when(EMPLOYERS_ALL_COW_2000.03_E8000300   %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2000.03_E8000300   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_2000 = case_when(EMPLOYERS_ALL_COW_2000.04_E8000400  %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2000.04_E8000400  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_2000 = case_when(EMPLOYERS_ALL_COW_2000.05_E8000500  %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2000.05_E8000500  == 2 ~ 1))

# 2002
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_2002 = case_when(EMPLOYERS_ALL_COW_2002.01_E8010100 %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2002.01_E8010100 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_2002 = case_when(EMPLOYERS_ALL_COW_2002.02_E8010200 %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2002.02_E8010200 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_2002 = case_when(EMPLOYERS_ALL_COW_2002.03_E8010300   %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2002.03_E8010300   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_2002 = case_when(EMPLOYERS_ALL_COW_2002.04_E8010400  %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2002.04_E8010400  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_2002 = case_when(EMPLOYERS_ALL_COW_2002.05_E8010500  %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2002.05_E8010500  == 2 ~ 1))

# 2004
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_2004 = case_when(EMPLOYERS_ALL_COW_2004.01_E8020100 %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2004.01_E8020100 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_2004 = case_when(EMPLOYERS_ALL_COW_2004.02_E8020200 %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2004.02_E8020200 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_2004 = case_when(EMPLOYERS_ALL_COW_2004.03_E8020300   %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2004.03_E8020300   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_2004 = case_when(EMPLOYERS_ALL_COW_2004.04_E8020400  %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2004.04_E8020400  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_2004 = case_when(EMPLOYERS_ALL_COW_2004.05_E8020500  %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2004.05_E8020500  == 2 ~ 1))

# 2006
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_2006 = case_when(EMPLOYERS_ALL_COW_2006.01_E8030100 %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2006.01_E8030100 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_2006 = case_when(EMPLOYERS_ALL_COW_2006.02_E8030200 %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2006.02_E8030200 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_2006 = case_when(EMPLOYERS_ALL_COW_2006.03_E8030300   %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2006.03_E8030300   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_2006 = case_when(EMPLOYERS_ALL_COW_2006.04_E8030400  %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2006.04_E8030400  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_2006 = case_when(EMPLOYERS_ALL_COW_2006.05_E8030500  %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2006.05_E8030500  == 2 ~ 1))

# 2008
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_2008 = case_when(EMPLOYERS_ALL_COW_2008.01_E8040100 %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2008.01_E8040100 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_2008 = case_when(EMPLOYERS_ALL_COW_2008.02_E8040200 %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2008.02_E8040200 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_2008 = case_when(EMPLOYERS_ALL_COW_2008.03_E8040300   %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2008.03_E8040300   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_2008 = case_when(EMPLOYERS_ALL_COW_2008.04_E8040400  %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2008.04_E8040400  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_2008 = case_when(EMPLOYERS_ALL_COW_2008.05_E8040500  %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2008.05_E8040500  == 2 ~ 1))

# 2010
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_2010 = case_when(EMPLOYERS_ALL_COW_2010.01_E8050100 %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2010.01_E8050100 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_2010 = case_when(EMPLOYERS_ALL_COW_2010.02_E8050200 %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2010.02_E8050200 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_2010 = case_when(EMPLOYERS_ALL_COW_2010.03_E8050300   %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2010.03_E8050300   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_2010 = case_when(EMPLOYERS_ALL_COW_2010.04_E8050400  %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2010.04_E8050400  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_2010 = case_when(EMPLOYERS_ALL_COW_2010.05_E8050500  %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2010.05_E8050500  == 2 ~ 1))

# 2012
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_2012 = case_when(EMPLOYERS_ALL_COW_2012.01_E8670100 %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2012.01_E8670100 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_2012 = case_when(EMPLOYERS_ALL_COW_2012.02_E8670200 %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2012.02_E8670200 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_2012 = case_when(EMPLOYERS_ALL_COW_2012.03_E8670300   %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2012.03_E8670300   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_2012 = case_when(EMPLOYERS_ALL_COW_2012.04_E8670400  %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2012.04_E8670400  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_2012 = case_when(EMPLOYERS_ALL_COW_2012.05_E8670500  %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2012.05_E8670500  == 2 ~ 1))

# 2014
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_2014 = case_when(EMPLOYERS_ALL_COW_2014.01_X0220200 %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2014.01_X0220200 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_2014 = case_when(EMPLOYERS_ALL_COW_2014.02_X0220300 %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2014.02_X0220300 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_2014 = case_when(EMPLOYERS_ALL_COW_2014.03_X0220400   %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2014.03_X0220400   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_2014 = case_when(EMPLOYERS_ALL_COW_2014.04_X0220500  %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2014.04_X0220500  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_2014 = case_when(EMPLOYERS_ALL_COW_2014.05_X0220600  %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2014.05_X0220600  == 2 ~ 1))

# 2016
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_2016 = case_when(EMPLOYERS_ALL_COW_2016.01_E8696500 %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2016.01_E8696500 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_2016 = case_when(EMPLOYERS_ALL_COW_2016.02_E8696600 %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2016.02_E8696600 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_2016 = case_when(EMPLOYERS_ALL_COW_2016.03_E8696700   %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2016.03_E8696700   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_2016 = case_when(EMPLOYERS_ALL_COW_2016.04_E8696800  %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2016.04_E8696800  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_2016 = case_when(EMPLOYERS_ALL_COW_2016.05_E8696900  %in% c(1,3,4,5) ~ 0, 
                                           EMPLOYERS_ALL_COW_2016.05_E8696900  == 2 ~ 1))

### wages ###

# 1979
logCPI1979 <- function(wage) {
  wageLogCPI <- log(wage * 3.47) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("EMPLOYERS_ALL_HRLY_WAGE_1979"),"NET_FAMILY_INCOME_R0217900"), 
            funs(logCPI1979))

# 1980
logCPI1980 <- function(wage) {
  wageLogCPI <- log(wage * 3.05) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("EMPLOYERS_ALL_HRLY_WAGE_1980"),"NET_FAMILY_INCOME_R0406010"), 
            funs(logCPI1980))


# 1981
logCPI1981 <- function(wage) {
  wageLogCPI <- log(wage * 2.72) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("EMPLOYERS_ALL_HRLY_WAGE_1981"),"NET_FAMILY_INCOME_R0618410"), 
            funs(logCPI1981))

# 1982
logCPI1982 <- function(wage) {
  wageLogCPI <- log(wage * 2.51) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("EMPLOYERS_ALL_HRLY_WAGE_1982"),"NET_FAMILY_INCOME_R0898600"), 
            funs(logCPI1982))

# 1983
logCPI1983 <- function(wage) {
  wageLogCPI <- log(wage * 2.42) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("EMPLOYERS_ALL_HRLY_WAGE_1983"),"NET_FAMILY_INCOME_R1144500"), 
            funs(logCPI1983))

# 1984
logCPI1984 <- function(wage) {
  wageLogCPI <- log(wage * 2.32) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("EMPLOYERS_ALL_HRLY_WAGE_1984"),"NET_FAMILY_INCOME_R1519700"), 
            funs(logCPI1984))

# 1985
logCPI1985 <- function(wage) {
  wageLogCPI <- log(wage * 2.25) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("EMPLOYERS_ALL_HRLY_WAGE_1985"),"NET_FAMILY_INCOME_R1890400"), 
            funs(logCPI1985))

# 1986
logCPI1986 <- function(wage) {
  wageLogCPI <- log(wage * 2.16) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("EMPLOYERS_ALL_HRLY_WAGE_1986"),"NET_FAMILY_INCOME_R2257500"), 
            funs(logCPI1986))

# 1987
logCPI1987 <- function(wage) {
  wageLogCPI <- log(wage * 2.13) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("EMPLOYERS_ALL_HRLY_WAGE_1987"),"NET_FAMILY_INCOME_R2444700"), 
            funs(logCPI1987))

# 1988
logCPI1988 <- function(wage) {
  wageLogCPI <- log(wage * 2.05) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("EMPLOYERS_ALL_HRLY_WAGE_1988"),"NET_FAMILY_INCOME_R2870200"), 
            funs(logCPI1988))

# 1989
logCPI1989 <- function(wage) {
  wageLogCPI <- log(wage * 1.96) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("EMPLOYERS_ALL_HRLY_WAGE_1989"),"NET_FAMILY_INCOME_R3074000"), 
            funs(logCPI1989))

# 1990
logCPI1990 <- function(wage) {
  wageLogCPI <- log(wage * 1.86) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("EMPLOYERS_ALL_HRLY_WAGE_1990"),"NET_FAMILY_INCOME_R3400700"), 
            funs(logCPI1990))

# 1991
logCPI1991 <- function(wage) {
  wageLogCPI <- log(wage * 1.76) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("EMPLOYERS_ALL_HRLY_WAGE_1991"),"NET_FAMILY_INCOME_R3656100"), 
            funs(logCPI1991))

# 1992
logCPI1992 <- function(wage) {
  wageLogCPI <- log(wage * 1.72) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("EMPLOYERS_ALL_HRLY_WAGE_1992"),"NET_FAMILY_INCOME_R4006600"), 
            funs(logCPI1992))

# 1993
logCPI1993 <- function(wage) {
  wageLogCPI <- log(wage * 1.66) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("EMPLOYERS_ALL_HRLY_WAGE_1993"),"NET_FAMILY_INCOME_R4417700"), 
            funs(logCPI1993))

# 1994
logCPI1994 <- function(wage) {
  wageLogCPI <- log(wage * 1.62) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("EMPLOYERS_ALL_HRLY_WAGE_1994"),"NET_FAMILY_INCOME_R5080700"), 
            funs(logCPI1994))

# 1996
logCPI1996 <- function(wage) {
  wageLogCPI <- log(wage * 1.53) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("EMPLOYERS_ALL_HRLY_WAGE_1996"),"NET_FAMILY_INCOME_R5166000"), 
            funs(logCPI1996))

# 1998
logCPI1998 <- function(wage) {
  wageLogCPI <- log(wage * 1.47) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("EMPLOYERS_ALL_HRLY_WAGE_1998"),"NET_FAMILY_INCOME_R6478700"), 
            funs(logCPI1998))

# 2000
logCPI2000 <- function(wage) {
  wageLogCPI <- log(wage * 1.40) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("EMPLOYERS_ALL_HRLY_WAGE_2000"),"NET_FAMILY_INCOME_R7006500"), 
            funs(logCPI2000))

# 2002
logCPI2002 <- function(wage) {
  wageLogCPI <- log(wage * 1.34) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("EMPLOYERS_ALL_HRLY_WAGE_2002"),"NET_FAMILY_INCOME_R7703700"), 
            funs(logCPI2002))

# 2004
logCPI2004 <- function(wage) {
  wageLogCPI <- log(wage * 1.28) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("EMPLOYERS_ALL_HRLY_WAGE_2004"),"NET_FAMILY_INCOME_R8496100"), 
            funs(logCPI2004))

# 2006
logCPI2006 <- function(wage) {
  wageLogCPI <- log(wage * 1.19) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("EMPLOYERS_ALL_HRLY_WAGE_2006"),"NET_FAMILY_INCOME_T0987800"), 
            funs(logCPI2006))

# 2008
logCPI2008 <- function(wage) {
  wageLogCPI <- log(wage * 1.12) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("EMPLOYERS_ALL_HRLY_WAGE_2008"),"NET_FAMILY_INCOME_T2210000"), 
            funs(logCPI2008))

# 2010
logCPI2010 <- function(wage) {
  wageLogCPI <- log(wage * 1.09) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("EMPLOYERS_ALL_HRLY_WAGE_2010"),"NET_FAMILY_INCOME_T3107800"), 
            funs(logCPI2010))

# 2012
logCPI2012<- function(wage) {
  wageLogCPI <- log(wage * 1.05) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("EMPLOYERS_ALL_HRLY_WAGE_2012"),"NET_FAMILY_INCOME_T4112300"), 
            funs(logCPI2012))

# 2014
logCPI2014<- function(wage) {
  wageLogCPI <- log(wage * 1.01) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("EMPLOYERS_ALL_HRLY_WAGE_2014"),"NET_FAMILY_INCOME_T5022600"), 
            funs(logCPI2014))

# 2016
logCPI2016<- function(wage) {
  wageLogCPI <- log(wage * 1.00) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("EMPLOYERS_ALL_HRLY_WAGE_2016"),"NET_FAMILY_INCOME_T5770800"), 
            funs(logCPI2016))

### average age ###

# transforming age from years to weeks
ageToWeeks <- function(age) {
  ageWeeks <- age * 52
  return(ageWeeks)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("AGEATINT")), 
            funs(ageToWeeks))

# create age when started job
# average starting age and ending age per position and year

# 1979
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1979 = AGEATINT_R0216500 - TENURE1_R0068710,
         AGE_START_JOB2_1979 = AGEATINT_R0216500 - TENURE2_R0069010,
         AGE_START_JOB3_1979 = AGEATINT_R0216500 - TENURE3_R0069310,
         AGE_START_JOB4_1979 = AGEATINT_R0216500 - TENURE4_R0069610,
         AGE_START_JOB5_1979 = AGEATINT_R0216500 - TENURE5_R0069910)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1979 = (AGEATINT_R0216500 + AGE_START_JOB1_1979) / 2,
         MEAN_AGE_JOB2_1979 = (AGEATINT_R0216500 + AGE_START_JOB2_1979) / 2,
         MEAN_AGE_JOB3_1979 = (AGEATINT_R0216500 + AGE_START_JOB3_1979) / 2,
         MEAN_AGE_JOB4_1979 = (AGEATINT_R0216500 + AGE_START_JOB4_1979) / 2,
         MEAN_AGE_JOB5_1979 = (AGEATINT_R0216500 + AGE_START_JOB5_1979) / 2)

# 1980
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1980 = AGEATINT_R0406510 - TENURE1_R0333221,
         AGE_START_JOB2_1980 = AGEATINT_R0406510 - TENURE2_R0344721,
         AGE_START_JOB3_1980 = AGEATINT_R0406510 - TENURE3_R0356221,
         AGE_START_JOB4_1980 = AGEATINT_R0406510 - TENURE4_R0367721,
         AGE_START_JOB5_1980 = AGEATINT_R0406510 - TENURE5_R0379221)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1980 = (AGEATINT_R0406510 + AGE_START_JOB1_1980) / 2,
         MEAN_AGE_JOB2_1980 = (AGEATINT_R0406510 + AGE_START_JOB2_1980) / 2,
         MEAN_AGE_JOB3_1980 = (AGEATINT_R0406510 + AGE_START_JOB3_1980) / 2,
         MEAN_AGE_JOB4_1980 = (AGEATINT_R0406510 + AGE_START_JOB4_1980) / 2,
         MEAN_AGE_JOB5_1980 = (AGEATINT_R0406510 + AGE_START_JOB5_1980) / 2)

# 1981
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1981 = AGEATINT_R0619010 - TENURE1_R0539410,
         AGE_START_JOB2_1981 = AGEATINT_R0619010 - TENURE2_R0552510,
         AGE_START_JOB3_1981 = AGEATINT_R0619010 - TENURE3_R0565610,
         AGE_START_JOB4_1981 = AGEATINT_R0619010 - TENURE4_R0578710,
         AGE_START_JOB5_1981 = AGEATINT_R0619010 - TENURE5_R0591810)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1981 = (AGEATINT_R0619010 + AGE_START_JOB1_1981) / 2,
         MEAN_AGE_JOB2_1981 = (AGEATINT_R0619010 + AGE_START_JOB2_1981) / 2,
         MEAN_AGE_JOB3_1981 = (AGEATINT_R0619010 + AGE_START_JOB3_1981) / 2,
         MEAN_AGE_JOB4_1981 = (AGEATINT_R0619010 + AGE_START_JOB4_1981) / 2,
         MEAN_AGE_JOB5_1981 = (AGEATINT_R0619010 + AGE_START_JOB5_1981) / 2)

# 1982
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1982 = AGEATINT_R0898310 - TENURE1_R0833810,
         AGE_START_JOB2_1982 = AGEATINT_R0898310 - TENURE2_R0846910,
         AGE_START_JOB3_1982 = AGEATINT_R0898310 - TENURE3_R0860010,
         AGE_START_JOB4_1982 = AGEATINT_R0898310 - TENURE4_R0873110,
         AGE_START_JOB5_1982 = AGEATINT_R0898310 - TENURE5_R0886210)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1982 = (AGEATINT_R0898310 + AGE_START_JOB1_1982) / 2,
         MEAN_AGE_JOB2_1982 = (AGEATINT_R0898310 + AGE_START_JOB2_1982) / 2,
         MEAN_AGE_JOB3_1982 = (AGEATINT_R0898310 + AGE_START_JOB3_1982) / 2,
         MEAN_AGE_JOB4_1982 = (AGEATINT_R0898310 + AGE_START_JOB4_1982) / 2,
         MEAN_AGE_JOB5_1982 = (AGEATINT_R0898310 + AGE_START_JOB5_1982) / 2)

# 1983
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1983 = AGEATINT_R1145110 - TENURE1_R1081010,
         AGE_START_JOB2_1983 = AGEATINT_R1145110 - TENURE2_R1094210,
         AGE_START_JOB3_1983 = AGEATINT_R1145110 - TENURE3_R1107410,
         AGE_START_JOB4_1983 = AGEATINT_R1145110 - TENURE4_R1120610,
         AGE_START_JOB5_1983 = AGEATINT_R1145110 - TENURE5_R1133810)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1983 = (AGEATINT_R1145110 + AGE_START_JOB1_1983) / 2,
         MEAN_AGE_JOB2_1983 = (AGEATINT_R1145110 + AGE_START_JOB2_1983) / 2,
         MEAN_AGE_JOB3_1983 = (AGEATINT_R1145110 + AGE_START_JOB3_1983) / 2,
         MEAN_AGE_JOB4_1983 = (AGEATINT_R1145110 + AGE_START_JOB4_1983) / 2,
         MEAN_AGE_JOB5_1983 = (AGEATINT_R1145110 + AGE_START_JOB5_1983) / 2)

# 1984
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1984 = AGEATINT_R1520310 - TENURE1_R1456710,
         AGE_START_JOB2_1984 = AGEATINT_R1520310 - TENURE2_R1469810,
         AGE_START_JOB3_1984 = AGEATINT_R1520310 - TENURE3_R1482910,
         AGE_START_JOB4_1984 = AGEATINT_R1520310 - TENURE4_R1496010,
         AGE_START_JOB5_1984 = AGEATINT_R1520310 - TENURE5_R1509110)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1984 = (AGEATINT_R1520310 + AGE_START_JOB1_1984) / 2,
         MEAN_AGE_JOB2_1984 = (AGEATINT_R1520310 + AGE_START_JOB2_1984) / 2,
         MEAN_AGE_JOB3_1984 = (AGEATINT_R1520310 + AGE_START_JOB3_1984) / 2,
         MEAN_AGE_JOB4_1984 = (AGEATINT_R1520310 + AGE_START_JOB4_1984) / 2,
         MEAN_AGE_JOB5_1984 = (AGEATINT_R1520310 + AGE_START_JOB5_1984) / 2)

# 1985
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1985 = AGEATINT_R1891010 - TENURE1_R1803510,
         AGE_START_JOB2_1985 = AGEATINT_R1891010 - TENURE2_R1816210,
         AGE_START_JOB3_1985 = AGEATINT_R1891010 - TENURE3_R1828910,
         AGE_START_JOB4_1985 = AGEATINT_R1891010 - TENURE4_R1841610,
         AGE_START_JOB5_1985 = AGEATINT_R1891010 - TENURE5_R1854310)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1985 = (AGEATINT_R1891010 + AGE_START_JOB1_1985) / 2,
         MEAN_AGE_JOB2_1985 = (AGEATINT_R1891010 + AGE_START_JOB2_1985) / 2,
         MEAN_AGE_JOB3_1985 = (AGEATINT_R1891010 + AGE_START_JOB3_1985) / 2,
         MEAN_AGE_JOB4_1985 = (AGEATINT_R1891010 + AGE_START_JOB4_1985) / 2,
         MEAN_AGE_JOB5_1985 = (AGEATINT_R1891010 + AGE_START_JOB5_1985) / 2)

# 1986
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1986 = AGEATINT_R2258110 - TENURE1_R2165110,
         AGE_START_JOB2_1986 = AGEATINT_R2258110 - TENURE2_R2178710,
         AGE_START_JOB3_1986 = AGEATINT_R2258110 - TENURE3_R2192310,
         AGE_START_JOB4_1986 = AGEATINT_R2258110 - TENURE4_R2205910,
         AGE_START_JOB5_1986 = AGEATINT_R2258110 - TENURE5_R2219510)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1986 = (AGEATINT_R2258110 + AGE_START_JOB1_1986) / 2,
         MEAN_AGE_JOB2_1986 = (AGEATINT_R2258110 + AGE_START_JOB2_1986) / 2,
         MEAN_AGE_JOB3_1986 = (AGEATINT_R2258110 + AGE_START_JOB3_1986) / 2,
         MEAN_AGE_JOB4_1986 = (AGEATINT_R2258110 + AGE_START_JOB4_1986) / 2,
         MEAN_AGE_JOB5_1986 = (AGEATINT_R2258110 + AGE_START_JOB5_1986) / 2)

# 1987
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1987 = AGEATINT_R2445510 - TENURE1_R2372510,
         AGE_START_JOB2_1987 = AGEATINT_R2445510 - TENURE2_R2383810,
         AGE_START_JOB3_1987 = AGEATINT_R2445510 - TENURE3_R2395110,
         AGE_START_JOB4_1987 = AGEATINT_R2445510 - TENURE4_R2406410,
         AGE_START_JOB5_1987 = AGEATINT_R2445510 - TENURE5_R2417710)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1987 = (AGEATINT_R2445510 + AGE_START_JOB1_1987) / 2,
         MEAN_AGE_JOB2_1987 = (AGEATINT_R2445510 + AGE_START_JOB2_1987) / 2,
         MEAN_AGE_JOB3_1987 = (AGEATINT_R2445510 + AGE_START_JOB3_1987) / 2,
         MEAN_AGE_JOB4_1987 = (AGEATINT_R2445510 + AGE_START_JOB4_1987) / 2,
         MEAN_AGE_JOB5_1987 = (AGEATINT_R2445510 + AGE_START_JOB5_1987) / 2)

# 1988
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1988 = AGEATINT_R2871300 - TENURE1_R2763410,
         AGE_START_JOB2_1988 = AGEATINT_R2871300 - TENURE2_R2776310,
         AGE_START_JOB3_1988 = AGEATINT_R2871300 - TENURE3_R2789210,
         AGE_START_JOB4_1988 = AGEATINT_R2871300 - TENURE4_R2802110,
         AGE_START_JOB5_1988 = AGEATINT_R2871300 - TENURE5_R2815010)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1988 = (AGEATINT_R2871300 + AGE_START_JOB1_1988) / 2,
         MEAN_AGE_JOB2_1988 = (AGEATINT_R2871300 + AGE_START_JOB2_1988) / 2,
         MEAN_AGE_JOB3_1988 = (AGEATINT_R2871300 + AGE_START_JOB3_1988) / 2,
         MEAN_AGE_JOB4_1988 = (AGEATINT_R2871300 + AGE_START_JOB4_1988) / 2,
         MEAN_AGE_JOB5_1988 = (AGEATINT_R2871300 + AGE_START_JOB5_1988) / 2)

# 1989
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1989 = AGEATINT_R3075000 - TENURE1_R3005210,
         AGE_START_JOB2_1989 = AGEATINT_R3075000 - TENURE2_R3018310,
         AGE_START_JOB3_1989 = AGEATINT_R3075000 - TENURE3_R3031410,
         AGE_START_JOB4_1989 = AGEATINT_R3075000 - TENURE4_R3044510,
         AGE_START_JOB5_1989 = AGEATINT_R3075000 - TENURE5_R3057610)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1989 = (AGEATINT_R3075000 + AGE_START_JOB1_1989) / 2,
         MEAN_AGE_JOB2_1989 = (AGEATINT_R3075000 + AGE_START_JOB2_1989) / 2,
         MEAN_AGE_JOB3_1989 = (AGEATINT_R3075000 + AGE_START_JOB3_1989) / 2,
         MEAN_AGE_JOB4_1989 = (AGEATINT_R3075000 + AGE_START_JOB4_1989) / 2,
         MEAN_AGE_JOB5_1989 = (AGEATINT_R3075000 + AGE_START_JOB5_1989) / 2)

# 1990
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1990 = AGEATINT_R3401700 - TENURE1_R3332610,
         AGE_START_JOB2_1990 = AGEATINT_R3401700 - TENURE2_R3346610,
         AGE_START_JOB3_1990 = AGEATINT_R3401700 - TENURE3_R3360610,
         AGE_START_JOB4_1990 = AGEATINT_R3401700 - TENURE4_R3374610,
         AGE_START_JOB5_1990 = AGEATINT_R3401700 - TENURE5_R3388610)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1990 = (AGEATINT_R3401700 + AGE_START_JOB1_1990) / 2,
         MEAN_AGE_JOB2_1990 = (AGEATINT_R3401700 + AGE_START_JOB2_1990) / 2,
         MEAN_AGE_JOB3_1990 = (AGEATINT_R3401700 + AGE_START_JOB3_1990) / 2,
         MEAN_AGE_JOB4_1990 = (AGEATINT_R3401700 + AGE_START_JOB4_1990) / 2,
         MEAN_AGE_JOB5_1990 = (AGEATINT_R3401700 + AGE_START_JOB5_1990) / 2)

# 1991
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1991 = AGEATINT_R3657100 - TENURE1_R3597610,
         AGE_START_JOB2_1991 = AGEATINT_R3657100 - TENURE2_R3609710,
         AGE_START_JOB3_1991 = AGEATINT_R3657100 - TENURE3_R3621810,
         AGE_START_JOB4_1991 = AGEATINT_R3657100 - TENURE4_R3633910,
         AGE_START_JOB5_1991 = AGEATINT_R3657100 - TENURE5_R3646010)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1991 = (AGEATINT_R3657100 + AGE_START_JOB1_1991) / 2,
         MEAN_AGE_JOB2_1991 = (AGEATINT_R3657100 + AGE_START_JOB2_1991) / 2,
         MEAN_AGE_JOB3_1991 = (AGEATINT_R3657100 + AGE_START_JOB3_1991) / 2,
         MEAN_AGE_JOB4_1991 = (AGEATINT_R3657100 + AGE_START_JOB4_1991) / 2,
         MEAN_AGE_JOB5_1991 = (AGEATINT_R3657100 + AGE_START_JOB5_1991) / 2)

# 1992
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1992 = AGEATINT_R4007600 - TENURE1_R3947800,
         AGE_START_JOB2_1992 = AGEATINT_R4007600 - TENURE2_R3960000,
         AGE_START_JOB3_1992 = AGEATINT_R4007600 - TENURE3_R3972200,
         AGE_START_JOB4_1992 = AGEATINT_R4007600 - TENURE4_R3984400,
         AGE_START_JOB5_1992 = AGEATINT_R4007600 - TENURE5_R3996600)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1992 = (AGEATINT_R4007600 + AGE_START_JOB1_1992) / 2,
         MEAN_AGE_JOB2_1992 = (AGEATINT_R4007600 + AGE_START_JOB2_1992) / 2,
         MEAN_AGE_JOB3_1992 = (AGEATINT_R4007600 + AGE_START_JOB3_1992) / 2,
         MEAN_AGE_JOB4_1992 = (AGEATINT_R4007600 + AGE_START_JOB4_1992) / 2,
         MEAN_AGE_JOB5_1992 = (AGEATINT_R4007600 + AGE_START_JOB5_1992) / 2)

# 1993
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1993 = AGEATINT_R4418700 - TENURE1_R4416300,
         AGE_START_JOB2_1993 = AGEATINT_R4418700 - TENURE2_R4416400,
         AGE_START_JOB3_1993 = AGEATINT_R4418700 - TENURE3_R4416500,
         AGE_START_JOB4_1993 = AGEATINT_R4418700 - TENURE4_R4416600,
         AGE_START_JOB5_1993 = AGEATINT_R4418700 - TENURE5_R4416700)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1993 = (AGEATINT_R4418700 + AGE_START_JOB1_1993) / 2,
         MEAN_AGE_JOB2_1993 = (AGEATINT_R4418700 + AGE_START_JOB2_1993) / 2,
         MEAN_AGE_JOB3_1993 = (AGEATINT_R4418700 + AGE_START_JOB3_1993) / 2,
         MEAN_AGE_JOB4_1993 = (AGEATINT_R4418700 + AGE_START_JOB4_1993) / 2,
         MEAN_AGE_JOB5_1993 = (AGEATINT_R4418700 + AGE_START_JOB5_1993) / 2)

# 1994
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1994 = AGEATINT_R5081700 - TENURE1_R5079300,
         AGE_START_JOB2_1994 = AGEATINT_R5081700 - TENURE2_R5079400,
         AGE_START_JOB3_1994 = AGEATINT_R5081700 - TENURE3_R5079500,
         AGE_START_JOB4_1994 = AGEATINT_R5081700 - TENURE4_R5079600,
         AGE_START_JOB5_1994 = AGEATINT_R5081700 - TENURE5_R5079700)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1994 = (AGEATINT_R5081700 + AGE_START_JOB1_1994) / 2,
         MEAN_AGE_JOB2_1994 = (AGEATINT_R5081700 + AGE_START_JOB2_1994) / 2,
         MEAN_AGE_JOB3_1994 = (AGEATINT_R5081700 + AGE_START_JOB3_1994) / 2,
         MEAN_AGE_JOB4_1994 = (AGEATINT_R5081700 + AGE_START_JOB4_1994) / 2,
         MEAN_AGE_JOB5_1994 = (AGEATINT_R5081700 + AGE_START_JOB5_1994) / 2)

# 1996
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1996 = AGEATINT_R5167000 - TENURE1_R5164600,
         AGE_START_JOB2_1996 = AGEATINT_R5167000 - TENURE2_R5164700,
         AGE_START_JOB3_1996 = AGEATINT_R5167000 - TENURE3_R5164800,
         AGE_START_JOB4_1996 = AGEATINT_R5167000 - TENURE4_R5164900,
         AGE_START_JOB5_1996 = AGEATINT_R5167000 - TENURE5_R5165000)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1996 = (AGEATINT_R5167000 + AGE_START_JOB1_1996) / 2,
         MEAN_AGE_JOB2_1996 = (AGEATINT_R5167000 + AGE_START_JOB2_1996) / 2,
         MEAN_AGE_JOB3_1996 = (AGEATINT_R5167000 + AGE_START_JOB3_1996) / 2,
         MEAN_AGE_JOB4_1996 = (AGEATINT_R5167000 + AGE_START_JOB4_1996) / 2,
         MEAN_AGE_JOB5_1996 = (AGEATINT_R5167000 + AGE_START_JOB5_1996) / 2)

# 1998
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1998 = AGEATINT_R6479800 - TENURE1_R6477500,
         AGE_START_JOB2_1998 = AGEATINT_R6479800 - TENURE2_R6477600,
         AGE_START_JOB3_1998 = AGEATINT_R6479800 - TENURE3_R6477700,
         AGE_START_JOB4_1998 = AGEATINT_R6479800 - TENURE4_R6477800,
         AGE_START_JOB5_1998 = AGEATINT_R6479800 - TENURE5_R6477900)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1998 = (AGEATINT_R6479800 + AGE_START_JOB1_1998) / 2,
         MEAN_AGE_JOB2_1998 = (AGEATINT_R6479800 + AGE_START_JOB2_1998) / 2,
         MEAN_AGE_JOB3_1998 = (AGEATINT_R6479800 + AGE_START_JOB3_1998) / 2,
         MEAN_AGE_JOB4_1998 = (AGEATINT_R6479800 + AGE_START_JOB4_1998) / 2,
         MEAN_AGE_JOB5_1998 = (AGEATINT_R6479800 + AGE_START_JOB5_1998) / 2)

# 2000
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_2000 = AGEATINT_R7007500 - TENURE1_R7005200,
         AGE_START_JOB2_2000 = AGEATINT_R7007500 - TENURE2_R7005300,
         AGE_START_JOB3_2000 = AGEATINT_R7007500 - TENURE3_R7005400,
         AGE_START_JOB4_2000 = AGEATINT_R7007500 - TENURE4_R7005500,
         AGE_START_JOB5_2000 = AGEATINT_R7007500 - TENURE5_R7005600)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_2000 = (AGEATINT_R7007500 + AGE_START_JOB1_2000) / 2,
         MEAN_AGE_JOB2_2000 = (AGEATINT_R7007500 + AGE_START_JOB2_2000) / 2,
         MEAN_AGE_JOB3_2000 = (AGEATINT_R7007500 + AGE_START_JOB3_2000) / 2,
         MEAN_AGE_JOB4_2000 = (AGEATINT_R7007500 + AGE_START_JOB4_2000) / 2,
         MEAN_AGE_JOB5_2000 = (AGEATINT_R7007500 + AGE_START_JOB5_2000) / 2)

# 2002
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_2002 = AGEATINT_R7704800 - TENURE1_R7702400,
         AGE_START_JOB2_2002 = AGEATINT_R7704800 - TENURE2_R7702500,
         AGE_START_JOB3_2002 = AGEATINT_R7704800 - TENURE3_R7702600,
         AGE_START_JOB4_2002 = AGEATINT_R7704800 - TENURE4_R7702700,
         AGE_START_JOB5_2002 = AGEATINT_R7704800 - TENURE5_R7702800)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_2002 = (AGEATINT_R7704800 + AGE_START_JOB1_2002) / 2,
         MEAN_AGE_JOB2_2002 = (AGEATINT_R7704800 + AGE_START_JOB2_2002) / 2,
         MEAN_AGE_JOB3_2002 = (AGEATINT_R7704800 + AGE_START_JOB3_2002) / 2,
         MEAN_AGE_JOB4_2002 = (AGEATINT_R7704800 + AGE_START_JOB4_2002) / 2,
         MEAN_AGE_JOB5_2002 = (AGEATINT_R7704800 + AGE_START_JOB5_2002) / 2)

# 2004
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_2004 = AGEATINT_R8497200 - TENURE1_R8494700,
         AGE_START_JOB2_2004 = AGEATINT_R8497200 - TENURE2_R8494800,
         AGE_START_JOB3_2004 = AGEATINT_R8497200 - TENURE3_R8494900,
         AGE_START_JOB4_2004 = AGEATINT_R8497200 - TENURE4_R8495000,
         AGE_START_JOB5_2004 = AGEATINT_R8497200 - TENURE5_R8495100)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_2004 = (AGEATINT_R8497200 + AGE_START_JOB1_2004) / 2,
         MEAN_AGE_JOB2_2004 = (AGEATINT_R8497200 + AGE_START_JOB2_2004) / 2,
         MEAN_AGE_JOB3_2004 = (AGEATINT_R8497200 + AGE_START_JOB3_2004) / 2,
         MEAN_AGE_JOB4_2004 = (AGEATINT_R8497200 + AGE_START_JOB4_2004) / 2,
         MEAN_AGE_JOB5_2004 = (AGEATINT_R8497200 + AGE_START_JOB5_2004) / 2)

# 2006
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_2006 = AGEATINT_T0989000 - TENURE1_T0986300,
         AGE_START_JOB2_2006 = AGEATINT_T0989000 - TENURE2_T0986400,
         AGE_START_JOB3_2006 = AGEATINT_T0989000 - TENURE3_T0986500,
         AGE_START_JOB4_2006 = AGEATINT_T0989000 - TENURE4_T0986600,
         AGE_START_JOB5_2006 = AGEATINT_T0989000 - TENURE5_T0986700)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_2006 = (AGEATINT_T0989000 + AGE_START_JOB1_2006) / 2,
         MEAN_AGE_JOB2_2006 = (AGEATINT_T0989000 + AGE_START_JOB2_2006) / 2,
         MEAN_AGE_JOB3_2006 = (AGEATINT_T0989000 + AGE_START_JOB3_2006) / 2,
         MEAN_AGE_JOB4_2006 = (AGEATINT_T0989000 + AGE_START_JOB4_2006) / 2,
         MEAN_AGE_JOB5_2006 = (AGEATINT_T0989000 + AGE_START_JOB5_2006) / 2)

# 2008
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_2008 = AGEATINT_T2210800 - TENURE1_T2208600,
         AGE_START_JOB2_2008 = AGEATINT_T2210800 - TENURE2_T2208700,
         AGE_START_JOB3_2008 = AGEATINT_T2210800 - TENURE3_T2208800,
         AGE_START_JOB4_2008 = AGEATINT_T2210800 - TENURE4_T2208900,
         AGE_START_JOB5_2008 = AGEATINT_T2210800 - TENURE5_T2209000)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_2008 = (AGEATINT_T2210800 + AGE_START_JOB1_2008) / 2,
         MEAN_AGE_JOB2_2008 = (AGEATINT_T2210800 + AGE_START_JOB2_2008) / 2,
         MEAN_AGE_JOB3_2008 = (AGEATINT_T2210800 + AGE_START_JOB3_2008) / 2,
         MEAN_AGE_JOB4_2008 = (AGEATINT_T2210800 + AGE_START_JOB4_2008) / 2,
         MEAN_AGE_JOB5_2008 = (AGEATINT_T2210800 + AGE_START_JOB5_2008) / 2)

# 2010
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_2010 = AGEATINT_T3108700 - TENURE1_T3106400,
         AGE_START_JOB2_2010 = AGEATINT_T3108700 - TENURE2_T3106500,
         AGE_START_JOB3_2010 = AGEATINT_T3108700 - TENURE3_T3106600,
         AGE_START_JOB4_2010 = AGEATINT_T3108700 - TENURE4_T3106700,
         AGE_START_JOB5_2010 = AGEATINT_T3108700 - TENURE5_T3106800)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_2010 = (AGEATINT_T3108700 + AGE_START_JOB1_2010) / 2,
         MEAN_AGE_JOB2_2010 = (AGEATINT_T3108700 + AGE_START_JOB2_2010) / 2,
         MEAN_AGE_JOB3_2010 = (AGEATINT_T3108700 + AGE_START_JOB3_2010) / 2,
         MEAN_AGE_JOB4_2010 = (AGEATINT_T3108700 + AGE_START_JOB4_2010) / 2,
         MEAN_AGE_JOB5_2010 = (AGEATINT_T3108700 + AGE_START_JOB5_2010) / 2)

# 2012
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_2012 = AGEATINT_T4113200 - TENURE1_T4110400,
         AGE_START_JOB2_2012 = AGEATINT_T4113200 - TENURE2_T4110500,
         AGE_START_JOB3_2012 = AGEATINT_T4113200 - TENURE3_T4110600,
         AGE_START_JOB4_2012 = AGEATINT_T4113200 - TENURE4_T4110700,
         AGE_START_JOB5_2012 = AGEATINT_T4113200 - TENURE5_T4110800)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_2012 = (AGEATINT_T4113200 + AGE_START_JOB1_2012) / 2,
         MEAN_AGE_JOB2_2012 = (AGEATINT_T4113200 + AGE_START_JOB2_2012) / 2,
         MEAN_AGE_JOB3_2012 = (AGEATINT_T4113200 + AGE_START_JOB3_2012) / 2,
         MEAN_AGE_JOB4_2012 = (AGEATINT_T4113200 + AGE_START_JOB4_2012) / 2,
         MEAN_AGE_JOB5_2012 = (AGEATINT_T4113200 + AGE_START_JOB5_2012) / 2)

# 2014
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_2014 = AGEATINT_T5023600 - TENURE1_T5019200,
         AGE_START_JOB2_2014 = AGEATINT_T5023600 - TENURE2_T5019300,
         AGE_START_JOB3_2014 = AGEATINT_T5023600 - TENURE3_T5019400,
         AGE_START_JOB4_2014 = AGEATINT_T5023600 - TENURE4_T5019500,
         AGE_START_JOB5_2014 = AGEATINT_T5023600 - TENURE5_T5019600)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_2014 = (AGEATINT_T5023600 + AGE_START_JOB1_2014) / 2,
         MEAN_AGE_JOB2_2014 = (AGEATINT_T5023600 + AGE_START_JOB2_2014) / 2,
         MEAN_AGE_JOB3_2014 = (AGEATINT_T5023600 + AGE_START_JOB3_2014) / 2,
         MEAN_AGE_JOB4_2014 = (AGEATINT_T5023600 + AGE_START_JOB4_2014) / 2,
         MEAN_AGE_JOB5_2014 = (AGEATINT_T5023600 + AGE_START_JOB5_2014) / 2)

# 2016
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_2016 = AGEATINT_T5771500 - TENURE1_T5768100,
         AGE_START_JOB2_2016 = AGEATINT_T5771500 - TENURE2_T5768200,
         AGE_START_JOB3_2016 = AGEATINT_T5771500 - TENURE3_T5768300,
         AGE_START_JOB4_2016 = AGEATINT_T5771500 - TENURE4_T5768400,
         AGE_START_JOB5_2016 = AGEATINT_T5771500 - TENURE5_T5768500)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_2016 = (AGEATINT_T5771500 + AGE_START_JOB1_2016) / 2,
         MEAN_AGE_JOB2_2016 = (AGEATINT_T5771500 + AGE_START_JOB2_2016) / 2,
         MEAN_AGE_JOB3_2016 = (AGEATINT_T5771500 + AGE_START_JOB3_2016) / 2,
         MEAN_AGE_JOB4_2016 = (AGEATINT_T5771500 + AGE_START_JOB4_2016) / 2,
         MEAN_AGE_JOB5_2016 = (AGEATINT_T5771500 + AGE_START_JOB5_2016) / 2)


# transform age back to years
ageToYears <- function(age) {
  ageYears <- age / 52
  return(ageYears)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("AGEATINT")), 
            funs(ageToYears))

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("MEAN_AGE_JOB")), 
            funs(ageToYears))


### job satisfaction flip scale ###
# replacing NAs with 0
trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("JOB_SATISFACTION")), funs(replace_na(., 0)))

# function to flip
scaleFlip <- function(score){
    if(score == 4){
      return(1)
    }else if(score == 3){
      return(2)
    }else if(score == 2){
      return(3)
    }else if(score == 1){
      return(4)
    }else{
      return(0)
  }
}

# job satisfaction columns ## NOT WORKING YET ##
jobSatisfactionColumns <- trainData79 %>%
  select(starts_with("JOB_SATISFACTION")) %>%
  mapply(. %>% scaleFlip)

