library(tidyverse)

# switching variable names to words vs codes
varNames <- read.csv("varNames79.csv")

names(trainData79) <- varNames[match(names(trainData79), varNames[ ,'Code']),'Name']

##### FEATURE ENGINEERING #####

### replacing all "skip" values with NA
trainData79 <- trainData79 %>% 
  mutate_each(funs(replace(., .< 0, NA)))


### job satisfaction ###

# replacing NAs with 0
trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("JOB_SATISFACTION")), 
            funs(replace_na(., 0)))

# flip scale so 4 = 1, 3 = 2, 2 = 3, and 1 = 4 *** NOT WORKING YET ***
scaleFlip <- function(score){
    if(score == 4){
      score <- 1
    }else if(score == 3){
      score <- 2
    }else if(score == 2){
      score <- 3
    }else if(score == 1){
      score <- 4
    }else{
      return(0)
  }
}

test <- trainData79 %>% 
  mutate_at(vars(starts_with("JOB_SATISFACTION")), 
             funs(scaleFlip))


# replacing all NAs with 0 in the "currently working" columns
trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("CURRENTLY_WORKING")), 
            funs(replace_na(., 0)))

# 1979
trainData79$JOB_SATISFACTION.01_1979 <- NA
trainData79$JOB_SATISFACTION.02_1979 <- NA
trainData79$JOB_SATISFACTION.03_1979 <- NA
trainData79$JOB_SATISFACTION.04_1979 <- NA
trainData79$JOB_SATISFACTION.05_1979 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY_WORKING.01_1979"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1979"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1979"]
  }else if (trainData79[i,"CURRENTLY_WORKING.02_1979"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1979"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1979"]
  }else if (trainData79[i,"CURRENTLY_WORKING.03_1979"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1979"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1979"]
  }else if (trainData79[i,"CURRENTLY_WORKING.04_1979"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1979"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1979"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1979"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1979"]
  }
}

# 1980
trainData79$JOB_SATISFACTION.01_1980 <- NA
trainData79$JOB_SATISFACTION.02_1980 <- NA
trainData79$JOB_SATISFACTION.03_1980 <- NA
trainData79$JOB_SATISFACTION.04_1980 <- NA
trainData79$JOB_SATISFACTION.05_1980 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY_WORKING.01_1980"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1980"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1980"]
  }else if (trainData79[i,"CURRENTLY_WORKING.02_1980"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1980"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1980"]
  }else if (trainData79[i,"CURRENTLY_WORKING.03_1980"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1980"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1980"]
  }else if (trainData79[i,"CURRENTLY_WORKING.04_1980"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1980"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1980"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1980"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1980"]
  }
}

# 1981
trainData79$JOB_SATISFACTION.01_1981 <- NA
trainData79$JOB_SATISFACTION.02_1981 <- NA
trainData79$JOB_SATISFACTION.03_1981 <- NA
trainData79$JOB_SATISFACTION.04_1981 <- NA
trainData79$JOB_SATISFACTION.05_1981 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY_WORKING.01_1981"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1981"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1981"]
  }else if (trainData79[i,"CURRENTLY_WORKING.02_1981"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1981"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1981"]
  }else if (trainData79[i,"CURRENTLY_WORKING.03_1981"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1981"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1981"]
  }else if (trainData79[i,"CURRENTLY_WORKING.04_1981"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1981"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1981"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1981"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1981"]
  }
}

# 1982
trainData79$JOB_SATISFACTION.01_1982 <- NA
trainData79$JOB_SATISFACTION.02_1982 <- NA
trainData79$JOB_SATISFACTION.03_1982 <- NA
trainData79$JOB_SATISFACTION.04_1982 <- NA
trainData79$JOB_SATISFACTION.05_1982 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY_WORKING.01_1982"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1982"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1982"]
  }else if (trainData79[i,"CURRENTLY_WORKING.02_1982"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1982"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1982"]
  }else if (trainData79[i,"CURRENTLY_WORKING.03_1982"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1982"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1982"]
  }else if (trainData79[i,"CURRENTLY_WORKING.04_1982"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1982"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1982"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1982"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1982"]
  }
}

# 1983
trainData79$JOB_SATISFACTION.01_1983 <- NA
trainData79$JOB_SATISFACTION.02_1983 <- NA
trainData79$JOB_SATISFACTION.03_1983 <- NA
trainData79$JOB_SATISFACTION.04_1983 <- NA
trainData79$JOB_SATISFACTION.05_1983 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY_WORKING.01_1983"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1983"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1983"]
  }else if (trainData79[i,"CURRENTLY_WORKING.02_1983"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1983"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1983"]
  }else if (trainData79[i,"CURRENTLY_WORKING.03_1983"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1983"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1983"]
  }else if (trainData79[i,"CURRENTLY_WORKING.04_1983"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1983"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1983"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1983"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1983"]
  }
}

# 1984
trainData79$JOB_SATISFACTION.01_1984 <- NA
trainData79$JOB_SATISFACTION.02_1984 <- NA
trainData79$JOB_SATISFACTION.03_1984 <- NA
trainData79$JOB_SATISFACTION.04_1984 <- NA
trainData79$JOB_SATISFACTION.05_1984 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY_WORKING.01_1984"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1984"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1984"]
  }else if (trainData79[i,"CURRENTLY_WORKING.02_1984"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1984"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1984"]
  }else if (trainData79[i,"CURRENTLY_WORKING.03_1984"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1984"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1984"]
  }else if (trainData79[i,"CURRENTLY_WORKING.04_1984"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1984"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1984"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1984"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1984"]
  }
}

# 1985
trainData79$JOB_SATISFACTION.01_1985 <- NA
trainData79$JOB_SATISFACTION.02_1985 <- NA
trainData79$JOB_SATISFACTION.03_1985 <- NA
trainData79$JOB_SATISFACTION.04_1985 <- NA
trainData79$JOB_SATISFACTION.05_1985 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY_WORKING.01_1985"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1985"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1985"]
  }else if (trainData79[i,"CURRENTLY_WORKING.02_1985"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1985"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1985"]
  }else if (trainData79[i,"CURRENTLY_WORKING.03_1985"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1985"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1985"]
  }else if (trainData79[i,"CURRENTLY_WORKING.04_1985"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1985"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1985"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1985"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1985"]
  }
}

# 1986
trainData79$JOB_SATISFACTION.01_1986 <- NA
trainData79$JOB_SATISFACTION.02_1986 <- NA
trainData79$JOB_SATISFACTION.03_1986 <- NA
trainData79$JOB_SATISFACTION.04_1986 <- NA
trainData79$JOB_SATISFACTION.05_1986 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY_WORKING.01_1986"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1986"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1986"]
  }else if (trainData79[i,"CURRENTLY_WORKING.02_1986"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1986"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1986"]
  }else if (trainData79[i,"CURRENTLY_WORKING.03_1986"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1986"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1986"]
  }else if (trainData79[i,"CURRENTLY_WORKING.04_1986"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1986"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1986"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1986"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1986"]
  }
}

# 1987
trainData79$JOB_SATISFACTION.01_1987 <- NA
trainData79$JOB_SATISFACTION.02_1987 <- NA
trainData79$JOB_SATISFACTION.03_1987 <- NA
trainData79$JOB_SATISFACTION.04_1987 <- NA
trainData79$JOB_SATISFACTION.05_1987 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY_WORKING.01_1987"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1987"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1987"]
  }else if (trainData79[i,"CURRENTLY_WORKING.02_1987"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1987"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1987"]
  }else if (trainData79[i,"CURRENTLY_WORKING.03_1987"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1987"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1987"]
  }else if (trainData79[i,"CURRENTLY_WORKING.04_1987"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1987"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1987"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1987"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1987"]
  }
}

# 1988
trainData79$JOB_SATISFACTION.01_1988 <- NA
trainData79$JOB_SATISFACTION.02_1988 <- NA
trainData79$JOB_SATISFACTION.03_1988 <- NA
trainData79$JOB_SATISFACTION.04_1988 <- NA
trainData79$JOB_SATISFACTION.05_1988 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY_WORKING.01_1988"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1988"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1988"]
  }else if (trainData79[i,"CURRENTLY_WORKING.02_1988"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1988"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1988"]
  }else if (trainData79[i,"CURRENTLY_WORKING.03_1988"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1988"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1988"]
  }else if (trainData79[i,"CURRENTLY_WORKING.04_1988"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1988"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1988"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1988"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1988"]
  }
}

# 1989
trainData79$JOB_SATISFACTION.01_1989 <- NA
trainData79$JOB_SATISFACTION.02_1989 <- NA
trainData79$JOB_SATISFACTION.03_1989 <- NA
trainData79$JOB_SATISFACTION.04_1989 <- NA
trainData79$JOB_SATISFACTION.05_1989 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY_WORKING.01_1989"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1989"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1989"]
  }else if (trainData79[i,"CURRENTLY_WORKING.02_1989"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1989"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1989"]
  }else if (trainData79[i,"CURRENTLY_WORKING.03_1989"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1989"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1989"]
  }else if (trainData79[i,"CURRENTLY_WORKING.04_1989"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1989"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1989"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1989"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1989"]
  }
}

# 1990
trainData79$JOB_SATISFACTION.01_1990 <- NA
trainData79$JOB_SATISFACTION.02_1990 <- NA
trainData79$JOB_SATISFACTION.03_1990 <- NA
trainData79$JOB_SATISFACTION.04_1990 <- NA
trainData79$JOB_SATISFACTION.05_1990 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY_WORKING.01_1990"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1990"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1990"]
  }else if (trainData79[i,"CURRENTLY_WORKING.02_1990"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1990"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1990"]
  }else if (trainData79[i,"CURRENTLY_WORKING.03_1990"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1990"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1990"]
  }else if (trainData79[i,"CURRENTLY_WORKING.04_1990"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1990"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1990"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1990"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1990"]
  }
}

# 1991
trainData79$JOB_SATISFACTION.01_1991 <- NA
trainData79$JOB_SATISFACTION.02_1991 <- NA
trainData79$JOB_SATISFACTION.03_1991 <- NA
trainData79$JOB_SATISFACTION.04_1991 <- NA
trainData79$JOB_SATISFACTION.05_1991 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY_WORKING.01_1991"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1991"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1991"]
  }else if (trainData79[i,"CURRENTLY_WORKING.02_1991"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1991"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1991"]
  }else if (trainData79[i,"CURRENTLY_WORKING.03_1991"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1991"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1991"]
  }else if (trainData79[i,"CURRENTLY_WORKING.04_1991"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1991"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1991"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1991"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1991"]
  }
}

# 1992
trainData79$JOB_SATISFACTION.01_1992 <- NA
trainData79$JOB_SATISFACTION.02_1992 <- NA
trainData79$JOB_SATISFACTION.03_1992 <- NA
trainData79$JOB_SATISFACTION.04_1992 <- NA
trainData79$JOB_SATISFACTION.05_1992 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY_WORKING.01_1992"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1992"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1992"]
  }else if (trainData79[i,"CURRENTLY_WORKING.02_1992"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1992"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1992"]
  }else if (trainData79[i,"CURRENTLY_WORKING.03_1992"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1992"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1992"]
  }else if (trainData79[i,"CURRENTLY_WORKING.04_1992"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1992"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1992"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1992"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1992"]
  }
}

# 1993
trainData79$JOB_SATISFACTION.01_1993 <- NA
trainData79$JOB_SATISFACTION.02_1993 <- NA
trainData79$JOB_SATISFACTION.03_1993 <- NA
trainData79$JOB_SATISFACTION.04_1993 <- NA
trainData79$JOB_SATISFACTION.05_1993 <- NA

for(i in 1:nrow(trainData79)){
  if (trainData79[i,"CURRENTLY_WORKING.01_1993"] == 1){
    trainData79[i,"JOB_SATISFACTION.01_1993"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1993"]
  }else if (trainData79[i,"CURRENTLY_WORKING.02_1993"] == 1){
    trainData79[i,"JOB_SATISFACTION.02_1993"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1993"]
  }else if (trainData79[i,"CURRENTLY_WORKING.03_1993"] == 1){
    trainData79[i,"JOB_SATISFACTION.03_1993"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1993"]
  }else if (trainData79[i,"CURRENTLY_WORKING.04_1993"] == 1){
    trainData79[i,"JOB_SATISFACTION.04_1993"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1993"]
  }else{
    trainData79[i,"JOB_SATISFACTION.05_1993"] <- trainData79[i,"JOB_SATISFACTION_GLOBAL_1993"]
  }
}


### sector ###
# 1979
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1979 = case_when(SECTOR.02_1979 %in% c(1,3,4) ~ 0, 
                                    SECTOR.02_1979 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1979 = case_when(SECTOR.02_1979 %in% c(1,3,4) ~ 0, 
                                    SECTOR.02_1979 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1979 = case_when(SECTOR.03_1979 %in% c(1,3,4) ~ 0, 
                                    SECTOR.03_1979 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1979 = case_when(SECTOR.04_1979 %in% c(1,3,4) ~ 0, 
                                    SECTOR.04_1979 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1979 = case_when(SECTOR.05_1979 %in% c(1,3,4) ~ 0, 
                                    SECTOR.05_1979 == 2 ~ 1))

# 1980
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1980 = case_when(SECTOR.01_1980 %in% c(1,3,4) ~ 0, 
                                    SECTOR.01_1980 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1980 = case_when(SECTOR.02_1980 %in% c(1,3,4) ~ 0, 
                                    SECTOR.02_1980 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1980 = case_when(SECTOR.03_1980 %in% c(1,3,4) ~ 0, 
                                    SECTOR.03_1980 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1980 = case_when(SECTOR.04_1980 %in% c(1,3,4) ~ 0, 
                                    SECTOR.04_1980 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1980 = case_when(SECTOR.05_1980 %in% c(1,3,4) ~ 0, 
                                    SECTOR.05_1980 == 2 ~ 1))

# 1981
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1981 = case_when(SECTOR.01_1981 %in% c(1,3,4) ~ 0, 
                                    SECTOR.01_1981 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1981 = case_when(SECTOR.02_1981 %in% c(1,3,4) ~ 0, 
                                    SECTOR.02_1981 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1981 = case_when(SECTOR.03_1981 %in% c(1,3,4) ~ 0, 
                                    SECTOR.03_1981 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1981 = case_when(SECTOR.04_1981 %in% c(1,3,4) ~ 0, 
                                    SECTOR.04_1981 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1981 = case_when(SECTOR.05_1981 %in% c(1,3,4) ~ 0, 
                                    SECTOR.05_1981 == 2 ~ 1))

# 1982
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1982 = case_when(SECTOR.01_1982 %in% c(1,3,4) ~ 0, 
                                    SECTOR.01_1982 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1982 = case_when(SECTOR.02_1982 %in% c(1,3,4) ~ 0, 
                                    SECTOR.02_1982 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1982 = case_when(SECTOR.03_1982 %in% c(1,3,4) ~ 0, 
                                    SECTOR.03_1982 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1982 = case_when(SECTOR.04_1982 %in% c(1,3,4) ~ 0, 
                                    SECTOR.04_1982 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1982 = case_when(SECTOR.05_1982 %in% c(1,3,4) ~ 0, 
                                    SECTOR.05_1982 == 2 ~ 1))

# 1983
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1983 = case_when(SECTOR.01_1983 %in% c(1,3,4) ~ 0, 
                                    SECTOR.01_1983 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1983 = case_when(SECTOR.02_1983 %in% c(1,3,4) ~ 0, 
                                    SECTOR.02_1983 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1983 = case_when(SECTOR.03_1983  %in% c(1,3,4) ~ 0, 
                                    SECTOR.03_1983  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1983 = case_when(SECTOR.04_1983 %in% c(1,3,4) ~ 0, 
                                    SECTOR.04_1983 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1983 = case_when(SECTOR.05_1983 %in% c(1,3,4) ~ 0, 
                                    SECTOR.05_1983 == 2 ~ 1))


# 1984
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1984 = case_when(SECTOR.01_1984 %in% c(1,3,4) ~ 0, 
                                    SECTOR.01_1984 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1984 = case_when(SECTOR.02_1984 %in% c(1,3,4) ~ 0, 
                                    SECTOR.02_1984 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1984 = case_when(SECTOR.03_1984  %in% c(1,3,4) ~ 0, 
                                    SECTOR.03_1984  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1984 = case_when(SECTOR.04_1984 %in% c(1,3,4) ~ 0, 
                                    SECTOR.04_1984 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1984 = case_when(SECTOR.05_1984 %in% c(1,3,4) ~ 0, 
                                    SECTOR.05_1984 == 2 ~ 1))

# 1985
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1985 = case_when(SECTOR.01_1985 %in% c(1,3,4) ~ 0, 
                                    SECTOR.01_1985 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1985 = case_when(SECTOR.02_1985 %in% c(1,3,4) ~ 0, 
                                    SECTOR.02_1985 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1985 = case_when(SECTOR.03_1985  %in% c(1,3,4) ~ 0, 
                                    SECTOR.03_1985  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1985 = case_when(SECTOR.04_1985 %in% c(1,3,4) ~ 0, 
                                    SECTOR.04_1985 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1985 = case_when(SECTOR.05_1985 %in% c(1,3,4) ~ 0, 
                                    SECTOR.05_1985 == 2 ~ 1))

# 1986
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1986 = case_when(SECTOR.01_1986 %in% c(1,3,4) ~ 0, 
                                    SECTOR.01_1986 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1986 = case_when(SECTOR.02_1986 %in% c(1,3,4) ~ 0, 
                                    SECTOR.02_1986 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1986 = case_when(SECTOR.03_1986   %in% c(1,3,4) ~ 0, 
                                    SECTOR.03_1986   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1986 = case_when(SECTOR.04_1986  %in% c(1,3,4) ~ 0, 
                                    SECTOR.04_1986  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1986 = case_when(SECTOR.05_1986  %in% c(1,3,4) ~ 0, 
                                    SECTOR.05_1986  == 2 ~ 1))

# 1987
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1987 = case_when(SECTOR.01_1987 %in% c(1,3,4) ~ 0, 
                                    SECTOR.01_1987 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1987 = case_when(SECTOR.02_1987 %in% c(1,3,4) ~ 0, 
                                    SECTOR.02_1987 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1987 = case_when(SECTOR.03_1987   %in% c(1,3,4) ~ 0, 
                                    SECTOR.03_1987   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1987 = case_when(SECTOR.04_1987  %in% c(1,3,4) ~ 0, 
                                    SECTOR.04_1987  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1987 = case_when(SECTOR.05_1987  %in% c(1,3,4) ~ 0, 
                                    SECTOR.05_1987  == 2 ~ 1))

# 1988
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1988 = case_when(SECTOR.01_1988 %in% c(1,3,4) ~ 0, 
                                    SECTOR.01_1988 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1988 = case_when(SECTOR.02_1988 %in% c(1,3,4) ~ 0, 
                                    SECTOR.02_1988 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1988 = case_when(SECTOR.03_1988   %in% c(1,3,4) ~ 0, 
                                    SECTOR.03_1988   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1988 = case_when(SECTOR.04_1988  %in% c(1,3,4) ~ 0, 
                                    SECTOR.04_1988  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1988 = case_when(SECTOR.05_1988  %in% c(1,3,4) ~ 0, 
                                    SECTOR.05_1988  == 2 ~ 1))

# 1989
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1989 = case_when(SECTOR.01_1989 %in% c(1,3,4) ~ 0, 
                                    SECTOR.01_1989 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1989 = case_when(SECTOR.02_1989 %in% c(1,3,4) ~ 0, 
                                    SECTOR.02_1989 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1989 = case_when(SECTOR.03_1989   %in% c(1,3,4) ~ 0, 
                                    SECTOR.03_1989   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1989 = case_when(SECTOR.04_1989  %in% c(1,3,4) ~ 0, 
                                    SECTOR.04_1989  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1989 = case_when(SECTOR.05_1989  %in% c(1,3,4) ~ 0, 
                                    SECTOR.05_1989  == 2 ~ 1))

# 1990
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1990 = case_when(SECTOR.01_1990 %in% c(1,3,4) ~ 0, 
                                    SECTOR.01_1990 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1990 = case_when(SECTOR.02_1990 %in% c(1,3,4) ~ 0, 
                                    SECTOR.02_1990 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1990 = case_when(SECTOR.03_1990   %in% c(1,3,4) ~ 0, 
                                    SECTOR.03_1990   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1990 = case_when(SECTOR.04_1990  %in% c(1,3,4) ~ 0, 
                                    SECTOR.04_1990  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1990 = case_when(SECTOR.05_1990  %in% c(1,3,4) ~ 0, 
                                    SECTOR.05_1990  == 2 ~ 1))

# 1991
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1991 = case_when(SECTOR.01_1991 %in% c(1,3,4) ~ 0, 
                                    SECTOR.01_1991 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1991 = case_when(SECTOR.02_1991 %in% c(1,3,4) ~ 0, 
                                    SECTOR.02_1991 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1991 = case_when(SECTOR.03_1991   %in% c(1,3,4) ~ 0, 
                                    SECTOR.03_1991   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1991 = case_when(SECTOR.04_1991  %in% c(1,3,4) ~ 0, 
                                    SECTOR.04_1991  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1991 = case_when(SECTOR.05_1991  %in% c(1,3,4) ~ 0, 
                                    SECTOR.05_1991  == 2 ~ 1))

# 1992
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1992 = case_when(SECTOR.01_1992 %in% c(1,3,4) ~ 0, 
                                    SECTOR.01_1992 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1992 = case_when(SECTOR.02_1992 %in% c(1,3,4) ~ 0, 
                                    SECTOR.02_1992 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1992 = case_when(SECTOR.03_1992   %in% c(1,3,4) ~ 0, 
                                    SECTOR.03_1992   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1992 = case_when(SECTOR.04_1992  %in% c(1,3,4) ~ 0, 
                                    SECTOR.04_1992  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1992 = case_when(SECTOR.05_1992  %in% c(1,3,4) ~ 0, 
                                    SECTOR.05_1992  == 2 ~ 1))     
       
# 1993
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1993 = case_when(SECTOR.01_1993 %in% c(1,3,4) ~ 0, 
                                    SECTOR.01_1993 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1993 = case_when(SECTOR.02_1993 %in% c(1,3,4) ~ 0, 
                                    SECTOR.02_1993 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1993 = case_when(SECTOR.03_1993   %in% c(1,3,4) ~ 0, 
                                    SECTOR.03_1993   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1993 = case_when(SECTOR.04_1993  %in% c(1,3,4) ~ 0, 
                                    SECTOR.04_1993  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1993 = case_when(SECTOR.05_1993  %in% c(1,3,4) ~ 0, 
                                    SECTOR.05_1993  == 2 ~ 1))   

# 1994
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1994 = case_when(SECTOR.01_1994 %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.01_1994 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1994 = case_when(SECTOR.02_1994 %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.02_1994 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1994 = case_when(SECTOR.03_1994   %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.03_1994   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1994 = case_when(SECTOR.04_1994  %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.04_1994  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1994 = case_when(SECTOR.05_1994  %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.05_1994  == 2 ~ 1))

# 1996
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1996 = case_when(SECTOR.01_1996 %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.01_1996 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1996 = case_when(SECTOR.02_1996 %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.02_1996 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1996 = case_when(SECTOR.03_1996   %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.03_1996   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1996 = case_when(SECTOR.04_1996  %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.04_1996  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1996 = case_when(SECTOR.05_1996  %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.05_1996  == 2 ~ 1))


# 1998
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_1998 = case_when(SECTOR.01_1998 %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.01_1998 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_1998 = case_when(SECTOR.02_1998 %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.02_1998 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_1998 = case_when(SECTOR.03_1998   %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.03_1998   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_1998 = case_when(SECTOR.04_1998  %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.04_1998  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_1998 = case_when(SECTOR.05_1998  %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.05_1998  == 2 ~ 1))
 
# 2000
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_2000 = case_when(SECTOR.01_2000 %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.01_2000 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_2000 = case_when(SECTOR.02_2000 %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.02_2000 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_2000 = case_when(SECTOR.03_2000   %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.03_2000   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_2000 = case_when(SECTOR.04_2000  %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.04_2000  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_2000 = case_when(SECTOR.05_2000  %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.05_2000  == 2 ~ 1))

# 2002
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_2002 = case_when(SECTOR.01_2002 %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.01_2002 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_2002 = case_when(SECTOR.02_2002 %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.02_2002 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_2002 = case_when(SECTOR.03_2002   %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.03_2002   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_2002 = case_when(SECTOR.04_2002  %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.04_2002  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_2002 = case_when(SECTOR.05_2002  %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.05_2002  == 2 ~ 1))

# 2004
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_2004 = case_when(SECTOR.01_2004 %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.01_2004 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_2004 = case_when(SECTOR.02_2004 %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.02_2004 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_2004 = case_when(SECTOR.03_2004   %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.03_2004   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_2004 = case_when(SECTOR.04_2004  %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.04_2004  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_2004 = case_when(SECTOR.05_2004  %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.05_2004  == 2 ~ 1))

# 2006
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_2006 = case_when(SECTOR.01_2006 %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.01_2006 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_2006 = case_when(SECTOR.02_2006 %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.02_2006 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_2006 = case_when(SECTOR.03_2006   %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.03_2006   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_2006 = case_when(SECTOR.04_2006  %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.04_2006  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_2006 = case_when(SECTOR.05_2006  %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.05_2006  == 2 ~ 1))

# 2008
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_2008 = case_when(SECTOR.01_2008 %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.01_2008 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_2008 = case_when(SECTOR.02_2008 %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.02_2008 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_2008 = case_when(SECTOR.03_2008   %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.03_2008   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_2008 = case_when(SECTOR.04_2008  %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.04_2008  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_2008 = case_when(SECTOR.05_2008  %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.05_2008  == 2 ~ 1))

# 2010
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_2010 = case_when(SECTOR.01_2010 %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.01_2010 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_2010 = case_when(SECTOR.02_2010 %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.02_2010 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_2010 = case_when(SECTOR.03_2010   %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.03_2010   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_2010 = case_when(SECTOR.04_2010  %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.04_2010  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_2010 = case_when(SECTOR.05_2010  %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.05_2010  == 2 ~ 1))

# 2012
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_2012 = case_when(SECTOR.01_2012 %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.01_2012 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_2012 = case_when(SECTOR.02_2012 %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.02_2012 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_2012 = case_when(SECTOR.03_2012   %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.03_2012   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_2012 = case_when(SECTOR.04_2012  %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.04_2012  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_2012 = case_when(SECTOR.05_2012  %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.05_2012  == 2 ~ 1))

# 2014
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_2014 = case_when(SECTOR.01_2014 %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.01_2014 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_2014 = case_when(SECTOR.02_2014 %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.02_2014 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_2014 = case_when(SECTOR.03_2014   %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.03_2014   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_2014 = case_when(SECTOR.04_2014  %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.04_2014  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_2014 = case_when(SECTOR.05_2014  %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.05_2014  == 2 ~ 1))

# 2016
trainData79 <- trainData79 %>%
  mutate(PUBLIC.01_2016 = case_when(SECTOR.01_2016 %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.01_2016 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.02_2016 = case_when(SECTOR.02_2016 %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.02_2016 == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.03_2016 = case_when(SECTOR.03_2016   %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.03_2016   == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.04_2016 = case_when(SECTOR.04_2016  %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.04_2016  == 2 ~ 1))

trainData79 <- trainData79 %>%
  mutate(PUBLIC.05_2016 = case_when(SECTOR.05_2016  %in% c(1,3,4,5) ~ 0, 
                                    SECTOR.05_2016  == 2 ~ 1))

### wages and income ###

# 1979
logCPI1979 <- function(wage) {
  wageLogCPI <- log(wage * 3.47) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("HOURLY_PAY_1979"),"NET_FAMILY_INCOME_1979"), 
            funs(logCPI1979))

# 1980
logCPI1980 <- function(wage) {
  wageLogCPI <- log(wage * 3.05) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("HOURLY_PAY_1980"),"NET_FAMILY_INCOME_1980"), 
            funs(logCPI1980))


# 1981
logCPI1981 <- function(wage) {
  wageLogCPI <- log(wage * 2.72) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("HOURLY_PAY_1981"),"NET_FAMILY_INCOME_1981"), 
            funs(logCPI1981))

# 1982
logCPI1982 <- function(wage) {
  wageLogCPI <- log(wage * 2.51) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("HOURLY_PAY_1982"),"NET_FAMILY_INCOME_1982"), 
            funs(logCPI1982))

# 1983
logCPI1983 <- function(wage) {
  wageLogCPI <- log(wage * 2.42) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("HOURLY_PAY_1983"),"NET_FAMILY_INCOME_1983"), 
            funs(logCPI1983))

# 1984
logCPI1984 <- function(wage) {
  wageLogCPI <- log(wage * 2.32) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("HOURLY_PAY_1984"),"NET_FAMILY_INCOME_1984"), 
            funs(logCPI1984))

# 1985
logCPI1985 <- function(wage) {
  wageLogCPI <- log(wage * 2.25) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("HOURLY_PAY_1985"),"NET_FAMILY_INCOME_1985"), 
            funs(logCPI1985))

# 1986
logCPI1986 <- function(wage) {
  wageLogCPI <- log(wage * 2.16) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("HOURLY_PAY_1986"),"NET_FAMILY_INCOME_1986"), 
            funs(logCPI1986))

# 1987
logCPI1987 <- function(wage) {
  wageLogCPI <- log(wage * 2.13) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("HOURLY_PAY_1987"),"NET_FAMILY_INCOME_1987"), 
            funs(logCPI1987))

# 1988
logCPI1988 <- function(wage) {
  wageLogCPI <- log(wage * 2.05) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("HOURLY_PAY_1988"),"NET_FAMILY_INCOME_1988"), 
            funs(logCPI1988))

# 1989
logCPI1989 <- function(wage) {
  wageLogCPI <- log(wage * 1.96) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("HOURLY_PAY_1989"),"NET_FAMILY_INCOME_1989"), 
            funs(logCPI1989))

# 1990
logCPI1990 <- function(wage) {
  wageLogCPI <- log(wage * 1.86) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("HOURLY_PAY_1990"),"NET_FAMILY_INCOME_1990"), 
            funs(logCPI1990))

# 1991
logCPI1991 <- function(wage) {
  wageLogCPI <- log(wage * 1.76) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("HOURLY_PAY_1991"),"NET_FAMILY_INCOME_1991"), 
            funs(logCPI1991))

# 1992
logCPI1992 <- function(wage) {
  wageLogCPI <- log(wage * 1.72) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("HOURLY_PAY_1992"),"NET_FAMILY_INCOME_1992"), 
            funs(logCPI1992))

# 1993
logCPI1993 <- function(wage) {
  wageLogCPI <- log(wage * 1.66) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("HOURLY_PAY_1993"),"NET_FAMILY_INCOME_1993"), 
            funs(logCPI1993))

# 1994
logCPI1994 <- function(wage) {
  wageLogCPI <- log(wage * 1.62) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("HOURLY_PAY_1994"),"NET_FAMILY_INCOME_1994"), 
            funs(logCPI1994))

# 1996
logCPI1996 <- function(wage) {
  wageLogCPI <- log(wage * 1.53) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("HOURLY_PAY_1996"),"NET_FAMILY_INCOME_1996"), 
            funs(logCPI1996))

# 1998
logCPI1998 <- function(wage) {
  wageLogCPI <- log(wage * 1.47) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("HOURLY_PAY_1998"),"NET_FAMILY_INCOME_1998"), 
            funs(logCPI1998))

# 2000
logCPI2000 <- function(wage) {
  wageLogCPI <- log(wage * 1.40) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("HOURLY_PAY_2000"),"NET_FAMILY_INCOME_2000"), 
            funs(logCPI2000))

# 2002
logCPI2002 <- function(wage) {
  wageLogCPI <- log(wage * 1.34) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("HOURLY_PAY_2002"),"NET_FAMILY_INCOME_2002"), 
            funs(logCPI2002))

# 2004
logCPI2004 <- function(wage) {
  wageLogCPI <- log(wage * 1.28) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("HOURLY_PAY_2004"),"NET_FAMILY_INCOME_2004"), 
            funs(logCPI2004))

# 2006
logCPI2006 <- function(wage) {
  wageLogCPI <- log(wage * 1.19) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("HOURLY_PAY_2006"),"NET_FAMILY_INCOME_2006"), 
            funs(logCPI2006))

# 2008
logCPI2008 <- function(wage) {
  wageLogCPI <- log(wage * 1.12) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("HOURLY_PAY_2008"),"NET_FAMILY_INCOME_2008"), 
            funs(logCPI2008))

# 2010
logCPI2010 <- function(wage) {
  wageLogCPI <- log(wage * 1.09) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("HOURLY_PAY_2010"),"NET_FAMILY_INCOME_2010"), 
            funs(logCPI2010))

# 2012
logCPI2012<- function(wage) {
  wageLogCPI <- log(wage * 1.05) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("HOURLY_PAY_2012"),"NET_FAMILY_INCOME_2012"), 
            funs(logCPI2012))

# 2014
logCPI2014<- function(wage) {
  wageLogCPI <- log(wage * 1.01) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("HOURLY_PAY_2014"),"NET_FAMILY_INCOME_2014"), 
            funs(logCPI2014))

# 2016
logCPI2016<- function(wage) {
  wageLogCPI <- log(wage * 1.00) 
  return(wageLogCPI)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("HOURLY_PAY_2016"),"NET_FAMILY_INCOME_2016"), 
            funs(logCPI2016))

# replacing -Inf caused by 0 with 0 
trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("NET_FAMILY_INCOME")), 
            funs(replace(., .< 0, 0)))


### average age ###

# transforming age from years to weeks
ageToWeeks <- function(age) {
  ageWeeks <- age * 52
  return(ageWeeks)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("AGE")), 
            funs(ageToWeeks))

# create age when started job
# average starting age and ending age per position and year

# 1979
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1979 = AGE_1979 - TENURE.01_1979,
         AGE_START_JOB2_1979 = AGE_1979 - TENURE.02_1979,
         AGE_START_JOB3_1979 = AGE_1979 - TENURE.03_1979,
         AGE_START_JOB4_1979 = AGE_1979 - TENURE.04_1979,
         AGE_START_JOB5_1979 = AGE_1979 - TENURE.05_1979)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1979 = (AGE_1979 + AGE_START_JOB1_1979) / 2,
         MEAN_AGE_JOB2_1979 = (AGE_1979 + AGE_START_JOB2_1979) / 2,
         MEAN_AGE_JOB3_1979 = (AGE_1979 + AGE_START_JOB3_1979) / 2,
         MEAN_AGE_JOB4_1979 = (AGE_1979 + AGE_START_JOB4_1979) / 2,
         MEAN_AGE_JOB5_1979 = (AGE_1979 + AGE_START_JOB5_1979) / 2)

# 1980
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1980 = AGE_1980 - TENURE.01_1980,
         AGE_START_JOB2_1980 = AGE_1980 - TENURE.02_1980,
         AGE_START_JOB3_1980 = AGE_1980 - TENURE.03_1980,
         AGE_START_JOB4_1980 = AGE_1980 - TENURE.04_1980,
         AGE_START_JOB5_1980 = AGE_1980 - TENURE.05_1980)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1980 = (AGE_1980 + AGE_START_JOB1_1980) / 2,
         MEAN_AGE_JOB2_1980 = (AGE_1980 + AGE_START_JOB2_1980) / 2,
         MEAN_AGE_JOB3_1980 = (AGE_1980 + AGE_START_JOB3_1980) / 2,
         MEAN_AGE_JOB4_1980 = (AGE_1980 + AGE_START_JOB4_1980) / 2,
         MEAN_AGE_JOB5_1980 = (AGE_1980 + AGE_START_JOB5_1980) / 2)

# 1981
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1981 = AGE_1981 - TENURE.01_1981,
         AGE_START_JOB2_1981 = AGE_1981 - TENURE.02_1981,
         AGE_START_JOB3_1981 = AGE_1981 - TENURE.03_1981,
         AGE_START_JOB4_1981 = AGE_1981 - TENURE.04_1981,
         AGE_START_JOB5_1981 = AGE_1981 - TENURE.05_1981)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1981 = (AGE_1981 + AGE_START_JOB1_1981) / 2,
         MEAN_AGE_JOB2_1981 = (AGE_1981 + AGE_START_JOB2_1981) / 2,
         MEAN_AGE_JOB3_1981 = (AGE_1981 + AGE_START_JOB3_1981) / 2,
         MEAN_AGE_JOB4_1981 = (AGE_1981 + AGE_START_JOB4_1981) / 2,
         MEAN_AGE_JOB5_1981 = (AGE_1981 + AGE_START_JOB5_1981) / 2)

# 1982
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1982 = AGE_1982 - TENURE.01_1982,
         AGE_START_JOB2_1982 = AGE_1982 - TENURE.02_1982,
         AGE_START_JOB3_1982 = AGE_1982 - TENURE.03_1982,
         AGE_START_JOB4_1982 = AGE_1982 - TENURE.04_1982,
         AGE_START_JOB5_1982 = AGE_1982 - TENURE.05_1982)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1982 = (AGE_1982 + AGE_START_JOB1_1982) / 2,
         MEAN_AGE_JOB2_1982 = (AGE_1982 + AGE_START_JOB2_1982) / 2,
         MEAN_AGE_JOB3_1982 = (AGE_1982 + AGE_START_JOB3_1982) / 2,
         MEAN_AGE_JOB4_1982 = (AGE_1982 + AGE_START_JOB4_1982) / 2,
         MEAN_AGE_JOB5_1982 = (AGE_1982 + AGE_START_JOB5_1982) / 2)

# 1983
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1983 = AGE_1983 - TENURE.01_1983,
         AGE_START_JOB2_1983 = AGE_1983 - TENURE.02_1983,
         AGE_START_JOB3_1983 = AGE_1983 - TENURE.03_1983,
         AGE_START_JOB4_1983 = AGE_1983 - TENURE.04_1983,
         AGE_START_JOB5_1983 = AGE_1983 - TENURE.05_1983)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1983 = (AGE_1983 + AGE_START_JOB1_1983) / 2,
         MEAN_AGE_JOB2_1983 = (AGE_1983 + AGE_START_JOB2_1983) / 2,
         MEAN_AGE_JOB3_1983 = (AGE_1983 + AGE_START_JOB3_1983) / 2,
         MEAN_AGE_JOB4_1983 = (AGE_1983 + AGE_START_JOB4_1983) / 2,
         MEAN_AGE_JOB5_1983 = (AGE_1983 + AGE_START_JOB5_1983) / 2)

# 1984
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1984 = AGE_1984 - TENURE.01_1984,
         AGE_START_JOB2_1984 = AGE_1984 - TENURE.02_1984,
         AGE_START_JOB3_1984 = AGE_1984 - TENURE.03_1984,
         AGE_START_JOB4_1984 = AGE_1984 - TENURE.04_1984,
         AGE_START_JOB5_1984 = AGE_1984 - TENURE.05_1984)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1984 = (AGE_1984 + AGE_START_JOB1_1984) / 2,
         MEAN_AGE_JOB2_1984 = (AGE_1984 + AGE_START_JOB2_1984) / 2,
         MEAN_AGE_JOB3_1984 = (AGE_1984 + AGE_START_JOB3_1984) / 2,
         MEAN_AGE_JOB4_1984 = (AGE_1984 + AGE_START_JOB4_1984) / 2,
         MEAN_AGE_JOB5_1984 = (AGE_1984 + AGE_START_JOB5_1984) / 2)

# 1985
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1985 = AGE_1985 - TENURE.01_1985,
         AGE_START_JOB2_1985 = AGE_1985 - TENURE.02_1985,
         AGE_START_JOB3_1985 = AGE_1985 - TENURE.03_1985,
         AGE_START_JOB4_1985 = AGE_1985 - TENURE.04_1985,
         AGE_START_JOB5_1985 = AGE_1985 - TENURE.05_1985)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1985 = (AGE_1985 + AGE_START_JOB1_1985) / 2,
         MEAN_AGE_JOB2_1985 = (AGE_1985 + AGE_START_JOB2_1985) / 2,
         MEAN_AGE_JOB3_1985 = (AGE_1985 + AGE_START_JOB3_1985) / 2,
         MEAN_AGE_JOB4_1985 = (AGE_1985 + AGE_START_JOB4_1985) / 2,
         MEAN_AGE_JOB5_1985 = (AGE_1985 + AGE_START_JOB5_1985) / 2)

# 1986
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1986 = AGE_1986 - TENURE.01_1986,
         AGE_START_JOB2_1986 = AGE_1986 - TENURE.02_1986,
         AGE_START_JOB3_1986 = AGE_1986 - TENURE.03_1986,
         AGE_START_JOB4_1986 = AGE_1986 - TENURE.04_1986,
         AGE_START_JOB5_1986 = AGE_1986 - TENURE.05_1986)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1986 = (AGE_1986 + AGE_START_JOB1_1986) / 2,
         MEAN_AGE_JOB2_1986 = (AGE_1986 + AGE_START_JOB2_1986) / 2,
         MEAN_AGE_JOB3_1986 = (AGE_1986 + AGE_START_JOB3_1986) / 2,
         MEAN_AGE_JOB4_1986 = (AGE_1986 + AGE_START_JOB4_1986) / 2,
         MEAN_AGE_JOB5_1986 = (AGE_1986 + AGE_START_JOB5_1986) / 2)

# 1987
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1987 = AGE_1987 - TENURE.01_1987,
         AGE_START_JOB2_1987 = AGE_1987 - TENURE.02_1987,
         AGE_START_JOB3_1987 = AGE_1987 - TENURE.03_1987,
         AGE_START_JOB4_1987 = AGE_1987 - TENURE.04_1987,
         AGE_START_JOB5_1987 = AGE_1987 - TENURE.05_1987)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1987 = (AGE_1987 + AGE_START_JOB1_1987) / 2,
         MEAN_AGE_JOB2_1987 = (AGE_1987 + AGE_START_JOB2_1987) / 2,
         MEAN_AGE_JOB3_1987 = (AGE_1987 + AGE_START_JOB3_1987) / 2,
         MEAN_AGE_JOB4_1987 = (AGE_1987 + AGE_START_JOB4_1987) / 2,
         MEAN_AGE_JOB5_1987 = (AGE_1987 + AGE_START_JOB5_1987) / 2)

# 1988
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1988 = AGE_1988 - TENURE.01_1988,
         AGE_START_JOB2_1988 = AGE_1988 - TENURE.02_1988,
         AGE_START_JOB3_1988 = AGE_1988 - TENURE.03_1988,
         AGE_START_JOB4_1988 = AGE_1988 - TENURE.04_1988,
         AGE_START_JOB5_1988 = AGE_1988 - TENURE.05_1988)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1988 = (AGE_1988 + AGE_START_JOB1_1988) / 2,
         MEAN_AGE_JOB2_1988 = (AGE_1988 + AGE_START_JOB2_1988) / 2,
         MEAN_AGE_JOB3_1988 = (AGE_1988 + AGE_START_JOB3_1988) / 2,
         MEAN_AGE_JOB4_1988 = (AGE_1988 + AGE_START_JOB4_1988) / 2,
         MEAN_AGE_JOB5_1988 = (AGE_1988 + AGE_START_JOB5_1988) / 2)

# 1989
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1989 = AGE_1989 - TENURE.01_1989,
         AGE_START_JOB2_1989 = AGE_1989 - TENURE.02_1989,
         AGE_START_JOB3_1989 = AGE_1989 - TENURE.03_1989,
         AGE_START_JOB4_1989 = AGE_1989 - TENURE.04_1989,
         AGE_START_JOB5_1989 = AGE_1989 - TENURE.05_1989)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1989 = (AGE_1989 + AGE_START_JOB1_1989) / 2,
         MEAN_AGE_JOB2_1989 = (AGE_1989 + AGE_START_JOB2_1989) / 2,
         MEAN_AGE_JOB3_1989 = (AGE_1989 + AGE_START_JOB3_1989) / 2,
         MEAN_AGE_JOB4_1989 = (AGE_1989 + AGE_START_JOB4_1989) / 2,
         MEAN_AGE_JOB5_1989 = (AGE_1989 + AGE_START_JOB5_1989) / 2)

# 1990
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1990 = AGE_1990 - TENURE.01_1990,
         AGE_START_JOB2_1990 = AGE_1990 - TENURE.02_1990,
         AGE_START_JOB3_1990 = AGE_1990 - TENURE.03_1990,
         AGE_START_JOB4_1990 = AGE_1990 - TENURE.04_1990,
         AGE_START_JOB5_1990 = AGE_1990 - TENURE.05_1990)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1990 = (AGE_1990 + AGE_START_JOB1_1990) / 2,
         MEAN_AGE_JOB2_1990 = (AGE_1990 + AGE_START_JOB2_1990) / 2,
         MEAN_AGE_JOB3_1990 = (AGE_1990 + AGE_START_JOB3_1990) / 2,
         MEAN_AGE_JOB4_1990 = (AGE_1990 + AGE_START_JOB4_1990) / 2,
         MEAN_AGE_JOB5_1990 = (AGE_1990 + AGE_START_JOB5_1990) / 2)

# 1991
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1991 = AGE_1991 - TENURE.01_1991,
         AGE_START_JOB2_1991 = AGE_1991 - TENURE.02_1991,
         AGE_START_JOB3_1991 = AGE_1991 - TENURE.03_1991,
         AGE_START_JOB4_1991 = AGE_1991 - TENURE.04_1991,
         AGE_START_JOB5_1991 = AGE_1991 - TENURE.05_1991)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1991 = (AGE_1991 + AGE_START_JOB1_1991) / 2,
         MEAN_AGE_JOB2_1991 = (AGE_1991 + AGE_START_JOB2_1991) / 2,
         MEAN_AGE_JOB3_1991 = (AGE_1991 + AGE_START_JOB3_1991) / 2,
         MEAN_AGE_JOB4_1991 = (AGE_1991 + AGE_START_JOB4_1991) / 2,
         MEAN_AGE_JOB5_1991 = (AGE_1991 + AGE_START_JOB5_1991) / 2)

# 1992
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1992 = AGE_1992 - TENURE.01_1992,
         AGE_START_JOB2_1992 = AGE_1992 - TENURE.02_1992,
         AGE_START_JOB3_1992 = AGE_1992 - TENURE.03_1992,
         AGE_START_JOB4_1992 = AGE_1992 - TENURE.04_1992,
         AGE_START_JOB5_1992 = AGE_1992 - TENURE.05_1992)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1992 = (AGE_1992 + AGE_START_JOB1_1992) / 2,
         MEAN_AGE_JOB2_1992 = (AGE_1992 + AGE_START_JOB2_1992) / 2,
         MEAN_AGE_JOB3_1992 = (AGE_1992 + AGE_START_JOB3_1992) / 2,
         MEAN_AGE_JOB4_1992 = (AGE_1992 + AGE_START_JOB4_1992) / 2,
         MEAN_AGE_JOB5_1992 = (AGE_1992 + AGE_START_JOB5_1992) / 2)

# 1993
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1993 = AGE_1993 - TENURE.01_1993,
         AGE_START_JOB2_1993 = AGE_1993 - TENURE.02_1993,
         AGE_START_JOB3_1993 = AGE_1993 - TENURE.03_1993,
         AGE_START_JOB4_1993 = AGE_1993 - TENURE.04_1993,
         AGE_START_JOB5_1993 = AGE_1993 - TENURE.05_1993)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1993 = (AGE_1993 + AGE_START_JOB1_1993) / 2,
         MEAN_AGE_JOB2_1993 = (AGE_1993 + AGE_START_JOB2_1993) / 2,
         MEAN_AGE_JOB3_1993 = (AGE_1993 + AGE_START_JOB3_1993) / 2,
         MEAN_AGE_JOB4_1993 = (AGE_1993 + AGE_START_JOB4_1993) / 2,
         MEAN_AGE_JOB5_1993 = (AGE_1993 + AGE_START_JOB5_1993) / 2)

# 1994
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1994 = AGE_1994 - TENURE.01_1994,
         AGE_START_JOB2_1994 = AGE_1994 - TENURE.02_1994,
         AGE_START_JOB3_1994 = AGE_1994 - TENURE.03_1994,
         AGE_START_JOB4_1994 = AGE_1994 - TENURE.04_1994,
         AGE_START_JOB5_1994 = AGE_1994 - TENURE.05_1994)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1994 = (AGE_1994 + AGE_START_JOB1_1994) / 2,
         MEAN_AGE_JOB2_1994 = (AGE_1994 + AGE_START_JOB2_1994) / 2,
         MEAN_AGE_JOB3_1994 = (AGE_1994 + AGE_START_JOB3_1994) / 2,
         MEAN_AGE_JOB4_1994 = (AGE_1994 + AGE_START_JOB4_1994) / 2,
         MEAN_AGE_JOB5_1994 = (AGE_1994 + AGE_START_JOB5_1994) / 2)

# 1996
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1996 = AGE_1996 - TENURE.01_1996,
         AGE_START_JOB2_1996 = AGE_1996 - TENURE.02_1996,
         AGE_START_JOB3_1996 = AGE_1996 - TENURE.03_1996,
         AGE_START_JOB4_1996 = AGE_1996 - TENURE.04_1996,
         AGE_START_JOB5_1996 = AGE_1996 - TENURE.05_1996)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1996 = (AGE_1996 + AGE_START_JOB1_1996) / 2,
         MEAN_AGE_JOB2_1996 = (AGE_1996 + AGE_START_JOB2_1996) / 2,
         MEAN_AGE_JOB3_1996 = (AGE_1996 + AGE_START_JOB3_1996) / 2,
         MEAN_AGE_JOB4_1996 = (AGE_1996 + AGE_START_JOB4_1996) / 2,
         MEAN_AGE_JOB5_1996 = (AGE_1996 + AGE_START_JOB5_1996) / 2)

# 1998
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_1998 = AGE_1998 - TENURE.01_1998,
         AGE_START_JOB2_1998 = AGE_1998 - TENURE.02_1998,
         AGE_START_JOB3_1998 = AGE_1998 - TENURE.03_1998,
         AGE_START_JOB4_1998 = AGE_1998 - TENURE.04_1998,
         AGE_START_JOB5_1998 = AGE_1998 - TENURE.05_1998)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_1998 = (AGE_1998 + AGE_START_JOB1_1998) / 2,
         MEAN_AGE_JOB2_1998 = (AGE_1998 + AGE_START_JOB2_1998) / 2,
         MEAN_AGE_JOB3_1998 = (AGE_1998 + AGE_START_JOB3_1998) / 2,
         MEAN_AGE_JOB4_1998 = (AGE_1998 + AGE_START_JOB4_1998) / 2,
         MEAN_AGE_JOB5_1998 = (AGE_1998 + AGE_START_JOB5_1998) / 2)

# 2000
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_2000 = AGE_2000 - TENURE.01_2000,
         AGE_START_JOB2_2000 = AGE_2000 - TENURE.02_2000,
         AGE_START_JOB3_2000 = AGE_2000 - TENURE.03_2000,
         AGE_START_JOB4_2000 = AGE_2000 - TENURE.04_2000,
         AGE_START_JOB5_2000 = AGE_2000 - TENURE.05_2000)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_2000 = (AGE_2000 + AGE_START_JOB1_2000) / 2,
         MEAN_AGE_JOB2_2000 = (AGE_2000 + AGE_START_JOB2_2000) / 2,
         MEAN_AGE_JOB3_2000 = (AGE_2000 + AGE_START_JOB3_2000) / 2,
         MEAN_AGE_JOB4_2000 = (AGE_2000 + AGE_START_JOB4_2000) / 2,
         MEAN_AGE_JOB5_2000 = (AGE_2000 + AGE_START_JOB5_2000) / 2)

# 2002
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_2002 = AGE_2002 - TENURE.01_2002,
         AGE_START_JOB2_2002 = AGE_2002 - TENURE.02_2002,
         AGE_START_JOB3_2002 = AGE_2002 - TENURE.03_2002,
         AGE_START_JOB4_2002 = AGE_2002 - TENURE.04_2002,
         AGE_START_JOB5_2002 = AGE_2002 - TENURE.05_2002)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_2002 = (AGE_2002 + AGE_START_JOB1_2002) / 2,
         MEAN_AGE_JOB2_2002 = (AGE_2002 + AGE_START_JOB2_2002) / 2,
         MEAN_AGE_JOB3_2002 = (AGE_2002 + AGE_START_JOB3_2002) / 2,
         MEAN_AGE_JOB4_2002 = (AGE_2002 + AGE_START_JOB4_2002) / 2,
         MEAN_AGE_JOB5_2002 = (AGE_2002 + AGE_START_JOB5_2002) / 2)

# 2004
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_2004 = AGE_2004 - TENURE.01_2004,
         AGE_START_JOB2_2004 = AGE_2004 - TENURE.02_2004,
         AGE_START_JOB3_2004 = AGE_2004 - TENURE.03_2004,
         AGE_START_JOB4_2004 = AGE_2004 - TENURE.04_2004,
         AGE_START_JOB5_2004 = AGE_2004 - TENURE.05_2004)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_2004 = (AGE_2004 + AGE_START_JOB1_2004) / 2,
         MEAN_AGE_JOB2_2004 = (AGE_2004 + AGE_START_JOB2_2004) / 2,
         MEAN_AGE_JOB3_2004 = (AGE_2004 + AGE_START_JOB3_2004) / 2,
         MEAN_AGE_JOB4_2004 = (AGE_2004 + AGE_START_JOB4_2004) / 2,
         MEAN_AGE_JOB5_2004 = (AGE_2004 + AGE_START_JOB5_2004) / 2)

# 2006
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_2006 = AGE_2006 - TENURE.01_2006,
         AGE_START_JOB2_2006 = AGE_2006 - TENURE.02_2006,
         AGE_START_JOB3_2006 = AGE_2006 - TENURE.03_2006,
         AGE_START_JOB4_2006 = AGE_2006 - TENURE.04_2006,
         AGE_START_JOB5_2006 = AGE_2006 - TENURE.05_2006)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_2006 = (AGE_2006 + AGE_START_JOB1_2006) / 2,
         MEAN_AGE_JOB2_2006 = (AGE_2006 + AGE_START_JOB2_2006) / 2,
         MEAN_AGE_JOB3_2006 = (AGE_2006 + AGE_START_JOB3_2006) / 2,
         MEAN_AGE_JOB4_2006 = (AGE_2006 + AGE_START_JOB4_2006) / 2,
         MEAN_AGE_JOB5_2006 = (AGE_2006 + AGE_START_JOB5_2006) / 2)

# 2008
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_2008 = AGE_2008 - TENURE.01_2008,
         AGE_START_JOB2_2008 = AGE_2008 - TENURE.02_2008,
         AGE_START_JOB3_2008 = AGE_2008 - TENURE.03_2008,
         AGE_START_JOB4_2008 = AGE_2008 - TENURE.04_2008,
         AGE_START_JOB5_2008 = AGE_2008 - TENURE.05_2008)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_2008 = (AGE_2008 + AGE_START_JOB1_2008) / 2,
         MEAN_AGE_JOB2_2008 = (AGE_2008 + AGE_START_JOB2_2008) / 2,
         MEAN_AGE_JOB3_2008 = (AGE_2008 + AGE_START_JOB3_2008) / 2,
         MEAN_AGE_JOB4_2008 = (AGE_2008 + AGE_START_JOB4_2008) / 2,
         MEAN_AGE_JOB5_2008 = (AGE_2008 + AGE_START_JOB5_2008) / 2)

# 2010
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_2010 = AGE_2010 - TENURE.01_2010,
         AGE_START_JOB2_2010 = AGE_2010 - TENURE.02_2010,
         AGE_START_JOB3_2010 = AGE_2010 - TENURE.03_2010,
         AGE_START_JOB4_2010 = AGE_2010 - TENURE.04_2010,
         AGE_START_JOB5_2010 = AGE_2010 - TENURE.05_2010)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_2010 = (AGE_2010 + AGE_START_JOB1_2010) / 2,
         MEAN_AGE_JOB2_2010 = (AGE_2010 + AGE_START_JOB2_2010) / 2,
         MEAN_AGE_JOB3_2010 = (AGE_2010 + AGE_START_JOB3_2010) / 2,
         MEAN_AGE_JOB4_2010 = (AGE_2010 + AGE_START_JOB4_2010) / 2,
         MEAN_AGE_JOB5_2010 = (AGE_2010 + AGE_START_JOB5_2010) / 2)

# 2012
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_2012 = AGE_2012 - TENURE.01_2012,
         AGE_START_JOB2_2012 = AGE_2012 - TENURE.02_2012,
         AGE_START_JOB3_2012 = AGE_2012 - TENURE.03_2012,
         AGE_START_JOB4_2012 = AGE_2012 - TENURE.04_2012,
         AGE_START_JOB5_2012 = AGE_2012 - TENURE.05_2012)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_2012 = (AGE_2012 + AGE_START_JOB1_2012) / 2,
         MEAN_AGE_JOB2_2012 = (AGE_2012 + AGE_START_JOB2_2012) / 2,
         MEAN_AGE_JOB3_2012 = (AGE_2012 + AGE_START_JOB3_2012) / 2,
         MEAN_AGE_JOB4_2012 = (AGE_2012 + AGE_START_JOB4_2012) / 2,
         MEAN_AGE_JOB5_2012 = (AGE_2012 + AGE_START_JOB5_2012) / 2)

# 2014
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_2014 = AGE_2014 - TENURE.01_2014,
         AGE_START_JOB2_2014 = AGE_2014 - TENURE.02_2014,
         AGE_START_JOB3_2014 = AGE_2014 - TENURE.03_2014,
         AGE_START_JOB4_2014 = AGE_2014 - TENURE.04_2014,
         AGE_START_JOB5_2014 = AGE_2014 - TENURE.05_2014)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_2014 = (AGE_2014 + AGE_START_JOB1_2014) / 2,
         MEAN_AGE_JOB2_2014 = (AGE_2014 + AGE_START_JOB2_2014) / 2,
         MEAN_AGE_JOB3_2014 = (AGE_2014 + AGE_START_JOB3_2014) / 2,
         MEAN_AGE_JOB4_2014 = (AGE_2014 + AGE_START_JOB4_2014) / 2,
         MEAN_AGE_JOB5_2014 = (AGE_2014 + AGE_START_JOB5_2014) / 2)

# 2016
trainData79 <- trainData79 %>% 
  mutate(AGE_START_JOB1_2016 = AGE_2016 - TENURE.01_2016,
         AGE_START_JOB2_2016 = AGE_2016 - TENURE.02_2016,
         AGE_START_JOB3_2016 = AGE_2016 - TENURE.03_2016,
         AGE_START_JOB4_2016 = AGE_2016 - TENURE.04_2016,
         AGE_START_JOB5_2016 = AGE_2016 - TENURE.05_2016)

trainData79 <- trainData79 %>%
  mutate(MEAN_AGE_JOB1_2016 = (AGE_2016 + AGE_START_JOB1_2016) / 2,
         MEAN_AGE_JOB2_2016 = (AGE_2016 + AGE_START_JOB2_2016) / 2,
         MEAN_AGE_JOB3_2016 = (AGE_2016 + AGE_START_JOB3_2016) / 2,
         MEAN_AGE_JOB4_2016 = (AGE_2016 + AGE_START_JOB4_2016) / 2,
         MEAN_AGE_JOB5_2016 = (AGE_2016 + AGE_START_JOB5_2016) / 2)


# transform age back to years
ageToYears <- function(age) {
  ageYears <- age / 52
  return(ageYears)
}

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("AGE")), 
            funs(ageToYears))

trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("MEAN_AGE_JOB")), 
            funs(ageToYears))

# same for tenure, while we're at it
trainData79 <- trainData79 %>%
  mutate_at(vars(starts_with("TENURE")), 
            funs(ageToYears))


##### SELECTING RELEVANT COLUMNS #####

workingTrainData79 <- trainData79 %>%
  select(CASEID,
         SEX,
         RACE,
         RELIGION,
         starts_with("JOB_SATISFACTION"), 
         starts_with("MEAN_AGE"),
         starts_with("AGE"),
         starts_with("TENURE"),
         starts_with("HOURLY_PAY"),
         starts_with("NET_FAMILY_INCOME"),
         starts_with("ROSENBERG"),
         starts_with("ROTTER"),
         starts_with("HIGHEST_GRADE"),
         starts_with("URBAN-RURAL"),
         starts_with("UNION"),
         starts_with("PUBLIC"))

##### FACTORS #####

workingTrainData79 <- workingTrainData79 %>%
  mutate_at(vars(starts_with("HIGHEST_GRADE"),
            RELIGION,
            starts_with("URBAN-RURAL"),
            starts_with("UNION"),
            starts_with("PUBLIC")),
            funs(factor))

##### SAVING WORKING DATASET #####

# save(workingTrainData79, file = "workingTrainDataSet79.RData")


