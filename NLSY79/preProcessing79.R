library(dplyr)

# switching variable trainData79$JOB_SATISFACTION_R4187200mes to words vs codes
varNames <- read.csv("varNames.csv", header = FALSE)

names(trainData79) <- varNames[match(names(trainData79), varNames[ ,'V1']),'V3']


# making a tiny sample for testing feature engineering 
set.seed(456) 

tinySample79 <- sample_n(trainData79, 20)


##### FEATURE ENGINEERING #####

### job satisfaction ###

# 1979
trainData79$JOB_SATISFACTION.01_1979 <- trainData79$JOB_SATISFACTION_R0050800
trainData79$JOB_SATISFACTION.02_1979 <- trainData79$JOB_SATISFACTION_R0050800
trainData79$JOB_SATISFACTION.03_1979 <- trainData79$JOB_SATISFACTION_R0050800
trainData79$JOB_SATISFACTION.04_1979 <- trainData79$JOB_SATISFACTION_R0050800
trainData79$JOB_SATISFACTION.05_1979 <- trainData79$JOB_SATISFACTION_R0050800

# 1980
trainData79$JOB_SATISFACTION.01_1980 <- trainData79$JOB_SATISFACTION_R0267800
trainData79$JOB_SATISFACTION.02_1980 <- trainData79$JOB_SATISFACTION_R0267800
trainData79$JOB_SATISFACTION.03_1980 <- trainData79$JOB_SATISFACTION_R0267800
trainData79$JOB_SATISFACTION.04_1980 <- trainData79$JOB_SATISFACTION_R0267800
trainData79$JOB_SATISFACTION.05_1980 <- trainData79$JOB_SATISFACTION_R0267800

# 1981
trainData79$JOB_SATISFACTION.01_1981 <- trainData79$JOB_SATISFACTION_R0449200
trainData79$JOB_SATISFACTION.02_1981 <- trainData79$JOB_SATISFACTION_R0449200
trainData79$JOB_SATISFACTION.03_1981 <- trainData79$JOB_SATISFACTION_R0449200
trainData79$JOB_SATISFACTION.04_1981 <- trainData79$JOB_SATISFACTION_R0449200
trainData79$JOB_SATISFACTION.05_1981 <- trainData79$JOB_SATISFACTION_R0449200

# 1982
trainData79$JOB_SATISFACTION.01_1982 <- trainData79$JOB_SATISFACTION_R0706500
trainData79$JOB_SATISFACTION.02_1982 <- trainData79$JOB_SATISFACTION_R0706500
trainData79$JOB_SATISFACTION.03_1982 <- trainData79$JOB_SATISFACTION_R0706500
trainData79$JOB_SATISFACTION.04_1982 <- trainData79$JOB_SATISFACTION_R0706500
trainData79$JOB_SATISFACTION.05_1982 <- trainData79$JOB_SATISFACTION_R0706500

# 1983
trainData79$JOB_SATISFACTION.01_1983 <- trainData79$JOB_SATISFACTION_R0946500
trainData79$JOB_SATISFACTION.02_1983 <- trainData79$JOB_SATISFACTION_R0946500
trainData79$JOB_SATISFACTION.03_1983 <- trainData79$JOB_SATISFACTION_R0946500
trainData79$JOB_SATISFACTION.04_1983 <- trainData79$JOB_SATISFACTION_R0946500
trainData79$JOB_SATISFACTION.05_1983 <- trainData79$JOB_SATISFACTION_R0946500

# 1984
trainData79$JOB_SATISFACTION.01_1984 <- trainData79$JOB_SATISFACTION_R1256900
trainData79$JOB_SATISFACTION.02_1984 <- trainData79$JOB_SATISFACTION_R1256900
trainData79$JOB_SATISFACTION.03_1984 <- trainData79$JOB_SATISFACTION_R1256900
trainData79$JOB_SATISFACTION.04_1984 <- trainData79$JOB_SATISFACTION_R1256900
trainData79$JOB_SATISFACTION.05_1984 <- trainData79$JOB_SATISFACTION_R1256900

# 1985
trainData79$JOB_SATISFACTION.01_1985 <- trainData79$JOB_SATISFACTION_R1652000
trainData79$JOB_SATISFACTION.02_1985 <- trainData79$JOB_SATISFACTION_R1652000
trainData79$JOB_SATISFACTION.03_1985 <- trainData79$JOB_SATISFACTION_R1652000
trainData79$JOB_SATISFACTION.04_1985 <- trainData79$JOB_SATISFACTION_R1652000
trainData79$JOB_SATISFACTION.05_1985 <- trainData79$JOB_SATISFACTION_R1652000

# 1986
trainData79$JOB_SATISFACTION.01_1986 <- trainData79$JOB_SATISFACTION_R1925100
trainData79$JOB_SATISFACTION.02_1986 <- trainData79$JOB_SATISFACTION_R1925100
trainData79$JOB_SATISFACTION.03_1986 <- trainData79$JOB_SATISFACTION_R1925100
trainData79$JOB_SATISFACTION.04_1986 <- trainData79$JOB_SATISFACTION_R1925100
trainData79$JOB_SATISFACTION.05_1986 <- trainData79$JOB_SATISFACTION_R1925100

# 1987
trainData79$JOB_SATISFACTION.01_1987 <- trainData79$JOB_SATISFACTION_R2319900
trainData79$JOB_SATISFACTION.02_1987 <- trainData79$JOB_SATISFACTION_R2319900
trainData79$JOB_SATISFACTION.03_1987 <- trainData79$JOB_SATISFACTION_R2319900
trainData79$JOB_SATISFACTION.04_1987 <- trainData79$JOB_SATISFACTION_R2319900
trainData79$JOB_SATISFACTION.05_1987 <- trainData79$JOB_SATISFACTION_R2319900

# 1988
trainData79$JOB_SATISFACTION.01_1988 <- trainData79$JOB_SATISFACTION_R2532900
trainData79$JOB_SATISFACTION.02_1988 <- trainData79$JOB_SATISFACTION_R2532900
trainData79$JOB_SATISFACTION.03_1988 <- trainData79$JOB_SATISFACTION_R2532900
trainData79$JOB_SATISFACTION.04_1988 <- trainData79$JOB_SATISFACTION_R2532900
trainData79$JOB_SATISFACTION.05_1988 <- trainData79$JOB_SATISFACTION_R2532900

# 1989
trainData79$JOB_SATISFACTION.01_1989 <- trainData79$JOB_SATISFACTION_R2930500
trainData79$JOB_SATISFACTION.02_1989 <- trainData79$JOB_SATISFACTION_R2930500
trainData79$JOB_SATISFACTION.03_1989 <- trainData79$JOB_SATISFACTION_R2930500
trainData79$JOB_SATISFACTION.04_1989 <- trainData79$JOB_SATISFACTION_R2930500
trainData79$JOB_SATISFACTION.05_1989 <- trainData79$JOB_SATISFACTION_R2930500

# 1990
trainData79$JOB_SATISFACTION.01_1990 <- trainData79$JOB_SATISFACTION_R3136600
trainData79$JOB_SATISFACTION.02_1990 <- trainData79$JOB_SATISFACTION_R3136600
trainData79$JOB_SATISFACTION.03_1990 <- trainData79$JOB_SATISFACTION_R3136600
trainData79$JOB_SATISFACTION.04_1990 <- trainData79$JOB_SATISFACTION_R3136600
trainData79$JOB_SATISFACTION.05_1990 <- trainData79$JOB_SATISFACTION_R3136600

# 1991
trainData79$JOB_SATISFACTION.01_1991 <- trainData79$JOB_SATISFACTION_R3527000
trainData79$JOB_SATISFACTION.02_1991 <- trainData79$JOB_SATISFACTION_R3527000
trainData79$JOB_SATISFACTION.03_1991 <- trainData79$JOB_SATISFACTION_R3527000
trainData79$JOB_SATISFACTION.04_1991 <- trainData79$JOB_SATISFACTION_R3527000
trainData79$JOB_SATISFACTION.05_1991 <- trainData79$JOB_SATISFACTION_R3527000

# 1992
trainData79$JOB_SATISFACTION.01_1992 <- trainData79$JOB_SATISFACTION_R3732100
trainData79$JOB_SATISFACTION.02_1992 <- trainData79$JOB_SATISFACTION_R3732100
trainData79$JOB_SATISFACTION.03_1992 <- trainData79$JOB_SATISFACTION_R3732100
trainData79$JOB_SATISFACTION.04_1992 <- trainData79$JOB_SATISFACTION_R3732100
trainData79$JOB_SATISFACTION.05_1992 <- trainData79$JOB_SATISFACTION_R3732100

# 1993
trainData79$JOB_SATISFACTION.01_1993 <- trainData79$JOB_SATISFACTION_R4187200
trainData79$JOB_SATISFACTION.02_1993 <- trainData79$JOB_SATISFACTION_R4187200
trainData79$JOB_SATISFACTION.03_1993 <- trainData79$JOB_SATISFACTION_R4187200
trainData79$JOB_SATISFACTION.04_1993 <- trainData79$JOB_SATISFACTION_R4187200
trainData79$JOB_SATISFACTION.05_1993 <- trainData79$JOB_SATISFACTION_R4187200


### sector ###
# 1979
trainData79 <- trainData79 %>%
  mutate(publicPrivate.01_1979 = case_when(EMPLOYERS_ALL_COW_1979.01_E7820100 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1979.01_E7820100 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.02_1979 = case_when(EMPLOYERS_ALL_COW_1979.02_E7820200 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1979.02_E7820200 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.03_1979 = case_when(EMPLOYERS_ALL_COW_1979.03_E7820300 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1979.03_E7820300 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.04_1979 = case_when(EMPLOYERS_ALL_COW_1979.04_E7820400 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1979.04_E7820400 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.05_1979 = case_when(EMPLOYERS_ALL_COW_1979.05_E7820500 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1979.05_E7820500 == 2 ~ "Public"))

# 1980
trainData79 <- trainData79 %>%
  mutate(publicPrivate.01_1980 = case_when(EMPLOYERS_ALL_COW_1980.01_E7830100 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1980.01_E7830100 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.02_1980 = case_when(EMPLOYERS_ALL_COW_1980.02_E7830200 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1980.02_E7830200 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.03_1980 = case_when(EMPLOYERS_ALL_COW_1980.03_E7830300 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1980.03_E7830300 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.04_1980 = case_when(EMPLOYERS_ALL_COW_1980.04_E7830400 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1980.04_E7830400 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.05_1980 = case_when(EMPLOYERS_ALL_COW_1980.05_E7830500 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1980.05_E7830500 == 2 ~ "Public"))

# 1981
trainData79 <- trainData79 %>%
  mutate(publicPrivate.01_1981 = case_when(EMPLOYERS_ALL_COW_1981.01_E7840100 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1981.01_E7840100 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.02_1981 = case_when(EMPLOYERS_ALL_COW_1981.02_E7840200 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1981.02_E7840200 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.03_1981 = case_when(EMPLOYERS_ALL_COW_1981.03_E7840300 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1981.03_E7840300 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.04_1981 = case_when(EMPLOYERS_ALL_COW_1981.04_E7840400 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1981.04_E7840400 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.05_1981 = case_when(EMPLOYERS_ALL_COW_1981.05_E7840500 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1981.05_E7840500 == 2 ~ "Public"))

# 1982
trainData79 <- trainData79 %>%
  mutate(publicPrivate.01_1982 = case_when(EMPLOYERS_ALL_COW_1982.01_E7850100 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1982.01_E7850100 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.02_1982 = case_when(EMPLOYERS_ALL_COW_1982.02_E7850200 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1982.02_E7850200 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.03_1982 = case_when(EMPLOYERS_ALL_COW_1982.03_E7850300 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1982.03_E7850300 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.04_1982 = case_when(EMPLOYERS_ALL_COW_1982.04_E7850400 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1982.04_E7850400 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.05_1982 = case_when(EMPLOYERS_ALL_COW_1982.05_E7850500 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1982.05_E7850500 == 2 ~ "Public"))

# 1983
trainData79 <- trainData79 %>%
  mutate(publicPrivate.01_1983 = case_when(EMPLOYERS_ALL_COW_1983.01_E7860100 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1983.01_E7860100 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.02_1983 = case_when(EMPLOYERS_ALL_COW_1983.02_E7860200 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1983.02_E7860200 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.03_1983 = case_when(EMPLOYERS_ALL_COW_1983.03_E7860300  %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1983.03_E7860300  == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.04_1983 = case_when(EMPLOYERS_ALL_COW_1983.04_E7860400 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1983.04_E7860400 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.05_1983 = case_when(EMPLOYERS_ALL_COW_1983.05_E7860500 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1983.05_E7860500 == 2 ~ "Public"))


# 1984
trainData79 <- trainData79 %>%
  mutate(publicPrivate.01_1984 = case_when(EMPLOYERS_ALL_COW_1984.01_E7870100 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1984.01_E7870100 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.02_1984 = case_when(EMPLOYERS_ALL_COW_1984.02_E7870200 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1984.02_E7870200 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.03_1984 = case_when(EMPLOYERS_ALL_COW_1984.03_E7870300  %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1984.03_E7870300  == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.04_1984 = case_when(EMPLOYERS_ALL_COW_1984.04_E7870400 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1984.04_E7870400 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.05_1984 = case_when(EMPLOYERS_ALL_COW_1984.05_E7870500 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1984.05_E7870500 == 2 ~ "Public"))

# 1985
trainData79 <- trainData79 %>%
  mutate(publicPrivate.01_1985 = case_when(EMPLOYERS_ALL_COW_1985.01_E7880100 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1985.01_E7880100 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.02_1985 = case_when(EMPLOYERS_ALL_COW_1985.02_E7880200 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1985.02_E7880200 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.03_1985 = case_when(EMPLOYERS_ALL_COW_1985.03_E7880300  %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1985.03_E7880300  == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.04_1985 = case_when(EMPLOYERS_ALL_COW_1985.04_E7880400 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1985.04_E7880400 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.05_1985 = case_when(EMPLOYERS_ALL_COW_1985.05_E7880500 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1985.05_E7880500 == 2 ~ "Public"))

# 1986
trainData79 <- trainData79 %>%
  mutate(publicPrivate.01_1986 = case_when(EMPLOYERS_ALL_COW_1986.01_E7890100 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1986.01_E7890100 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.02_1986 = case_when(EMPLOYERS_ALL_COW_1986.02_E7890200 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1986.02_E7890200 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.03_1986 = case_when(EMPLOYERS_ALL_COW_1986.03_E7890300   %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1986.03_E7890300   == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.04_1986 = case_when(EMPLOYERS_ALL_COW_1986.04_E7890400  %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1986.04_E7890400  == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.05_1986 = case_when(EMPLOYERS_ALL_COW_1986.05_E7890500  %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1986.05_E7890500  == 2 ~ "Public"))

# 1987
trainData79 <- trainData79 %>%
  mutate(publicPrivate.01_1987 = case_when(EMPLOYERS_ALL_COW_1987.01_E7900100 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1987.01_E7900100 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.02_1987 = case_when(EMPLOYERS_ALL_COW_1987.02_E7900200 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1987.02_E7900200 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.03_1987 = case_when(EMPLOYERS_ALL_COW_1987.03_E7900300   %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1987.03_E7900300   == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.04_1987 = case_when(EMPLOYERS_ALL_COW_1987.04_E7900400  %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1987.04_E7900400  == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.05_1987 = case_when(EMPLOYERS_ALL_COW_1987.05_E7900500  %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1987.05_E7900500  == 2 ~ "Public"))

# 1988
trainData79 <- trainData79 %>%
  mutate(publicPrivate.01_1988 = case_when(EMPLOYERS_ALL_COW_1988.01_E7910100 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1988.01_E7910100 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.02_1988 = case_when(EMPLOYERS_ALL_COW_1988.02_E7910200 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1988.02_E7910200 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.03_1988 = case_when(EMPLOYERS_ALL_COW_1988.03_E7910300   %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1988.03_E7910300   == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.04_1988 = case_when(EMPLOYERS_ALL_COW_1988.04_E7910400  %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1988.04_E7910400  == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.05_1988 = case_when(EMPLOYERS_ALL_COW_1988.05_E7910500  %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1988.05_E7910500  == 2 ~ "Public"))

# 1989
trainData79 <- trainData79 %>%
  mutate(publicPrivate.01_1989 = case_when(EMPLOYERS_ALL_COW_1989.01_E7920100 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1989.01_E7920100 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.02_1989 = case_when(EMPLOYERS_ALL_COW_1989.02_E7920200 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1989.02_E7920200 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.03_1989 = case_when(EMPLOYERS_ALL_COW_1989.03_E7920300   %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1989.03_E7920300   == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.04_1989 = case_when(EMPLOYERS_ALL_COW_1989.04_E7920400  %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1989.04_E7920400  == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.05_1989 = case_when(EMPLOYERS_ALL_COW_1989.05_E7920500  %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1989.05_E7920500  == 2 ~ "Public"))

# 1990
trainData79 <- trainData79 %>%
  mutate(publicPrivate.01_1990 = case_when(EMPLOYERS_ALL_COW_1990.01_E7930100 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1990.01_E7930100 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.02_1990 = case_when(EMPLOYERS_ALL_COW_1990.02_E7930200 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1990.02_E7930200 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.03_1990 = case_when(EMPLOYERS_ALL_COW_1990.03_E7930300   %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1990.03_E7930300   == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.04_1990 = case_when(EMPLOYERS_ALL_COW_1990.04_E7930400  %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1990.04_E7930400  == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.05_1990 = case_when(EMPLOYERS_ALL_COW_1990.05_E7930500  %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1990.05_E7930500  == 2 ~ "Public"))

# 1991
trainData79 <- trainData79 %>%
  mutate(publicPrivate.01_1991 = case_when(EMPLOYERS_ALL_COW_1991.01_E7940100 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1991.01_E7940100 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.02_1991 = case_when(EMPLOYERS_ALL_COW_1991.02_E7940200 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1991.02_E7940200 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.03_1991 = case_when(EMPLOYERS_ALL_COW_1991.03_E7940300   %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1991.03_E7940300   == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.04_1991 = case_when(EMPLOYERS_ALL_COW_1991.04_E7940400  %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1991.04_E7940400  == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.05_1991 = case_when(EMPLOYERS_ALL_COW_1991.05_E7940500  %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1991.05_E7940500  == 2 ~ "Public"))

# 1992
trainData79 <- trainData79 %>%
  mutate(publicPrivate.01_1992 = case_when(EMPLOYERS_ALL_COW_1992.01_E7950100 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1992.01_E7950100 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.02_1992 = case_when(EMPLOYERS_ALL_COW_1992.02_E7950200 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1992.02_E7950200 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.03_1992 = case_when(EMPLOYERS_ALL_COW_1992.03_E7950300   %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1992.03_E7950300   == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.04_1992 = case_when(EMPLOYERS_ALL_COW_1992.04_E7950400  %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1992.04_E7950400  == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.05_1992 = case_when(EMPLOYERS_ALL_COW_1992.05_E7950500  %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1992.05_E7950500  == 2 ~ "Public"))     
       
# 1993
trainData79 <- trainData79 %>%
  mutate(publicPrivate.01_1993 = case_when(EMPLOYERS_ALL_COW_1993.01_E7960100 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1993.01_E7960100 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.02_1993 = case_when(EMPLOYERS_ALL_COW_1993.02_E7960200 %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1993.02_E7960200 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.03_1993 = case_when(EMPLOYERS_ALL_COW_1993.03_E7960300   %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1993.03_E7960300   == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.04_1993 = case_when(EMPLOYERS_ALL_COW_1993.04_E7960400  %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1993.04_E7960400  == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.05_1993 = case_when(EMPLOYERS_ALL_COW_1993.05_E7960500  %in% c(1,3,4) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1993.05_E7960500  == 2 ~ "Public"))       
# 1994
trainData79 <- trainData79 %>%
  mutate(publicPrivate.01_1994 = case_when(EMPLOYERS_ALL_COW_1994.01_E7970100 %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1994.01_E7970100 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.02_1994 = case_when(EMPLOYERS_ALL_COW_1994.02_E7970200 %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1994.02_E7970200 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.03_1994 = case_when(EMPLOYERS_ALL_COW_1994.03_E7970300   %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1994.03_E7970300   == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.04_1994 = case_when(EMPLOYERS_ALL_COW_1994.04_E7970400  %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1994.04_E7970400  == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.05_1994 = case_when(EMPLOYERS_ALL_COW_1994.05_E7970500  %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1994.05_E7970500  == 2 ~ "Public"))
# 1996
trainData79 <- trainData79 %>%
  mutate(publicPrivate.01_1996 = case_when(EMPLOYERS_ALL_COW_1996.01_E7980100 %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1996.01_E7980100 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.02_1996 = case_when(EMPLOYERS_ALL_COW_1996.02_E7980200 %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1996.02_E7980200 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.03_1996 = case_when(EMPLOYERS_ALL_COW_1996.03_E7980300   %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1996.03_E7980300   == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.04_1996 = case_when(EMPLOYERS_ALL_COW_1996.04_E7980400  %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1996.04_E7980400  == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.05_1996 = case_when(EMPLOYERS_ALL_COW_1996.05_E7980500  %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1996.05_E7980500  == 2 ~ "Public"))

# 1998
trainData79 <- trainData79 %>%
  mutate(publicPrivate.01_1998 = case_when(EMPLOYERS_ALL_COW_1998.01_E7990100 %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1998.01_E7990100 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.02_1998 = case_when(EMPLOYERS_ALL_COW_1998.02_E7990200 %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1998.02_E7990200 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.03_1998 = case_when(EMPLOYERS_ALL_COW_1998.03_E7990300   %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1998.03_E7990300   == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.04_1998 = case_when(EMPLOYERS_ALL_COW_1998.04_E7990400  %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1998.04_E7990400  == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.05_1998 = case_when(EMPLOYERS_ALL_COW_1998.05_E7990500  %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_1998.05_E7990500  == 2 ~ "Public"))
 
# 2000
trainData79 <- trainData79 %>%
  mutate(publicPrivate.01_2000 = case_when(EMPLOYERS_ALL_COW_2000.01_E8000100 %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2000.01_E8000100 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.02_2000 = case_when(EMPLOYERS_ALL_COW_2000.02_E8000200 %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2000.02_E8000200 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.03_2000 = case_when(EMPLOYERS_ALL_COW_2000.03_E8000300   %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2000.03_E8000300   == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.04_2000 = case_when(EMPLOYERS_ALL_COW_2000.04_E8000400  %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2000.04_E8000400  == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.05_2000 = case_when(EMPLOYERS_ALL_COW_2000.05_E8000500  %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2000.05_E8000500  == 2 ~ "Public"))

# 2002
trainData79 <- trainData79 %>%
  mutate(publicPrivate.01_2002 = case_when(EMPLOYERS_ALL_COW_2002.01_E8010100 %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2002.01_E8010100 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.02_2002 = case_when(EMPLOYERS_ALL_COW_2002.02_E8010200 %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2002.02_E8010200 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.03_2002 = case_when(EMPLOYERS_ALL_COW_2002.03_E8010300   %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2002.03_E8010300   == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.04_2002 = case_when(EMPLOYERS_ALL_COW_2002.04_E8010400  %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2002.04_E8010400  == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.05_2002 = case_when(EMPLOYERS_ALL_COW_2002.05_E8010500  %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2002.05_E8010500  == 2 ~ "Public"))

# 2004
trainData79 <- trainData79 %>%
  mutate(publicPrivate.01_2004 = case_when(EMPLOYERS_ALL_COW_2004.01_E8020100 %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2004.01_E8020100 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.02_2004 = case_when(EMPLOYERS_ALL_COW_2004.02_E8020200 %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2004.02_E8020200 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.03_2004 = case_when(EMPLOYERS_ALL_COW_2004.03_E8020300   %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2004.03_E8020300   == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.04_2004 = case_when(EMPLOYERS_ALL_COW_2004.04_E8020400  %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2004.04_E8020400  == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.05_2004 = case_when(EMPLOYERS_ALL_COW_2004.05_E8020500  %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2004.05_E8020500  == 2 ~ "Public"))

# 2006
trainData79 <- trainData79 %>%
  mutate(publicPrivate.01_2006 = case_when(EMPLOYERS_ALL_COW_2006.01_E8030100 %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2006.01_E8030100 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.02_2006 = case_when(EMPLOYERS_ALL_COW_2006.02_E8030200 %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2006.02_E8030200 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.03_2006 = case_when(EMPLOYERS_ALL_COW_2006.03_E8030300   %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2006.03_E8030300   == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.04_2006 = case_when(EMPLOYERS_ALL_COW_2006.04_E8030400  %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2006.04_E8030400  == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.05_2006 = case_when(EMPLOYERS_ALL_COW_2006.05_E8030500  %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2006.05_E8030500  == 2 ~ "Public"))

# 2008
trainData79 <- trainData79 %>%
  mutate(publicPrivate.01_2008 = case_when(EMPLOYERS_ALL_COW_2008.01_E8040100 %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2008.01_E8040100 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.02_2008 = case_when(EMPLOYERS_ALL_COW_2008.02_E8040200 %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2008.02_E8040200 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.03_2008 = case_when(EMPLOYERS_ALL_COW_2008.03_E8040300   %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2008.03_E8040300   == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.04_2008 = case_when(EMPLOYERS_ALL_COW_2008.04_E8040400  %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2008.04_E8040400  == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.05_2008 = case_when(EMPLOYERS_ALL_COW_2008.05_E8040500  %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2008.05_E8040500  == 2 ~ "Public"))

# 2010
trainData79 <- trainData79 %>%
  mutate(publicPrivate.01_2010 = case_when(EMPLOYERS_ALL_COW_2010.01_E8050100 %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2010.01_E8050100 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.02_2010 = case_when(EMPLOYERS_ALL_COW_2010.02_E8050200 %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2010.02_E8050200 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.03_2010 = case_when(EMPLOYERS_ALL_COW_2010.03_E8050300   %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2010.03_E8050300   == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.04_2010 = case_when(EMPLOYERS_ALL_COW_2010.04_E8050400  %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2010.04_E8050400  == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.05_2010 = case_when(EMPLOYERS_ALL_COW_2010.05_E8050500  %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2010.05_E8050500  == 2 ~ "Public"))

# 2012
trainData79 <- trainData79 %>%
  mutate(publicPrivate.01_2012 = case_when(EMPLOYERS_ALL_COW_2012.01_E8670100 %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2012.01_E8670100 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.02_2012 = case_when(EMPLOYERS_ALL_COW_2012.02_E8670200 %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2012.02_E8670200 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.03_2012 = case_when(EMPLOYERS_ALL_COW_2012.03_E8670300   %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2012.03_E8670300   == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.04_2012 = case_when(EMPLOYERS_ALL_COW_2012.04_E8670400  %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2012.04_E8670400  == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.05_2012 = case_when(EMPLOYERS_ALL_COW_2012.05_E8670500  %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2012.05_E8670500  == 2 ~ "Public"))

# 2014
trainData79 <- trainData79 %>%
  mutate(publicPrivate.01_2014 = case_when(EMPLOYERS_ALL_COW_2014.01_X0220200 %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2014.01_X0220200 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.02_2014 = case_when(EMPLOYERS_ALL_COW_2014.02_X0220300 %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2014.02_X0220300 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.03_2014 = case_when(EMPLOYERS_ALL_COW_2014.03_X0220400   %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2014.03_X0220400   == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.04_2014 = case_when(EMPLOYERS_ALL_COW_2014.04_X0220500  %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2014.04_X0220500  == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.05_2014 = case_when(EMPLOYERS_ALL_COW_2014.05_X0220600  %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2014.05_X0220600  == 2 ~ "Public"))

# 2016
trainData79 <- trainData79 %>%
  mutate(publicPrivate.01_2016 = case_when(EMPLOYERS_ALL_COW_2016.01_E8696500 %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2016.01_E8696500 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.02_2016 = case_when(EMPLOYERS_ALL_COW_2016.02_E8696600 %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2016.02_E8696600 == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.03_2016 = case_when(EMPLOYERS_ALL_COW_2016.03_E8696700   %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2016.03_E8696700   == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.04_2016 = case_when(EMPLOYERS_ALL_COW_2016.04_E8696800  %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2016.04_E8696800  == 2 ~ "Public"))

trainData79 <- trainData79 %>%
  mutate(publicPrivate.05_2016 = case_when(EMPLOYERS_ALL_COW_2016.05_E8696900  %in% c(1,3,4,5) ~ "Private", 
                                           EMPLOYERS_ALL_COW_2016.05_E8696900  == 2 ~ "Public"))

### Religion raised in ###
