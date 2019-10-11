library(dplyr)
library(ggplot2)
library(ggthemes)

workingTrainData79_1stJob <- workingTrainData79 %>%
  select(CASEID_R0000100,
         starts_with("JOB_SATISFACTION.01"),
         starts_with("MEAN_AGE_JOB1"), 
         starts_with("TENURE1"), 
         starts_with("EMPLOYERS_ALL_HRLY_WAGE_1979.01"), 
         starts_with("EMPLOYERS_ALL_HRLY_WAGE_1980.01"),
         starts_with("EMPLOYERS_ALL_HRLY_WAGE_1981.01"),
         starts_with("EMPLOYERS_ALL_HRLY_WAGE_1982.01"), 
         starts_with("EMPLOYERS_ALL_HRLY_WAGE_1983.01"), 
         starts_with("EMPLOYERS_ALL_HRLY_WAGE_1984.01"), 
         starts_with("EMPLOYERS_ALL_HRLY_WAGE_1985.01"), 
         starts_with("EMPLOYERS_ALL_HRLY_WAGE_1986.01"), 
         starts_with("EMPLOYERS_ALL_HRLY_WAGE_1987.01"), 
         starts_with("EMPLOYERS_ALL_HRLY_WAGE_1988.01"), 
         starts_with("EMPLOYERS_ALL_HRLY_WAGE_1989.01"), 
         starts_with("EMPLOYERS_ALL_HRLY_WAGE_1990.01"),
         starts_with("EMPLOYERS_ALL_HRLY_WAGE_1991.01"), 
         starts_with("EMPLOYERS_ALL_HRLY_WAGE_1992.01"),
         starts_with("EMPLOYERS_ALL_HRLY_WAGE_1994.01"),
         starts_with("EMPLOYERS_ALL_HRLY_WAGE_1996.01"),
         starts_with("EMPLOYERS_ALL_HRLY_WAGE_1998.01"),
         starts_with("EMPLOYERS_ALL_HRLY_WAGE_2000.01"),
         starts_with("EMPLOYERS_ALL_HRLY_WAGE_2002.01"),
         starts_with("EMPLOYERS_ALL_HRLY_WAGE_2004.01"),
         starts_with("EMPLOYERS_ALL_HRLY_WAGE_2006.01"),
         starts_with("EMPLOYERS_ALL_HRLY_WAGE_2008.01"),
         starts_with("EMPLOYERS_ALL_HRLY_WAGE_2010.01"),
         starts_with("EMPLOYERS_ALL_HRLY_WAGE_2012.01"),
         starts_with("EMPLOYERS_ALL_HRLY_WAGE_2014.01"),
         starts_with("EMPLOYERS_ALL_HRLY_WAGE_2016.01"),
         starts_with("ROSENBERG"),
         starts_with("ROTTER"),
         starts_with("HIGHEST_GRADE_COMPLETED"),
         `R_REL-1_COL_R0010300`, 
         starts_with("NET_FAMILY_INCOME"),
         starts_with("URBAN-RURAL"),
         starts_with("EMPLOYERS_ALL_UNION_1979.01"),
         starts_with("EMPLOYERS_ALL_UNION_1980.01"),
         starts_with("EMPLOYERS_ALL_UNION_1981.01"),
         starts_with("EMPLOYERS_ALL_UNION_1982.01"),
         starts_with("EMPLOYERS_ALL_UNION_1983.01"),
         starts_with("EMPLOYERS_ALL_UNION_1984.01"),
         starts_with("EMPLOYERS_ALL_UNION_1985.01"),
         starts_with("EMPLOYERS_ALL_UNION_1986.01"),
         starts_with("EMPLOYERS_ALL_UNION_1987.01"),
         starts_with("EMPLOYERS_ALL_UNION_1988.01"),
         starts_with("EMPLOYERS_ALL_UNION_1989.01"),
         starts_with("EMPLOYERS_ALL_UNION_1990.01"),
         starts_with("EMPLOYERS_ALL_UNION_1991.01"),
         starts_with("EMPLOYERS_ALL_UNION_1992.01"),
         starts_with("EMPLOYERS_ALL_UNION_1993.01"),
         starts_with("EMPLOYERS_ALL_UNION_1994.01"),
         starts_with("EMPLOYERS_ALL_UNION_1996.01"),
         starts_with("EMPLOYERS_ALL_UNION_1998.01"),
         starts_with("EMPLOYERS_ALL_UNION_2000.01"),
         starts_with("EMPLOYERS_ALL_UNION_2002.01"),
         starts_with("EMPLOYERS_ALL_UNION_2004.01"),
         starts_with("EMPLOYERS_ALL_UNION_2006.01"),
         starts_with("EMPLOYERS_ALL_UNION_2008.01"),
         starts_with("EMPLOYERS_ALL_UNION_2010.01"),
         starts_with("EMPLOYERS_ALL_UNION_2012.01"),
         starts_with("EMPLOYERS_ALL_UNION_2014.01"),
         starts_with("EMPLOYERS_ALL_UNION_2016.01"),
         starts_with("PUBLIC.01"))

### baseline models ###

# 1980
just1stJob80 <- workingTrainData79_1stJob %>%
  select(JOB_SATISFACTION.01_1980, 
         MEAN_AGE_JOB1_1980, 
         TENURE1_R0333221,
         EMPLOYERS_ALL_HRLY_WAGE_1980.01_E8070100,
         ROSENBERG_IRT_SCORE_R0304420,
         ROTTER_SCORE_R0153710,
         HIGHEST_GRADE_COMPLETED_R0017300,
         `R_REL-1_COL_R0010300`,
         NET_FAMILY_INCOME_R0406010,
         `URBAN-RURAL_R0393510`,
         EMPLOYERS_ALL_UNION_1980.01_E6630100,
         PUBLIC.01_1980)

just1stJob80 <- just1stJob80 %>%
  filter(!is.na(JOB_SATISFACTION.01_1980),
         NET_FAMILY_INCOME_R0406010 != "-Inf")

model1_80 <- lm(JOB_SATISFACTION.01_1980 ~ MEAN_AGE_JOB1_1980 + 
               TENURE1_R0333221 + 
               EMPLOYERS_ALL_HRLY_WAGE_1980.01_E8070100, 
             data = just1stJob80)

summary(model1_80)

model2_80 <- lm(JOB_SATISFACTION.01_1980 ~ ., data = just1stJob80)

summary(model2_80)

# 1987

just1stJob87 <- workingTrainData79_1stJob %>%
  select(JOB_SATISFACTION.01_1987, 
         MEAN_AGE_JOB1_1987, 
         TENURE1_R2372510,
         EMPLOYERS_ALL_HRLY_WAGE_1987.01_E8140100,
         ROSENBERG_IRT_SCORE_R2350020,
         ROTTER_SCORE_R0153710, # 1979
         HIGHEST_GRADE_COMPLETED_R2306500,
         `R_REL-1_COL_R0010300`,
         NET_FAMILY_INCOME_R2444700,
         `URBAN-RURAL_R2446900`,
         EMPLOYERS_ALL_UNION_1987.01_E6700100,
         PUBLIC.01_1987)

just1stJob87 <- just1stJob87 %>%
  filter(!is.na(JOB_SATISFACTION.01_1987))

model1_87 <- lm(JOB_SATISFACTION.01_1987 ~ MEAN_AGE_JOB1_1987 + 
                  TENURE1_R2372510 + 
                  EMPLOYERS_ALL_HRLY_WAGE_1987.01_E8140100, 
                data = just1stJob87)

summary(model1_87)

model2_87 <- lm(JOB_SATISFACTION.01_1987 ~ ., data = just1stJob87)

summary(model2_87)

# 2006

just1stJob06 <- workingTrainData79_1stJob %>%
  select(JOB_SATISFACTION.01_T0277600, 
         MEAN_AGE_JOB1_2006, 
         TENURE1_T0986300,
         EMPLOYERS_ALL_HRLY_WAGE_2006.01_E8270100,
         ROSENBERG_IRT_SCORE_T0899820,
         ROTTER_SCORE_R0153710, # 1979
         HIGHEST_GRADE_COMPLETED_T0014400,
         `R_REL-1_COL_R0010300`,
         NET_FAMILY_INCOME_T0987800,
         `URBAN-RURAL_T0990400`,
         EMPLOYERS_ALL_UNION_2006.01_E6830100,
         PUBLIC.01_2006)

just1stJob06 <- just1stJob06 %>%
  filter(!is.na(JOB_SATISFACTION.01_T0277600))

model1_06 <- lm(JOB_SATISFACTION.01_T0277600 ~ MEAN_AGE_JOB1_2006 + 
                  TENURE1_T0986300 + 
                  EMPLOYERS_ALL_HRLY_WAGE_2006.01_E8270100, 
                data = just1stJob06)

summary(model1_06)

model2_06 <- lm(JOB_SATISFACTION.01_T0277600 ~ ., data = just1stJob06)

summary(model2_06)

### data viz ###
plot1980 <- ggplot(aes(x = as.factor(JOB_SATISFACTION.01_1980)), 
                   data = just1stJob80) +
  geom_bar(stat = "count", aes(fill = PUBLIC.01_1980), color = "white") + 
  theme(text = element_text(family = "Times")) +
  labs(title = "Job Satisfaction: Public vs. Private Employees NLSY79",
       y = "Count",
       x = "Job Satisfaction: From 1 (best) to 4 (worst)",
       fill = "Public Employee")

plot1980



