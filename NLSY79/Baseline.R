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

model1 <- lm(JOB_SATISFACTION.01_1980 ~ MEAN_AGE_JOB1_1980 + 
               TENURE1_R0333221 + 
               EMPLOYERS_ALL_HRLY_WAGE_1980.01_E8070100, 
             data = just1stJob80)

summary(model1)

model2 <- lm(JOB_SATISFACTION.01_1980 ~ ., data = just1stJob80)

summary(model2)



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



