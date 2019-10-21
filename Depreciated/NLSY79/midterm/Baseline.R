library(dplyr)
library(extrafont)
library(ggplot2)
library(stargazer)
library(MASS)


workingTrainData79_1stJob <- workingTrainData79 %>%
  dplyr::select(CASEID_R0000100,
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
  dplyr::select(JOB_SATISFACTION.01_1980, 
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
         PUBLIC.01_1980) %>%
  rename(
    jobSatisfaction = JOB_SATISFACTION.01_1980,
    age = MEAN_AGE_JOB1_1980,
    tenure = TENURE1_R0333221, 
    wage = EMPLOYERS_ALL_HRLY_WAGE_1980.01_E8070100,
    selfEsteem = ROSENBERG_IRT_SCORE_R0304420,
    locusOfControl = ROTTER_SCORE_R0153710,
    education = HIGHEST_GRADE_COMPLETED_R0017300,
    religion = `R_REL-1_COL_R0010300`,
    familyIncome = NET_FAMILY_INCOME_R0406010,
    urban = `URBAN-RURAL_R0393510`,
    union = EMPLOYERS_ALL_UNION_1980.01_E6630100,
    public = PUBLIC.01_1980
  )

just1stJob80 <- just1stJob80 %>%
  filter(!is.na(jobSatisfaction),
         familyIncome != "-Inf")

just1stJob80 <- just1stJob80 %>%
  mutate(education = case_when(education %in% c(0, 1, 2, 3, 4, 5, 6, 7) ~ "SomeLower",
                                  education %in% c(8, 9, 10, 11, 12) ~ "SomeHighSchool",
                                  education %in% c(13, 14, 15, 16) ~ "SomeCollege",
                                  education %in% c(17, 18, 19, 20) ~ "Advanced",
                                  education == 95 ~ "Other")) 
just1stJob80$education <- as.factor(just1stJob80$education)

just1stJob80 <- just1stJob80 %>%
  mutate(religion = case_when(religion %in% c(1, 2, 3, 4, 5, 6) ~ "Christian",
                              religion == 7 ~ "Catholic",
                              religion == 8 ~ "Jewish",
                              religion == 0 ~ "NonReligious",
                              religion == 9 ~ "Other")) 
just1stJob80$religion <- as.factor(just1stJob80$religion)


# models 

# https://rpubs.com/rslbliss/r_logistic_ws
# https://stats.stackexchange.com/questions/35071/what-is-rank-deficiency-and-how-to-deal-with-it

just1stJob80$jobSatisfaction <- as.factor(just1stJob80$jobSatisfaction)

model1_80 <- polr(jobSatisfaction ~ age + 
                  tenure + 
                  wage, 
                data = just1stJob80)

model1_80ctable <- coef(summary(model1_80))
model1_80or <- exp(coef(model1_80))
model1_80conf <- exp(confint(model1_80))
model1_80p <- pnorm(abs(model1_80ctable[, "t value"]), lower.tail = FALSE) * 2

model2_80 <- polr(jobSatisfaction ~ age + 
                    tenure + 
                    wage +
                    selfEsteem +
                    urban, 
                  data = just1stJob80)

model2_80ctable <- coef(summary(model2_80))
model2_80or <- exp(coef(model2_80))
model2_80conf <- exp(confint(model2_80))
model2_80p <- pnorm(abs(model2_80ctable[, "t value"]), lower.tail = FALSE) * 2


stargazer(model1_80,
          coef = list(model1_80or),
          ci = T,
          ci.custom = list(model1_80conf),
          p = c(model1_80p),
          header = FALSE,
          title = "Odd-ratios of Job Satisfaction",
          type = "latex", 
          out = "model.tex")

stargazer(model1_80,
          coef = list(model1_80or),
          ci = T,
          ci.custom = list(model1_80conf),
          p = c(model1_80p),
          header = FALSE,
          title = "Odd-ratios of Job Satisfaction",
          type = "text", 
          out = "model.doc")


# RMSE

RSS <- c(crossprod(model2_80$residuals))
MSE <- RSS / length(model2_80$residuals)
RMSE <- sqrt(MSE)


### data viz ###
plotPublic <- ggplot(aes(x = as.factor(jobSatisfaction)), 
                   data = just1stJob80) +
  geom_bar(stat = "count", aes(fill = public), color = "white") + 
  theme(text = element_text(family = "Times New Roman")) +
  labs(title = "Job Satisfaction: Public vs. Private Employees NLSY79",
       y = "Count",
       x = "Job Satisfaction: From 1 (best) to 4 (worst)",
       fill = "Public Employee")

plotPublic

plotAge <- ggplot(aes(x = age,
                      y = jobSatisfaction), 
                     data = just1stJob80) +
  geom_jitter(aes(color = age)) + 
  theme(text = element_text(family = "Times")) +
  labs(title = "Job Satisfaction: By mean age at job NLSY79",
       y = "Job Satisfaction: From 1 (best) to 4 (worst)",
       x = "Age",
       color = "Mean age at job")

plotAge

plotTenure <- ggplot(aes(x = tenure,
                      y = jobSatisfaction), 
                  data = just1stJob80) +
  geom_jitter(aes(color = tenure)) + 
  xlim(0, 200) +
  theme(text = element_text(family = "Times")) +
  labs(title = "Job Satisfaction: By tenure at job NLSY79",
       y = "Job Satisfaction: From 1 (best) to 4 (worst)",
       x = "Length of tenure in weeks",
       color = "Tenure")

plotTenure



