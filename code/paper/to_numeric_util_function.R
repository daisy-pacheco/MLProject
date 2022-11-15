f_test <- final_test_data %>% 
  mutate(male = as.numeric(as.character(male)),
         white = as.numeric(as.character(white)),
         religious = as.numeric(as.character(religious)),
         urban = as.numeric(as.character(urban)),
         cohort = as.numeric(as.character(cohort)))


write.csv(f_train, "num_train_data.csv")
