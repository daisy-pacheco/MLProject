library(dplyr)

# switching variable names to words vs codes
varNames <- read.csv("varNames97.csv", header = FALSE)

names(trainData97) <- varNames[match(names(trainData97), varNames[ ,'V1']),'V3']

# making a tiny sample for testing feature engineering 
set.seed(456) 

tinySample97 <- sample_n(trainData97, size = 20)


##### FEATURE ENGINEERING #####

### sector ###

# 1997
trainData97 <- trainData97 %>%
  mutate(PUBLIC.01_1997 = case_when(`COW_YEMP-58500.01_R0171900` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.01_R0171900` %in% c(1, 5) ~ 1))
 
trainData97 <- trainData97 %>%
  mutate(PUBLIC.02_1997 = case_when(`COW_YEMP-58500.02_R0172000` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.02_R0172000` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.03_1997 = case_when(`COW_YEMP-58500.03_R0172100` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.03_R0172100` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.04_1997 = case_when(`COW_YEMP-58500.04_R0172200` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.04_R0172200` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.05_1997 = case_when(`COW_YEMP-58500.05_R0172300` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.05_R0172300` %in% c(1, 5) ~ 1))

# 1998
trainData97 <- trainData97 %>%
  mutate(PUBLIC.01_1998 = case_when(`COW_YEMP-58500.01_R1846600` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.01_R1846600` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.02_1998 = case_when(`COW_YEMP-58500.02_R1846700` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.02_R1846700` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.03_1998 = case_when(`COW_YEMP-58500.03_R1846800` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.03_R1846800` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.04_1998 = case_when(`COW_YEMP-58500.04_R1846900` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.04_R1846900` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.05_1998 = case_when(`COW_YEMP-58500.05_R1847000` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.05_R1847000` %in% c(1, 5) ~ 1))

# 1999
trainData97 <- trainData97 %>%
  mutate(PUBLIC.01_1999 = case_when(`COW_YEMP-58500.01_R3189000` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.01_R3189000` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.02_1999 = case_when(`COW_YEMP-58500.02_R3189100` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.02_R3189100` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.03_1999 = case_when(`COW_YEMP-58500.03_R3189200` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.03_R3189200` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.04_1999 = case_when(`COW_YEMP-58500.04_R3189300` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.04_R3189300` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.05_1999 = case_when(`COW_YEMP-58500.05_R3189400` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.05_R3189400` %in% c(1, 5) ~ 1))

# 2000
trainData97 <- trainData97 %>%
  mutate(PUBLIC.01_2000 = case_when(`COW_YEMP-58500.01_R4471400` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.01_R4471400` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.02_2000 = case_when(`COW_YEMP-58500.02_R4471500` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.02_R4471500` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.03_2000 = case_when(`COW_YEMP-58500.03_R4471600` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.03_R4471600` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.04_2000 = case_when(`COW_YEMP-58500.04_R4471700` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.04_R4471700` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.05_2000 = case_when(`COW_YEMP-58500.05_R4471800` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.05_R4471800` %in% c(1, 5) ~ 1))

# 2001
trainData97 <- trainData97 %>%
  mutate(PUBLIC.01_2001 = case_when(`COW_YEMP-58500.01_R6132100` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.01_R6132100` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.02_2001 = case_when(`COW_YEMP-58500.02_R6132200` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.02_R6132200` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.03_2001 = case_when(`COW_YEMP-58500.03_R6132300` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.03_R6132300` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.04_2001 = case_when(`COW_YEMP-58500.04_R6132400` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.04_R6132400` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.05_2001 = case_when(`COW_YEMP-58500.05_R6132500` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.05_R6132500` %in% c(1, 5) ~ 1))

# 2002
trainData97 <- trainData97 %>%
  mutate(PUBLIC.01_2002 = case_when(`COW_YEMP-58500.01_S0527800` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.01_S0527800` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.02_2002 = case_when(`COW_YEMP-58500.02_S0527900` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.02_S0527900` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.03_2002 = case_when(`COW_YEMP-58500.03_S0528000` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.03_S0528000` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.04_2002 = case_when(`COW_YEMP-58500.04_S0528100` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.04_S0528100` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.05_2002 = case_when(`COW_YEMP-58500.05_S0528200` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.05_S0528200` %in% c(1, 5) ~ 1))

# 2003
trainData97 <- trainData97 %>%
  mutate(PUBLIC.01_2003 = case_when(`COW_YEMP-58500.01_S2605700` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.01_S2605700` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.02_2003 = case_when(`COW_YEMP-58500.02_S2605800` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.02_S2605800` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.03_2003 = case_when(`COW_YEMP-58500.03_S2605900` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.03_S2605900` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.04_2003 = case_when(`COW_YEMP-58500.04_S2606000` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.04_S2606000` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.05_2003 = case_when(`COW_YEMP-58500.05_S2606100` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.05_S2606100` %in% c(1, 5) ~ 1))

# 2004
trainData97 <- trainData97 %>%
  mutate(PUBLIC.01_2004 = case_when(`COW_YEMP-58500.01_S4340700` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.01_S4340700` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.02_2004 = case_when(`COW_YEMP-58500.02_S4340800` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.02_S4340800` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.03_2004 = case_when(`COW_YEMP-58500.03_S4340900` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.03_S4340900` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.04_2004 = case_when(`COW_YEMP-58500.04_S4341000` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.04_S4341000` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.05_2004 = case_when(`COW_YEMP-58500.05_S4341100` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.05_S4341100` %in% c(1, 5) ~ 1))

# 2005
trainData97 <- trainData97 %>%
  mutate(PUBLIC.01_2005 = case_when(`COW_YEMP-58500.01_S5931500` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.01_S5931500` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.02_2005 = case_when(`COW_YEMP-58500.02_S5931600` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.02_S5931600` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.03_2005 = case_when(`COW_YEMP-58500.03_S5931700` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.03_S5931700` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.04_2005 = case_when(`COW_YEMP-58500.04_S5931800` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.04_S5931800` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.05_2005 = case_when(`COW_YEMP-58500.05_S5931900` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.05_S5931900` %in% c(1, 5) ~ 1))

# 2006
trainData97 <- trainData97 %>%
  mutate(PUBLIC.01_2006 = case_when(`COW_YEMP-58500.01_S8009500` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.01_S8009500` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.02_2006 = case_when(`COW_YEMP-58500.02_S8009600` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.02_S8009600` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.03_2006 = case_when(`COW_YEMP-58500.03_S8009700` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.03_S8009700` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.04_2006 = case_when(`COW_YEMP-58500.04_S8009800` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.04_S8009800` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.05_2006 = case_when(`COW_YEMP-58500.05_S8009900` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.05_S8009900` %in% c(1, 5) ~ 1))

# 2007
trainData97 <- trainData97 %>%
  mutate(PUBLIC.01_2007 = case_when(`COW_YEMP-58500.01_T0411500` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.01_T0411500` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.02_2007 = case_when(`COW_YEMP-58500.02_T0411600` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.02_T0411600` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.03_2007 = case_when(`COW_YEMP-58500.03_T0411700` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.03_T0411700` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.04_2007 = case_when(`COW_YEMP-58500.04_T0411800` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.04_T0411800` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.05_2007 = case_when(`COW_YEMP-58500.05_T0411900` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.05_T0411900` %in% c(1, 5) ~ 1))

# 2008
trainData97 <- trainData97 %>%
  mutate(PUBLIC.01_2008 = case_when(`COW_YEMP-58500.01_T2440600` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.01_T2440600` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.02_2008 = case_when(`COW_YEMP-58500.02_T2440700` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.02_T2440700` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.03_2008 = case_when(`COW_YEMP-58500.03_T2440800` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.03_T2440800` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.04_2008 = case_when(`COW_YEMP-58500.04_T2440900` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.04_T2440900` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.05_2008 = case_when(`COW_YEMP-58500.05_T2441000` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.05_T2441000` %in% c(1, 5) ~ 1))

# 2009
trainData97 <- trainData97 %>%
  mutate(PUBLIC.01_2009 = case_when(`COW_YEMP-58500.01_T3945500` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.01_T3945500` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.02_2009 = case_when(`COW_YEMP-58500.02_T3945600` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.02_T3945600` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.03_2009 = case_when(`COW_YEMP-58500.03_T3945700` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.03_T3945700` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.04_2009 = case_when(`COW_YEMP-58500.04_T3945800` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.04_T3945800` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.05_2009 = case_when(`COW_YEMP-58500.05_T3945900` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.05_T3945900` %in% c(1, 5) ~ 1))

# 2010
trainData97 <- trainData97 %>%
  mutate(PUBLIC.01_2010 = case_when(`COW_YEMP-58500.01_T5583900` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.01_T5583900` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.02_2010 = case_when(`COW_YEMP-58500.02_T5584000` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.02_T5584000` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.03_2010 = case_when(`COW_YEMP-58500.03_T5584100` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.03_T5584100` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.04_2010 = case_when(`COW_YEMP-58500.04_T5584200` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.04_T5584200` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.05_2010 = case_when(`COW_YEMP-58500.05_T5584300` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.05_T5584300` %in% c(1, 5) ~ 1))

# 2011
trainData97 <- trainData97 %>%
  mutate(PUBLIC.01_2011 = case_when(`COW_YEMP-58500.01_T6983800` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.01_T6983800` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.02_2011 = case_when(`COW_YEMP-58500.02_T6983900` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.02_T6983900` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.03_2011 = case_when(`COW_YEMP-58500.03_T6984000` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.03_T6984000` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.04_2011 = case_when(`COW_YEMP-58500.04_T6984100` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.04_T6984100` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.05_2011 = case_when(`COW_YEMP-58500.05_T6984200` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.05_T6984200` %in% c(1, 5) ~ 1))

# 2013
trainData97 <- trainData97 %>%
  mutate(PUBLIC.01_2013 = case_when(`COW_YEMP-58500.01_T8467200` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.01_T8467200` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.02_2013 = case_when(`COW_YEMP-58500.02_T8467300` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.02_T8467300` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.03_2013 = case_when(`COW_YEMP-58500.03_T8467400` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.03_T8467400` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.04_2013 = case_when(`COW_YEMP-58500.04_T8467500` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.04_T8467500` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.05_2013 = case_when(`COW_YEMP-58500.05_T8467600` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.05_T8467600` %in% c(1, 5) ~ 1))

# 2015
trainData97 <- trainData97 %>%
  mutate(PUBLIC.01_2015 = case_when(`COW_YEMP-58500.01_U0351800` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.01_U0351800` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.02_2015 = case_when(`COW_YEMP-58500.02_U0351900` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.02_U0351900` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.03_2015 = case_when(`COW_YEMP-58500.03_U0352000` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.03_U0352000` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.04_2015 = case_when(`COW_YEMP-58500.04_U0352100` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.04_U0352100` %in% c(1, 5) ~ 1))

trainData97 <- trainData97 %>%
  mutate(PUBLIC.05_2015 = case_when(`COW_YEMP-58500.05_U0352200` %in% c(2, 3, 4) ~ 0, 
                                           `COW_YEMP-58500.05_U0352200` %in% c(1, 5) ~ 1))

### tenure ###



