
####################
#                  #
#   Homework 8     #
#                  #
####################


## Your answers may be submitted as an annotated R file. ##
###########################################################

library(readr)
library(dplyr)

##############
## Question ##
##############

# For this problem you will use the files "homework08data01.csv", "homework08data02.csv", and "homework08data03.csv" to 
# demonstrate through simulation the issues that can result from using the "traditional" methods to treat missing data. Each
# of the three data sets contains real estate information for 1000 records that detail home selling price (y), 
# home size (x1), and a quality rating (x2). There are 400 missing values in each data set. The missing mechanism is 
# MCAR, MAR, and MNAR, respectively. The true relationship is y = 29.3 + 5.6*x1 + 3.8*x2 + epsilon and the standard 
# deviation of epsilon is 21.
# 
# Follow the procedures below for each of the three data sets.
#
# For each "traditional" method listed below, do the following:
#   (1) Repeat the following process 1000 times. 
#       (a) Take a simple random sample of size 500 from the data set.
#       (b) Treat the missing data according to the particular method.
#       (c) Estimate and record the three regression parameters. 
#       (d) Use the estimated regression parameters to calculate MSE.
#       (e) Determine the 95% confidence interval for each of the regression parameters and 
#           record whether or not it contains the true parameter value.
#   (2) Determine and report the mean and variance of the generated coefficients and the MSE. 
#   (3) Determine and report the coverage of the confidence intervals of the parameters.
#
# The "traditional" methods to use:
#   (1) Listwise deletion
#   (2) Pairwise deletion
#   (3) Arithmetic mean imputation
#   (4) Regression imputation
#   (5) Stochastic regression imputation
#   (6) Hot-Deck imputation
#   (7) Similar resonse pattern imputation
#   (8) Indicator method imputation
#
# Summarize your findings in a table and discuss your observations.

summary_table <- data.frame()

# Data Set 1 (MCAR)

data_set_1 <- read_csv('Homework08data01.csv', col_names = F)
colnames(data_set_1) <- c('y','x1','x2')

mse <- function(model){
  mean(summary(model)$residuals^2)
}

# Listwise Deletion
data_frame_1_1 <- data.frame()

for (i in 1:1000){
  sample_1_1 <- sample_n(data_set_1, 500)
  sample_1_1_clean <- na.omit(sample_1_1)
  
  # Paramaters
  model_1_1 <- lm(y ~ x1 + x2, data = sample_1_1_clean)
  data_frame_1_1[i,'y'] <- model_1_1$coefficients[1]
  data_frame_1_1[i,'x1'] <- model_1_1$coefficients[2]
  data_frame_1_1[i,'x2'] <- model_1_1$coefficients[3]
  
  # MSE
  data_frame_1_1[i, 'MSE'] <- mse(model_1_1)

  # Confidence Interval
  ci <- confint(model_1_1)
  
  if(ci[1,1] < 29.3 && ci[1,2] > 29.3) {
    data_frame_1_1[i,'True y'] = T
  } else {data_frame_1_1[i,'True y'] = F}

  if(ci[2,1] < 5.6 && ci[2,2] > 5.6) {
    data_frame_1_1[i,'True x1'] = T
  } else {data_frame_1_1[i,'True x1'] = F}
  
  if(ci[3,1] < 3.8 && ci[3,2] > 3.8) {
    data_frame_1_1[i,'True x2'] = T
  } else {data_frame_1_1[i,'True x2'] = F}
}

summary_table['Set 1, Listwise', 'Mean y'] <- mean(data_frame_1_1$y)
summary_table['Set 1, Listwise', 'Var y']  <- var(data_frame_1_1$y)
summary_table['Set 1, Listwise', 'Mean x1']  <- mean(data_frame_1_1$x1)
summary_table['Set 1, Listwise', 'Var x1']  <- var(data_frame_1_1$x1)
summary_table['Set 1, Listwise', 'Mean x2']  <- mean(data_frame_1_1$x2)
summary_table['Set 1, Listwise', 'Var x2']  <- var(data_frame_1_1$x2)
summary_table['Set 1, Listwise', 'Mean MSE']  <- mean(data_frame_1_1$MSE)
summary_table['Set 1, Listwise', 'Var MSE']  <- var(data_frame_1_1$MSE)

summary_table['Set 1, Listwise', 'True y %']  <- sum(data_frame_1_1$`True y`) / 1000
summary_table['Set 1, Listwise', 'True x1 %']  <- sum(data_frame_1_1$`True x2`) / 1000
summary_table['Set 1, Listwise', 'True x2 %']  <- sum(data_frame_1_1$`True x2`) / 1000

# Pairwise
data_frame_1_2<- data.frame()

for (i in 1:1000){
  sample_1_2 <- sample_n(data_set_1, 500)
  sample_1_2_clean <- sample_1_2
  
  # Paramaters
  model_1_2 <- lm(y ~ x1 + x2, data = sample_1_2_clean)
  data_frame_1_2[i,'y'] <- model_1_2$coefficients[1]
  data_frame_1_2[i,'x1'] <- model_1_2$coefficients[2]
  data_frame_1_2[i,'x2'] <- model_1_2$coefficients[3]
  
  # MSE
  data_frame_1_2[i, 'MSE'] <- mse(model_1_2)
  
  # Confidence Interval
  ci <- confint(model_1_2)
  
  if(ci[1,1] < 29.3 && ci[1,2] > 29.3) {
    data_frame_1_2[i,'True y'] = T
  } else {data_frame_1_2[i,'True y'] = F}
  
  if(ci[2,1] < 5.6 && ci[2,2] > 5.6) {
    data_frame_1_2[i,'True x1'] = T
  } else {data_frame_1_2[i,'True x1'] = F}
  
  if(ci[3,1] < 3.8 && ci[3,2] > 3.8) {
    data_frame_1_2[i,'True x2'] = T
  } else {data_frame_1_2[i,'True x2'] = F}
}

summary_table['Set 1, Pairwise', 'Mean y'] <- mean(data_frame_1_2$y)
summary_table['Set 1, Pairwise', 'Var y']  <- var(data_frame_1_2$y)
summary_table['Set 1, Pairwise', 'Mean x1']  <- mean(data_frame_1_2$x1)
summary_table['Set 1, Pairwise', 'Var x1']  <- var(data_frame_1_2$x1)
summary_table['Set 1, Pairwise', 'Mean x2']  <- mean(data_frame_1_2$x2)
summary_table['Set 1, Pairwise', 'Var x2']  <- var(data_frame_1_2$x2)
summary_table['Set 1, Pairwise', 'Mean MSE']  <- mean(data_frame_1_2$MSE)
summary_table['Set 1, Pairwise', 'Var MSE']  <- var(data_frame_1_2$MSE)

summary_table['Set 1, Pairwise', 'True y %']  <- sum(data_frame_1_2$`True y`) / 1000
summary_table['Set 1, Pairwise', 'True x1 %']  <- sum(data_frame_1_2$`True x2`) / 1000
summary_table['Set 1, Pairwise', 'True x2 %']  <- sum(data_frame_1_2$`True x2`) / 1000

# Arithetic Mean
data_frame_1_3<- data.frame()

for (i in 1:1000){
  sample_1_3 <- sample_n(data_set_1, 500)
  
  sample_1_3 <- sample_1_3 %>%
    mutate(y = ifelse(is.na(y),mean(na.omit(sample_1_3$y)),y)) 
  sample_1_3 <- sample_1_3 %>%
    mutate(x1 = ifelse(is.na(x1),mean(na.omit(sample_1_3$x1)),x1)) 
  sample_1_3 <- sample_1_3 %>%
    mutate(x2 = ifelse(is.na(x2),mean(na.omit(sample_1_3$x2)),x2)) 
  
  sample_1_3_clean <- sample_1_3
  
  # Paramaters
  model_1_3 <- lm(y ~ x1 + x2, data = sample_1_3_clean)
  data_frame_1_3[i,'y'] <- model_1_3$coefficients[1]
  data_frame_1_3[i,'x1'] <- model_1_3$coefficients[2]
  data_frame_1_3[i,'x2'] <- model_1_3$coefficients[3]
  
  # MSE
  data_frame_1_3[i, 'MSE'] <- mse(model_1_3)
  
  # Confidence Interval
  ci <- confint(model_1_3)
  
  if(ci[1,1] < 29.3 && ci[1,2] > 29.3) {
    data_frame_1_3[i,'True y'] = T
  } else {data_frame_1_3[i,'True y'] = F}
  
  if(ci[2,1] < 5.6 && ci[2,2] > 5.6) {
    data_frame_1_3[i,'True x1'] = T
  } else {data_frame_1_3[i,'True x1'] = F}
  
  if(ci[3,1] < 3.8 && ci[3,2] > 3.8) {
    data_frame_1_2[i,'True x2'] = T
  } else {data_frame_1_2[i,'True x2'] = F}
}


summary_table['Set 1, Art. Mean', 'Mean y'] <- mean(data_frame_1_3$y)
summary_table['Set 1, Art. Mean', 'Var y']  <- var(data_frame_1_3$y)
summary_table['Set 1, Art. Mean', 'Mean x1']  <- mean(data_frame_1_3$x1)
summary_table['Set 1, Art. Mean', 'Var x1']  <- var(data_frame_1_3$x1)
summary_table['Set 1, Art. Mean', 'Mean x2']  <- mean(data_frame_1_3$x2)
summary_table['Set 1, Art. Mean', 'Var x2']  <- var(data_frame_1_3$x2)
summary_table['Set 1, Art. Mean', 'Mean MSE']  <- mean(data_frame_1_3$MSE)
summary_table['Set 1, Art. Mean', 'Var MSE']  <- var(data_frame_1_3$MSE)

summary_table['Set 1, Art. Mean', 'True y %']  <- sum(data_frame_1_3$`True y`) / 1000
summary_table['Set 1, Art. Mean', 'True x1 %']  <- sum(data_frame_1_3$`True x2`) / 1000
summary_table['Set 1, Art. Mean', 'True x2 %']  <- sum(data_frame_1_3$`True x2`) / 1000

# Regression imputation
data_frame_1_4<- data.frame()

for (i in 1:1000){
  sample_1_4 <- sample_n(data_set_1, 500)

  
  
  # Paramaters
  model_1_4 <- lm(y ~ x1 + x2, data = sample_1_4_clean)
  data_frame_1_4[i,'y'] <- model_1_4$coefficients[1]
  data_frame_1_4[i,'x1'] <- model_1_4$coefficients[2]
  data_frame_1_4[i,'x2'] <- model_1_4$coefficients[3]
  
  # MSE
  data_frame_1_4[i, 'MSE'] <- mse(model_1_4)
  
  # Confidence Interval
  ci <- confint(model_1_4)
  
  if(ci[1,1] < 29.3 && ci[1,2] > 29.3) {
    data_frame_1_4[i,'True y'] = T
  } else {data_frame_1_4[i,'True y'] = F}
  
  if(ci[2,1] < 5.6 && ci[2,2] > 5.6) {
    data_frame_1_4[i,'True x1'] = T
  } else {data_frame_1_4[i,'True x1'] = F}
  
  if(ci[3,1] < 3.8 && ci[3,2] > 3.8) {
    data_frame_1_4[i,'True x2'] = T
  } else {data_frame_1_4[i,'True x2'] = F}
  print(i)
}


summary_table['Set 1, Reg Imp', 'Mean y'] <- mean(data_frame_1_4$y)
summary_table['Set 1, Reg Imp', 'Var y']  <- var(data_frame_1_4$y)
summary_table['Set 1, Reg Imp', 'Mean x1']  <- mean(data_frame_1_4$x1)
summary_table['Set 1, Reg Imp', 'Var x1']  <- var(data_frame_1_4$x1)
summary_table['Set 1, Reg Imp', 'Mean x2']  <- mean(data_frame_1_4$x2)
summary_table['Set 1, Reg Imp', 'Var x2']  <- var(data_frame_1_4$x2)
summary_table['Set 1, Reg Imp', 'Mean MSE']  <- mean(data_frame_1_4$MSE)
summary_table['Set 1, Reg Imp', 'Var MSE']  <- var(data_frame_1_4$MSE)

summary_table['Set 1, Reg Imp', 'True y %']  <- sum(data_frame_1_4$`True y`) / 1000
summary_table['Set 1, Reg Imp', 'True x1 %']  <- sum(data_frame_1_4$`True x2`) / 1000
summary_table['Set 1, Reg Imp', 'True x2 %']  <- sum(data_frame_1_4$`True x2`) / 1000

