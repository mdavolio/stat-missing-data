
####################
#                  #
#   Homework 8     #
#                  #
####################


## Your answers may be submitted as an annotated R file. ##
###########################################################

library(readr)
library(dplyr)
library(HotDeckImputation)

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
#   (7) Similar resonse pattern imputation       # NOT DONE YET
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
  data_frame_1_1[i,'int'] <- model_1_1$coefficients[1]
  data_frame_1_1[i,'x1'] <- model_1_1$coefficients[2]
  data_frame_1_1[i,'x2'] <- model_1_1$coefficients[3]
  
  # MSE
  data_frame_1_1[i, 'MSE'] <- mse(model_1_1)

  # Confidence Interval
  ci <- confint(model_1_1)
  
  if(ci[1,1] < 29.3 && ci[1,2] > 29.3) {
    data_frame_1_1[i,'True int'] = T
  } else {data_frame_1_1[i,'True int'] = F}

  if(ci[2,1] < 5.6 && ci[2,2] > 5.6) {
    data_frame_1_1[i,'True x1'] = T
  } else {data_frame_1_1[i,'True x1'] = F}
  
  if(ci[3,1] < 3.8 && ci[3,2] > 3.8) {
    data_frame_1_1[i,'True x2'] = T
  } else {data_frame_1_1[i,'True x2'] = F}
}

summary_table['Set 1, Listwise', 'Mean int'] <- mean(data_frame_1_1$int)
summary_table['Set 1, Listwise', 'Var int']  <- var(data_frame_1_1$int)
summary_table['Set 1, Listwise', 'Mean x1']  <- mean(data_frame_1_1$x1)
summary_table['Set 1, Listwise', 'Var x1']  <- var(data_frame_1_1$x1)
summary_table['Set 1, Listwise', 'Mean x2']  <- mean(data_frame_1_1$x2)
summary_table['Set 1, Listwise', 'Var x2']  <- var(data_frame_1_1$x2)
summary_table['Set 1, Listwise', 'Mean MSE']  <- mean(data_frame_1_1$MSE)
summary_table['Set 1, Listwise', 'Var MSE']  <- var(data_frame_1_1$MSE)

summary_table['Set 1, Listwise', 'True int %']  <- sum(data_frame_1_1$`True int`) / 1000
summary_table['Set 1, Listwise', 'True x1 %']  <- sum(data_frame_1_1$`True x2`) / 1000
summary_table['Set 1, Listwise', 'True x2 %']  <- sum(data_frame_1_1$`True x2`) / 1000

# Pairwise
data_frame_1_2<- data.frame()

for (i in 1:1000){
  sample_1_2 <- sample_n(data_set_1, 500)
  sample_1_2_clean <- sample_1_2
  
  # Paramaters
  model_1_2 <- lm(y ~ x1 + x2, data = sample_1_2_clean)
  data_frame_1_2[i,'int'] <- model_1_2$coefficients[1]
  data_frame_1_2[i,'x1'] <- model_1_2$coefficients[2]
  data_frame_1_2[i,'x2'] <- model_1_2$coefficients[3]
  
  # MSE
  data_frame_1_2[i, 'MSE'] <- mse(model_1_2)
  
  # Confidence Interval
  ci <- confint(model_1_2)
  
  if(ci[1,1] < 29.3 && ci[1,2] > 29.3) {
    data_frame_1_2[i,'True int'] = T
  } else {data_frame_1_2[i,'True int'] = F}
  
  if(ci[2,1] < 5.6 && ci[2,2] > 5.6) {
    data_frame_1_2[i,'True x1'] = T
  } else {data_frame_1_2[i,'True x1'] = F}
  
  if(ci[3,1] < 3.8 && ci[3,2] > 3.8) {
    data_frame_1_2[i,'True x2'] = T
  } else {data_frame_1_2[i,'True x2'] = F}
}

summary_table['Set 1, Pairwise', 'Mean int'] <- mean(data_frame_1_2$int)
summary_table['Set 1, Pairwise', 'Var int']  <- var(data_frame_1_2$int)
summary_table['Set 1, Pairwise', 'Mean x1']  <- mean(data_frame_1_2$x1)
summary_table['Set 1, Pairwise', 'Var x1']  <- var(data_frame_1_2$x1)
summary_table['Set 1, Pairwise', 'Mean x2']  <- mean(data_frame_1_2$x2)
summary_table['Set 1, Pairwise', 'Var x2']  <- var(data_frame_1_2$x2)
summary_table['Set 1, Pairwise', 'Mean MSE']  <- mean(data_frame_1_2$MSE)
summary_table['Set 1, Pairwise', 'Var MSE']  <- var(data_frame_1_2$MSE)

summary_table['Set 1, Pairwise', 'True int %']  <- sum(data_frame_1_2$`True int`) / 1000
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
  data_frame_1_3[i,'int'] <- model_1_3$coefficients[1]
  data_frame_1_3[i,'x1'] <- model_1_3$coefficients[2]
  data_frame_1_3[i,'x2'] <- model_1_3$coefficients[3]
  
  # MSE
  data_frame_1_3[i, 'MSE'] <- mse(model_1_3)
  
  # Confidence Interval
  ci <- confint(model_1_3)
  
  if(ci[1,1] < 29.3 && ci[1,2] > 29.3) {
    data_frame_1_3[i,'True int'] = T
  } else {data_frame_1_3[i,'True int'] = F}
  
  if(ci[2,1] < 5.6 && ci[2,2] > 5.6) {
    data_frame_1_3[i,'True x1'] = T
  } else {data_frame_1_3[i,'True x1'] = F}
  
  if(ci[3,1] < 3.8 && ci[3,2] > 3.8) {
    data_frame_1_2[i,'True x2'] = T
  } else {data_frame_1_2[i,'True x2'] = F}
}

summary_table['Set 1, Art. Mean', 'Mean int'] <- mean(data_frame_1_3$int)
summary_table['Set 1, Art. Mean', 'Var int']  <- var(data_frame_1_3$int)
summary_table['Set 1, Art. Mean', 'Mean x1']  <- mean(data_frame_1_3$x1)
summary_table['Set 1, Art. Mean', 'Var x1']  <- var(data_frame_1_3$x1)
summary_table['Set 1, Art. Mean', 'Mean x2']  <- mean(data_frame_1_3$x2)
summary_table['Set 1, Art. Mean', 'Var x2']  <- var(data_frame_1_3$x2)
summary_table['Set 1, Art. Mean', 'Mean MSE']  <- mean(data_frame_1_3$MSE)
summary_table['Set 1, Art. Mean', 'Var MSE']  <- var(data_frame_1_3$MSE)

summary_table['Set 1, Art. Mean', 'True int %']  <- sum(data_frame_1_3$`True int`) / 1000
summary_table['Set 1, Art. Mean', 'True x1 %']  <- sum(data_frame_1_3$`True x2`) / 1000
summary_table['Set 1, Art. Mean', 'True x2 %']  <- sum(data_frame_1_3$`True x2`) / 1000

# Regression imputation
data_frame_1_4<- data.frame()

for (i in 1:1000){
  sample_1_4 <- sample_n(data_set_1, 500)

  y.lm <- lm(y ~ x1 + x2, data = sample_1_4)
  x1.lm <- lm(x1 ~ y + x2, data = sample_1_4)
  x2.lm <- lm(x2 ~ y + x1, data = sample_1_4)
  
  y_data <- data.frame(sample_1_4$x1,sample_1_4$x2)
  names(y_data) <- c('x1', 'x2')
  x1_data <- data.frame(sample_1_4$y,sample_1_4$x2)
  names(x1_data) <- c('y', 'x2')
  x2_data <- data.frame(sample_1_4$y,sample_1_4$x1)
  names(x2_data) <- c('y', 'x1')
  
  sample_1_4 <- sample_1_4 %>%
    mutate(y = ifelse(is.na(y),predict(y.lm,y_data),y)) 
  sample_1_4 <- sample_1_4 %>%
    mutate(x1 = ifelse(is.na(x1),predict(x1.lm,x1_data),x1))
  sample_1_4 <- sample_1_4 %>%
    mutate(x2 = ifelse(is.na(x2),predict(x2.lm,x2_data),x2))
  
  sample_1_4_clean <- sample_1_4
  
  # Paramaters
  model_1_4 <- lm(y ~ x1 + x2, data = sample_1_4_clean)
  data_frame_1_4[i,'int'] <- model_1_4$coefficients[1]
  data_frame_1_4[i,'x1'] <- model_1_4$coefficients[2]
  data_frame_1_4[i,'x2'] <- model_1_4$coefficients[3]
  
  # MSE
  data_frame_1_4[i, 'MSE'] <- mse(model_1_4)
  
  # Confidence Interval
  ci <- confint(model_1_4)
  
  if(ci[1,1] < 29.3 && ci[1,2] > 29.3) {
    data_frame_1_4[i,'True int'] = T
  } else {data_frame_1_4[i,'True int'] = F}
  
  if(ci[2,1] < 5.6 && ci[2,2] > 5.6) {
    data_frame_1_4[i,'True x1'] = T
  } else {data_frame_1_4[i,'True x1'] = F}
  
  if(ci[3,1] < 3.8 && ci[3,2] > 3.8) {
    data_frame_1_4[i,'True x2'] = T
  } else {data_frame_1_4[i,'True x2'] = F}
}

summary_table['Set 1, Reg Imp', 'Mean int'] <- mean(data_frame_1_4$int)
summary_table['Set 1, Reg Imp', 'Var int']  <- var(data_frame_1_4$int)
summary_table['Set 1, Reg Imp', 'Mean x1']  <- mean(data_frame_1_4$x1)
summary_table['Set 1, Reg Imp', 'Var x1']  <- var(data_frame_1_4$x1)
summary_table['Set 1, Reg Imp', 'Mean x2']  <- mean(data_frame_1_4$x2)
summary_table['Set 1, Reg Imp', 'Var x2']  <- var(data_frame_1_4$x2)
summary_table['Set 1, Reg Imp', 'Mean MSE']  <- mean(data_frame_1_4$MSE)
summary_table['Set 1, Reg Imp', 'Var MSE']  <- var(data_frame_1_4$MSE)

summary_table['Set 1, Reg Imp', 'True int %']  <- sum(data_frame_1_4$`True int`) / 1000
summary_table['Set 1, Reg Imp', 'True x1 %']  <- sum(data_frame_1_4$`True x2`) / 1000
summary_table['Set 1, Reg Imp', 'True x2 %']  <- sum(data_frame_1_4$`True x2`) / 1000

# Stochastic regression imputation 
data_frame_1_5<- data.frame()

for (i in 1:1000){
  sample_1_5 <- sample_n(data_set_1, 500)
  
  y.lm <- lm(y ~ x1 + x2, data = sample_1_5)
  x1.lm <- lm(x1 ~ y + x2, data = sample_1_5)
  x2.lm <- lm(x2 ~ y + x1, data = sample_1_5)
  
  y_data <- data.frame(sample_1_5$x1,sample_1_5$x2)
  names(y_data) <- c('x1', 'x2')
  x1_data <- data.frame(sample_1_5$y,sample_1_5$x2)
  names(x1_data) <- c('y', 'x2')
  x2_data <- data.frame(sample_1_5$y,sample_1_5$x1)
  names(x2_data) <- c('y', 'x1')
  
  sample_1_5 <- sample_1_5 %>%
    mutate(y = ifelse(is.na(y),predict(y.lm,y_data) + rnorm(1, mean = 0, sd = sqrt(mse(y.lm))),y))
  sample_1_5 <- sample_1_5 %>%
    mutate(x1 = ifelse(is.na(x1),predict(x1.lm,x1_data) + rnorm(1, mean = 0, sd = sqrt(mse(x1.lm))),x1))
  sample_1_5 <- sample_1_5 %>%
    mutate(x2 = ifelse(is.na(x2),predict(x2.lm,x2_data) + rnorm(1, mean = 0, sd = sqrt(mse(x2.lm))),x2))
  
  sample_1_5_clean <- sample_1_5
  
  # Paramaters
  model_1_5 <- lm(y ~ x1 + x2, data = sample_1_5_clean)
  data_frame_1_5[i,'int'] <- model_1_5$coefficients[1]
  data_frame_1_5[i,'x1'] <- model_1_5$coefficients[2]
  data_frame_1_5[i,'x2'] <- model_1_5$coefficients[3]
  
  # MSE
  data_frame_1_5[i, 'MSE'] <- mse(model_1_5)
  
  # Confidence Interval
  ci <- confint(model_1_5)
  
  if(ci[1,1] < 29.3 && ci[1,2] > 29.3) {
    data_frame_1_5[i,'True int'] = T
  } else {data_frame_1_5[i,'True int'] = F}
  
  if(ci[2,1] < 5.6 && ci[2,2] > 5.6) {
    data_frame_1_5[i,'True x1'] = T
  } else {data_frame_1_5[i,'True x1'] = F}
  
  if(ci[3,1] < 3.8 && ci[3,2] > 3.8) {
    data_frame_1_5[i,'True x2'] = T
  } else {data_frame_1_5[i,'True x2'] = F}
  print(i)
}

summary_table['Set 1, Stocastic Reg', 'Mean int'] <- mean(data_frame_1_5$int)
summary_table['Set 1, Stocastic Reg', 'Var int']  <- var(data_frame_1_5$int)
summary_table['Set 1, Stocastic Reg', 'Mean x1']  <- mean(data_frame_1_5$x1)
summary_table['Set 1, Stocastic Reg', 'Var x1']  <- var(data_frame_1_5$x1)
summary_table['Set 1, Stocastic Reg', 'Mean x2']  <- mean(data_frame_1_5$x2)
summary_table['Set 1, Stocastic Reg', 'Var x2']  <- var(data_frame_1_5$x2)
summary_table['Set 1, Stocastic Reg', 'Mean MSE']  <- mean(data_frame_1_5$MSE)
summary_table['Set 1, Stocastic Reg', 'Var MSE']  <- var(data_frame_1_5$MSE)

summary_table['Set 1, Stocastic Reg', 'True int %']  <- sum(data_frame_1_5$`True int`) / 1000
summary_table['Set 1, Stocastic Reg', 'True x1 %']  <- sum(data_frame_1_5$`True x2`) / 1000
summary_table['Set 1, Stocastic Reg', 'True x2 %']  <- sum(data_frame_1_5$`True x2`) / 1000

# Hot-Deck imputation
data_frame_1_6<- data.frame()

for (i in 1:1000){
  sample_1_6 <- sample_n(data_set_1, 500)
  
  sample_1_6 <- sample_1_6 %>%
    mutate(y = ifelse(is.na(y),sample(na.omit(sample_1_6$y), 1),y)) 
  sample_1_6 <- sample_1_6 %>%
    mutate(x1 = ifelse(is.na(x1),sample(na.omit(sample_1_6$x1), 1),x1)) 
  sample_1_6 <- sample_1_6 %>%
    mutate(x2 = ifelse(is.na(x2),sample(na.omit(sample_1_6$x2), 1),x2)) 
  
  sample_1_6_clean <- sample_1_6
  
  # Paramaters
  model_1_6 <- lm(y ~ x1 + x2, data = sample_1_6_clean)
  data_frame_1_6[i,'int'] <- model_1_6$coefficients[1]
  data_frame_1_6[i,'x1'] <- model_1_6$coefficients[2]
  data_frame_1_6[i,'x2'] <- model_1_6$coefficients[3]
  
  # MSE
  data_frame_1_6[i, 'MSE'] <- mse(model_1_6)
  
  # Confidence Interval
  ci <- confint(model_1_6)
  
  if(ci[1,1] < 29.3 && ci[1,2] > 29.3) {
    data_frame_1_6[i,'True int'] = T
  } else {data_frame_1_6[i,'True int'] = F}
  
  if(ci[2,1] < 5.6 && ci[2,2] > 5.6) {
    data_frame_1_6[i,'True x1'] = T
  } else {data_frame_1_6[i,'True x1'] = F}
  
  if(ci[3,1] < 3.8 && ci[3,2] > 3.8) {
    data_frame_1_6[i,'True x2'] = T
  } else {data_frame_1_6[i,'True x2'] = F}
}

summary_table['Set 1, Hot-Deck', 'Mean int'] <- mean(data_frame_1_6$int)
summary_table['Set 1, Hot-Deck', 'Var int']  <- var(data_frame_1_6$int)
summary_table['Set 1, Hot-Deck', 'Mean x1']  <- mean(data_frame_1_6$x1)
summary_table['Set 1, Hot-Deck', 'Var x1']  <- var(data_frame_1_6$x1)
summary_table['Set 1, Hot-Deck', 'Mean x2']  <- mean(data_frame_1_6$x2)
summary_table['Set 1, Hot-Deck', 'Var x2']  <- var(data_frame_1_6$x2)
summary_table['Set 1, Hot-Deck', 'Mean MSE']  <- mean(data_frame_1_6$MSE)
summary_table['Set 1, Hot-Deck', 'Var MSE']  <- var(data_frame_1_6$MSE)

summary_table['Set 1, Hot-Deck', 'True int %']  <- sum(data_frame_1_6$`True int`) / 1000
summary_table['Set 1, Hot-Deck', 'True x1 %']  <- sum(data_frame_1_6$`True x2`) / 1000
summary_table['Set 1, Hot-Deck', 'True x2 %']  <- sum(data_frame_1_6$`True x2`) / 1000

# Similar resonse pattern imputation 
data_frame_1_7<- data.frame()

for (i in 1:1000){
  sample_1_7 <- sample_n(data_set_1, 500)
  
  sample_1_7_clean <- impute.NN_HD(as.data.frame(sample_1_7))
  
  # Paramaters
  model_1_7 <- lm(y ~ ., data = sample_1_7_clean)
  data_frame_1_7[i,'int'] <- model_1_7$coefficients[1]
  data_frame_1_7[i,'x1'] <- model_1_7$coefficients[2]
  data_frame_1_7[i,'x2'] <- model_1_7$coefficients[3]
  
  # MSE
  data_frame_1_7[i, 'MSE'] <- mse(model_1_7)
  
  # Confidence Interval
  ci <- confint(model_1_7)
  
  if(ci[1,1] < 29.3 && ci[1,2] > 29.3) {
    data_frame_1_7[i,'True int'] = T
  } else {data_frame_1_7[i,'True int'] = F}
  
  if(ci[2,1] < 5.6 && ci[2,2] > 5.6) {
    data_frame_1_7[i,'True x1'] = T
  } else {data_frame_1_7[i,'True x1'] = F}
  
  if(ci[3,1] < 3.8 && ci[3,2] > 3.8) {
    data_frame_1_7[i,'True x2'] = T
  } else {data_frame_1_7[i,'True x2'] = F}
}

summary_table['Set 1, Similar Resp.', 'Mean int'] <- mean(data_frame_1_7$int)
summary_table['Set 1, Similar Resp.', 'Var int']  <- var(data_frame_1_7$int)
summary_table['Set 1, Similar Resp.', 'Mean x1']  <- mean(data_frame_1_7$x1)
summary_table['Set 1, Similar Resp.', 'Var x1']  <- var(data_frame_1_7$x1)
summary_table['Set 1, Similar Resp.', 'Mean x2']  <- mean(data_frame_1_7$x2)
summary_table['Set 1, Similar Resp.', 'Var x2']  <- var(data_frame_1_7$x2)
summary_table['Set 1, Similar Resp.', 'Mean MSE']  <- mean(data_frame_1_7$MSE)
summary_table['Set 1, Similar Resp.', 'Var MSE']  <- var(data_frame_1_7$MSE)

summary_table['Set 1, Similar Resp.', 'True int %']  <- sum(data_frame_1_7$`True int`) / 1000
summary_table['Set 1, Similar Resp.', 'True x1 %']  <- sum(data_frame_1_7$`True x2`) / 1000
summary_table['Set 1, Similar Resp.', 'True x2 %']  <- sum(data_frame_1_7$`True x2`) / 1000


# Indicator imputation
data_frame_1_8<- data.frame()

for (i in 1:1000){
  sample_1_8 <- sample_n(data_set_1, 500)
  
  sample_1_8 <- sample_1_8 %>%
    mutate(y_ind = ifelse(is.na(y),1,0)) %>%
    mutate(y = ifelse(is.na(y),0,y)) %>%
    mutate(x1_ind = ifelse(is.na(x1),1,0)) %>% 
    mutate(x1 = ifelse(is.na(x1),0,x1)) %>%
    mutate(x2_ind = ifelse(is.na(x2),1,0)) %>%
    mutate(x2 = ifelse(is.na(x2),0,x2)) 
  
  sample_1_8_clean <- sample_1_8
  
  # Paramaters
  model_1_8 <- lm(y ~ ., data = sample_1_8_clean)
  data_frame_1_8[i,'int'] <- model_1_8$coefficients[1]
  data_frame_1_8[i,'x1'] <- model_1_8$coefficients[2]
  data_frame_1_8[i,'x2'] <- model_1_8$coefficients[3]
  
  # MSE
  data_frame_1_8[i, 'MSE'] <- mse(model_1_8)
  
  # Confidence Interval
  ci <- confint(model_1_8)
  
  if(ci[1,1] < 29.3 && ci[1,2] > 29.3) {
    data_frame_1_8[i,'True int'] = T
  } else {data_frame_1_8[i,'True int'] = F}
  
  if(ci[2,1] < 5.6 && ci[2,2] > 5.6) {
    data_frame_1_8[i,'True x1'] = T
  } else {data_frame_1_8[i,'True x1'] = F}
  
  if(ci[3,1] < 3.8 && ci[3,2] > 3.8) {
    data_frame_1_8[i,'True x2'] = T
  } else {data_frame_1_8[i,'True x2'] = F}
}

summary_table['Set 1, Indicator', 'Mean int'] <- mean(data_frame_1_8$int)
summary_table['Set 1, Indicator', 'Var int']  <- var(data_frame_1_8$int)
summary_table['Set 1, Indicator', 'Mean x1']  <- mean(data_frame_1_8$x1)
summary_table['Set 1, Indicator', 'Var x1']  <- var(data_frame_1_8$x1)
summary_table['Set 1, Indicator', 'Mean x2']  <- mean(data_frame_1_8$x2)
summary_table['Set 1, Indicator', 'Var x2']  <- var(data_frame_1_8$x2)
summary_table['Set 1, Indicator', 'Mean MSE']  <- mean(data_frame_1_8$MSE)
summary_table['Set 1, Indicator', 'Var MSE']  <- var(data_frame_1_8$MSE)

summary_table['Set 1, Indicator', 'True int %']  <- sum(data_frame_1_8$`True int`) / 1000
summary_table['Set 1, Indicator', 'True x1 %']  <- sum(data_frame_1_8$`True x2`) / 1000
summary_table['Set 1, Indicator', 'True x2 %']  <- sum(data_frame_1_8$`True x2`) / 1000

# Data Set 2 (MAR)

data_set_2 <- read_csv('Homework08data02.csv', col_names = F)
colnames(data_set_1) <- c('y','x1','x2')

mse <- function(model){
  mean(summary(model)$residuals^2)
}

# Listwise Deletion
data_frame_1_1 <- data.frame()

for (i in 1:1000){
  sample_1_1 <- sample_n(data_set_2, 500)
  sample_1_1_clean <- na.omit(sample_1_1)
  
  # Paramaters
  model_1_1 <- lm(y ~ x1 + x2, data = sample_1_1_clean)
  data_frame_1_1[i,'int'] <- model_1_1$coefficients[1]
  data_frame_1_1[i,'x1'] <- model_1_1$coefficients[2]
  data_frame_1_1[i,'x2'] <- model_1_1$coefficients[3]
  
  # MSE
  data_frame_1_1[i, 'MSE'] <- mse(model_1_1)
  
  # Confidence Interval
  ci <- confint(model_1_1)
  
  if(ci[1,1] < 29.3 && ci[1,2] > 29.3) {
    data_frame_1_1[i,'True int'] = T
  } else {data_frame_1_1[i,'True int'] = F}
  
  if(ci[2,1] < 5.6 && ci[2,2] > 5.6) {
    data_frame_1_1[i,'True x1'] = T
  } else {data_frame_1_1[i,'True x1'] = F}
  
  if(ci[3,1] < 3.8 && ci[3,2] > 3.8) {
    data_frame_1_1[i,'True x2'] = T
  } else {data_frame_1_1[i,'True x2'] = F}
}

summary_table['Set 2, Listwise', 'Mean int'] <- mean(data_frame_1_1$int)
summary_table['Set 2, Listwise', 'Var int']  <- var(data_frame_1_1$int)
summary_table['Set 2, Listwise', 'Mean x1']  <- mean(data_frame_1_1$x1)
summary_table['Set 2, Listwise', 'Var x1']  <- var(data_frame_1_1$x1)
summary_table['Set 2, Listwise', 'Mean x2']  <- mean(data_frame_1_1$x2)
summary_table['Set 2, Listwise', 'Var x2']  <- var(data_frame_1_1$x2)
summary_table['Set 2, Listwise', 'Mean MSE']  <- mean(data_frame_1_1$MSE)
summary_table['Set 2, Listwise', 'Var MSE']  <- var(data_frame_1_1$MSE)

summary_table['Set 2, Listwise', 'True int %']  <- sum(data_frame_1_1$`True int`) / 1000
summary_table['Set 2, Listwise', 'True x1 %']  <- sum(data_frame_1_1$`True x2`) / 1000
summary_table['Set 2, Listwise', 'True x2 %']  <- sum(data_frame_1_1$`True x2`) / 1000

# Pairwise
data_frame_1_2<- data.frame()

for (i in 1:1000){
  sample_1_2 <- sample_n(data_set_2, 500)
  sample_1_2_clean <- sample_1_2
  
  # Paramaters
  model_1_2 <- lm(y ~ x1 + x2, data = sample_1_2_clean)
  data_frame_1_2[i,'int'] <- model_1_2$coefficients[1]
  data_frame_1_2[i,'x1'] <- model_1_2$coefficients[2]
  data_frame_1_2[i,'x2'] <- model_1_2$coefficients[3]
  
  # MSE
  data_frame_1_2[i, 'MSE'] <- mse(model_1_2)
  
  # Confidence Interval
  ci <- confint(model_1_2)
  
  if(ci[1,1] < 29.3 && ci[1,2] > 29.3) {
    data_frame_1_2[i,'True int'] = T
  } else {data_frame_1_2[i,'True int'] = F}
  
  if(ci[2,1] < 5.6 && ci[2,2] > 5.6) {
    data_frame_1_2[i,'True x1'] = T
  } else {data_frame_1_2[i,'True x1'] = F}
  
  if(ci[3,1] < 3.8 && ci[3,2] > 3.8) {
    data_frame_1_2[i,'True x2'] = T
  } else {data_frame_1_2[i,'True x2'] = F}
}

summary_table['Set 2, Pairwise', 'Mean int'] <- mean(data_frame_1_2$int)
summary_table['Set 2, Pairwise', 'Var int']  <- var(data_frame_1_2$int)
summary_table['Set 2, Pairwise', 'Mean x1']  <- mean(data_frame_1_2$x1)
summary_table['Set 2, Pairwise', 'Var x1']  <- var(data_frame_1_2$x1)
summary_table['Set 2, Pairwise', 'Mean x2']  <- mean(data_frame_1_2$x2)
summary_table['Set 2, Pairwise', 'Var x2']  <- var(data_frame_1_2$x2)
summary_table['Set 2, Pairwise', 'Mean MSE']  <- mean(data_frame_1_2$MSE)
summary_table['Set 2, Pairwise', 'Var MSE']  <- var(data_frame_1_2$MSE)

summary_table['Set 2, Pairwise', 'True int %']  <- sum(data_frame_1_2$`True int`) / 1000
summary_table['Set 2, Pairwise', 'True x1 %']  <- sum(data_frame_1_2$`True x2`) / 1000
summary_table['Set 2, Pairwise', 'True x2 %']  <- sum(data_frame_1_2$`True x2`) / 1000

# Arithetic Mean
data_frame_1_3<- data.frame()

for (i in 1:1000){
  sample_1_3 <- sample_n(data_set_2, 500)
  
  sample_1_3 <- sample_1_3 %>%
    mutate(y = ifelse(is.na(y),mean(na.omit(sample_1_3$y)),y)) 
  sample_1_3 <- sample_1_3 %>%
    mutate(x1 = ifelse(is.na(x1),mean(na.omit(sample_1_3$x1)),x1)) 
  sample_1_3 <- sample_1_3 %>%
    mutate(x2 = ifelse(is.na(x2),mean(na.omit(sample_1_3$x2)),x2)) 
  
  sample_1_3_clean <- sample_1_3
  
  # Paramaters
  model_1_3 <- lm(y ~ x1 + x2, data = sample_1_3_clean)
  data_frame_1_3[i,'int'] <- model_1_3$coefficients[1]
  data_frame_1_3[i,'x1'] <- model_1_3$coefficients[2]
  data_frame_1_3[i,'x2'] <- model_1_3$coefficients[3]
  
  # MSE
  data_frame_1_3[i, 'MSE'] <- mse(model_1_3)
  
  # Confidence Interval
  ci <- confint(model_1_3)
  
  if(ci[1,1] < 29.3 && ci[1,2] > 29.3) {
    data_frame_1_3[i,'True int'] = T
  } else {data_frame_1_3[i,'True int'] = F}
  
  if(ci[2,1] < 5.6 && ci[2,2] > 5.6) {
    data_frame_1_3[i,'True x1'] = T
  } else {data_frame_1_3[i,'True x1'] = F}
  
  if(ci[3,1] < 3.8 && ci[3,2] > 3.8) {
    data_frame_1_2[i,'True x2'] = T
  } else {data_frame_1_2[i,'True x2'] = F}
}

summary_table['Set 2, Art. Mean', 'Mean int'] <- mean(data_frame_1_3$int)
summary_table['Set 2, Art. Mean', 'Var int']  <- var(data_frame_1_3$int)
summary_table['Set 2, Art. Mean', 'Mean x1']  <- mean(data_frame_1_3$x1)
summary_table['Set 2, Art. Mean', 'Var x1']  <- var(data_frame_1_3$x1)
summary_table['Set 2, Art. Mean', 'Mean x2']  <- mean(data_frame_1_3$x2)
summary_table['Set 2, Art. Mean', 'Var x2']  <- var(data_frame_1_3$x2)
summary_table['Set 2, Art. Mean', 'Mean MSE']  <- mean(data_frame_1_3$MSE)
summary_table['Set 2, Art. Mean', 'Var MSE']  <- var(data_frame_1_3$MSE)

summary_table['Set 2, Art. Mean', 'True int %']  <- sum(data_frame_1_3$`True int`) / 1000
summary_table['Set 2, Art. Mean', 'True x1 %']  <- sum(data_frame_1_3$`True x2`) / 1000
summary_table['Set 2, Art. Mean', 'True x2 %']  <- sum(data_frame_1_3$`True x2`) / 1000

# Regression imputation
data_frame_1_4<- data.frame()

for (i in 1:1000){
  sample_1_4 <- sample_n(data_set_2, 500)
  
  y.lm <- lm(y ~ x1 + x2, data = sample_1_4)
  x1.lm <- lm(x1 ~ y + x2, data = sample_1_4)
  x2.lm <- lm(x2 ~ y + x1, data = sample_1_4)
  
  y_data <- data.frame(sample_1_4$x1,sample_1_4$x2)
  names(y_data) <- c('x1', 'x2')
  x1_data <- data.frame(sample_1_4$y,sample_1_4$x2)
  names(x1_data) <- c('y', 'x2')
  x2_data <- data.frame(sample_1_4$y,sample_1_4$x1)
  names(x2_data) <- c('y', 'x1')
  
  sample_1_4 <- sample_1_4 %>%
    mutate(y = ifelse(is.na(y),predict(y.lm,y_data),y)) 
  sample_1_4 <- sample_1_4 %>%
    mutate(x1 = ifelse(is.na(x1),predict(x1.lm,x1_data),x1))
  sample_1_4 <- sample_1_4 %>%
    mutate(x2 = ifelse(is.na(x2),predict(x2.lm,x2_data),x2))
  
  sample_1_4_clean <- sample_1_4
  
  # Paramaters
  model_1_4 <- lm(y ~ x1 + x2, data = sample_1_4_clean)
  data_frame_1_4[i,'int'] <- model_1_4$coefficients[1]
  data_frame_1_4[i,'x1'] <- model_1_4$coefficients[2]
  data_frame_1_4[i,'x2'] <- model_1_4$coefficients[3]
  
  # MSE
  data_frame_1_4[i, 'MSE'] <- mse(model_1_4)
  
  # Confidence Interval
  ci <- confint(model_1_4)
  
  if(ci[1,1] < 29.3 && ci[1,2] > 29.3) {
    data_frame_1_4[i,'True int'] = T
  } else {data_frame_1_4[i,'True int'] = F}
  
  if(ci[2,1] < 5.6 && ci[2,2] > 5.6) {
    data_frame_1_4[i,'True x1'] = T
  } else {data_frame_1_4[i,'True x1'] = F}
  
  if(ci[3,1] < 3.8 && ci[3,2] > 3.8) {
    data_frame_1_4[i,'True x2'] = T
  } else {data_frame_1_4[i,'True x2'] = F}
}

summary_table['Set 2, Reg Imp', 'Mean int'] <- mean(data_frame_1_4$int)
summary_table['Set 2, Reg Imp', 'Var int']  <- var(data_frame_1_4$int)
summary_table['Set 2, Reg Imp', 'Mean x1']  <- mean(data_frame_1_4$x1)
summary_table['Set 2, Reg Imp', 'Var x1']  <- var(data_frame_1_4$x1)
summary_table['Set 2, Reg Imp', 'Mean x2']  <- mean(data_frame_1_4$x2)
summary_table['Set 2, Reg Imp', 'Var x2']  <- var(data_frame_1_4$x2)
summary_table['Set 2, Reg Imp', 'Mean MSE']  <- mean(data_frame_1_4$MSE)
summary_table['Set 2, Reg Imp', 'Var MSE']  <- var(data_frame_1_4$MSE)

summary_table['Set 2, Reg Imp', 'True int %']  <- sum(data_frame_1_4$`True int`) / 1000
summary_table['Set 2, Reg Imp', 'True x1 %']  <- sum(data_frame_1_4$`True x2`) / 1000
summary_table['Set 2, Reg Imp', 'True x2 %']  <- sum(data_frame_1_4$`True x2`) / 1000

# Stochastic regression imputation 
data_frame_1_5<- data.frame()

for (i in 1:1000){
  sample_1_5 <- sample_n(data_set_2, 500)
  
  y.lm <- lm(y ~ x1 + x2, data = sample_1_5)
  x1.lm <- lm(x1 ~ y + x2, data = sample_1_5)
  x2.lm <- lm(x2 ~ y + x1, data = sample_1_5)
  
  y_data <- data.frame(sample_1_5$x1,sample_1_5$x2)
  names(y_data) <- c('x1', 'x2')
  x1_data <- data.frame(sample_1_5$y,sample_1_5$x2)
  names(x1_data) <- c('y', 'x2')
  x2_data <- data.frame(sample_1_5$y,sample_1_5$x1)
  names(x2_data) <- c('y', 'x1')
  
  sample_1_5 <- sample_1_5 %>%
    mutate(y = ifelse(is.na(y),predict(y.lm,y_data) + rnorm(1, mean = 0, sd = sqrt(mse(y.lm))),y))
  sample_1_5 <- sample_1_5 %>%
    mutate(x1 = ifelse(is.na(x1),predict(x1.lm,x1_data) + rnorm(1, mean = 0, sd = sqrt(mse(x1.lm))),x1))
  sample_1_5 <- sample_1_5 %>%
    mutate(x2 = ifelse(is.na(x2),predict(x2.lm,x2_data) + rnorm(1, mean = 0, sd = sqrt(mse(x2.lm))),x2))
  
  sample_1_5_clean <- sample_1_5
  
  # Paramaters
  model_1_5 <- lm(y ~ x1 + x2, data = sample_1_5_clean)
  data_frame_1_5[i,'int'] <- model_1_5$coefficients[1]
  data_frame_1_5[i,'x1'] <- model_1_5$coefficients[2]
  data_frame_1_5[i,'x2'] <- model_1_5$coefficients[3]
  
  # MSE
  data_frame_1_5[i, 'MSE'] <- mse(model_1_5)
  
  # Confidence Interval
  ci <- confint(model_1_5)
  
  if(ci[1,1] < 29.3 && ci[1,2] > 29.3) {
    data_frame_1_5[i,'True int'] = T
  } else {data_frame_1_5[i,'True int'] = F}
  
  if(ci[2,1] < 5.6 && ci[2,2] > 5.6) {
    data_frame_1_5[i,'True x1'] = T
  } else {data_frame_1_5[i,'True x1'] = F}
  
  if(ci[3,1] < 3.8 && ci[3,2] > 3.8) {
    data_frame_1_5[i,'True x2'] = T
  } else {data_frame_1_5[i,'True x2'] = F}
  print(i)
}

summary_table['Set 2, Stocastic Reg', 'Mean int'] <- mean(data_frame_1_5$int)
summary_table['Set 2, Stocastic Reg', 'Var int']  <- var(data_frame_1_5$int)
summary_table['Set 2, Stocastic Reg', 'Mean x1']  <- mean(data_frame_1_5$x1)
summary_table['Set 2, Stocastic Reg', 'Var x1']  <- var(data_frame_1_5$x1)
summary_table['Set 2, Stocastic Reg', 'Mean x2']  <- mean(data_frame_1_5$x2)
summary_table['Set 2, Stocastic Reg', 'Var x2']  <- var(data_frame_1_5$x2)
summary_table['Set 2, Stocastic Reg', 'Mean MSE']  <- mean(data_frame_1_5$MSE)
summary_table['Set 2, Stocastic Reg', 'Var MSE']  <- var(data_frame_1_5$MSE)

summary_table['Set 2, Stocastic Reg', 'True int %']  <- sum(data_frame_1_5$`True int`) / 1000
summary_table['Set 2, Stocastic Reg', 'True x1 %']  <- sum(data_frame_1_5$`True x2`) / 1000
summary_table['Set 2, Stocastic Reg', 'True x2 %']  <- sum(data_frame_1_5$`True x2`) / 1000

# Hot-Deck imputation
data_frame_1_6<- data.frame()

for (i in 1:1000){
  sample_1_6 <- sample_n(data_set_2, 500)
  
  sample_1_6 <- sample_1_6 %>%
    mutate(y = ifelse(is.na(y),sample(na.omit(sample_1_6$y), 1),y)) 
  sample_1_6 <- sample_1_6 %>%
    mutate(x1 = ifelse(is.na(x1),sample(na.omit(sample_1_6$x1), 1),x1)) 
  sample_1_6 <- sample_1_6 %>%
    mutate(x2 = ifelse(is.na(x2),sample(na.omit(sample_1_6$x2), 1),x2)) 
  
  sample_1_6_clean <- sample_1_6
  
  # Paramaters
  model_1_6 <- lm(y ~ x1 + x2, data = sample_1_6_clean)
  data_frame_1_6[i,'int'] <- model_1_6$coefficients[1]
  data_frame_1_6[i,'x1'] <- model_1_6$coefficients[2]
  data_frame_1_6[i,'x2'] <- model_1_6$coefficients[3]
  
  # MSE
  data_frame_1_6[i, 'MSE'] <- mse(model_1_6)
  
  # Confidence Interval
  ci <- confint(model_1_6)
  
  if(ci[1,1] < 29.3 && ci[1,2] > 29.3) {
    data_frame_1_6[i,'True int'] = T
  } else {data_frame_1_6[i,'True int'] = F}
  
  if(ci[2,1] < 5.6 && ci[2,2] > 5.6) {
    data_frame_1_6[i,'True x1'] = T
  } else {data_frame_1_6[i,'True x1'] = F}
  
  if(ci[3,1] < 3.8 && ci[3,2] > 3.8) {
    data_frame_1_6[i,'True x2'] = T
  } else {data_frame_1_6[i,'True x2'] = F}
}

summary_table['Set 2, Hot-Deck', 'Mean int'] <- mean(data_frame_1_6$int)
summary_table['Set 2, Hot-Deck', 'Var int']  <- var(data_frame_1_6$int)
summary_table['Set 2, Hot-Deck', 'Mean x1']  <- mean(data_frame_1_6$x1)
summary_table['Set 2, Hot-Deck', 'Var x1']  <- var(data_frame_1_6$x1)
summary_table['Set 2, Hot-Deck', 'Mean x2']  <- mean(data_frame_1_6$x2)
summary_table['Set 2, Hot-Deck', 'Var x2']  <- var(data_frame_1_6$x2)
summary_table['Set 2, Hot-Deck', 'Mean MSE']  <- mean(data_frame_1_6$MSE)
summary_table['Set 2, Hot-Deck', 'Var MSE']  <- var(data_frame_1_6$MSE)

summary_table['Set 2, Hot-Deck', 'True int %']  <- sum(data_frame_1_6$`True int`) / 1000
summary_table['Set 2, Hot-Deck', 'True x1 %']  <- sum(data_frame_1_6$`True x2`) / 1000
summary_table['Set 2, Hot-Deck', 'True x2 %']  <- sum(data_frame_1_6$`True x2`) / 1000

# Similar resonse pattern imputation 
data_frame_1_7<- data.frame()

for (i in 1:1000){
  sample_1_7 <- sample_n(data_set_2, 500)
  
  sample_1_7_clean <- impute.NN_HD(as.data.frame(sample_1_7))
  
  # Paramaters
  model_1_7 <- lm(y ~ ., data = sample_1_7_clean)
  data_frame_1_7[i,'int'] <- model_1_7$coefficients[1]
  data_frame_1_7[i,'x1'] <- model_1_7$coefficients[2]
  data_frame_1_7[i,'x2'] <- model_1_7$coefficients[3]
  
  # MSE
  data_frame_1_7[i, 'MSE'] <- mse(model_1_7)
  
  # Confidence Interval
  ci <- confint(model_1_7)
  
  if(ci[1,1] < 29.3 && ci[1,2] > 29.3) {
    data_frame_1_7[i,'True int'] = T
  } else {data_frame_1_7[i,'True int'] = F}
  
  if(ci[2,1] < 5.6 && ci[2,2] > 5.6) {
    data_frame_1_7[i,'True x1'] = T
  } else {data_frame_1_7[i,'True x1'] = F}
  
  if(ci[3,1] < 3.8 && ci[3,2] > 3.8) {
    data_frame_1_7[i,'True x2'] = T
  } else {data_frame_1_7[i,'True x2'] = F}
}

summary_table['Set 2, Similar Resp.', 'Mean int'] <- mean(data_frame_1_7$int)
summary_table['Set 2, Similar Resp.', 'Var int']  <- var(data_frame_1_7$int)
summary_table['Set 2, Similar Resp.', 'Mean x1']  <- mean(data_frame_1_7$x1)
summary_table['Set 2, Similar Resp.', 'Var x1']  <- var(data_frame_1_7$x1)
summary_table['Set 2, Similar Resp.', 'Mean x2']  <- mean(data_frame_1_7$x2)
summary_table['Set 2, Similar Resp.', 'Var x2']  <- var(data_frame_1_7$x2)
summary_table['Set 2, Similar Resp.', 'Mean MSE']  <- mean(data_frame_1_7$MSE)
summary_table['Set 2, Similar Resp.', 'Var MSE']  <- var(data_frame_1_7$MSE)

summary_table['Set 2, Similar Resp.', 'True int %']  <- sum(data_frame_1_7$`True int`) / 1000
summary_table['Set 2, Similar Resp.', 'True x1 %']  <- sum(data_frame_1_7$`True x2`) / 1000
summary_table['Set 2, Similar Resp.', 'True x2 %']  <- sum(data_frame_1_7$`True x2`) / 1000


# Indicator imputation
data_frame_1_8<- data.frame()

for (i in 1:1000){
  sample_1_8 <- sample_n(data_set_2, 500)
  
  sample_1_8 <- sample_1_8 %>%
    mutate(y_ind = ifelse(is.na(y),1,0)) %>%
    mutate(y = ifelse(is.na(y),0,y)) %>%
    mutate(x1_ind = ifelse(is.na(x1),1,0)) %>% 
    mutate(x1 = ifelse(is.na(x1),0,x1)) %>%
    mutate(x2_ind = ifelse(is.na(x2),1,0)) %>%
    mutate(x2 = ifelse(is.na(x2),0,x2)) 
  
  sample_1_8_clean <- sample_1_8
  
  # Paramaters
  model_1_8 <- lm(y ~ ., data = sample_1_8_clean)
  data_frame_1_8[i,'int'] <- model_1_8$coefficients[1]
  data_frame_1_8[i,'x1'] <- model_1_8$coefficients[2]
  data_frame_1_8[i,'x2'] <- model_1_8$coefficients[3]
  
  # MSE
  data_frame_1_8[i, 'MSE'] <- mse(model_1_8)
  
  # Confidence Interval
  ci <- confint(model_1_8)
  
  if(ci[1,1] < 29.3 && ci[1,2] > 29.3) {
    data_frame_1_8[i,'True int'] = T
  } else {data_frame_1_8[i,'True int'] = F}
  
  if(ci[2,1] < 5.6 && ci[2,2] > 5.6) {
    data_frame_1_8[i,'True x1'] = T
  } else {data_frame_1_8[i,'True x1'] = F}
  
  if(ci[3,1] < 3.8 && ci[3,2] > 3.8) {
    data_frame_1_8[i,'True x2'] = T
  } else {data_frame_1_8[i,'True x2'] = F}
}

summary_table['Set 2, Indicator', 'Mean int'] <- mean(data_frame_1_8$int)
summary_table['Set 2, Indicator', 'Var int']  <- var(data_frame_1_8$int)
summary_table['Set 2, Indicator', 'Mean x1']  <- mean(data_frame_1_8$x1)
summary_table['Set 2, Indicator', 'Var x1']  <- var(data_frame_1_8$x1)
summary_table['Set 2, Indicator', 'Mean x2']  <- mean(data_frame_1_8$x2)
summary_table['Set 2, Indicator', 'Var x2']  <- var(data_frame_1_8$x2)
summary_table['Set 2, Indicator', 'Mean MSE']  <- mean(data_frame_1_8$MSE)
summary_table['Set 2, Indicator', 'Var MSE']  <- var(data_frame_1_8$MSE)

summary_table['Set 2, Indicator', 'True int %']  <- sum(data_frame_1_8$`True int`) / 1000
summary_table['Set 2, Indicator', 'True x1 %']  <- sum(data_frame_1_8$`True x2`) / 1000
summary_table['Set 2, Indicator', 'True x2 %']  <- sum(data_frame_1_8$`True x2`) / 1000

# Data Set 3 (MNAR)

data_set_3 <- read_csv('Homework08data03.csv', col_names = F)
colnames(data_set_1) <- c('y','x1','x2')

mse <- function(model){
  mean(summary(model)$residuals^2)
}

# Listwise Deletion
data_frame_1_1 <- data.frame()

for (i in 1:1000){
  sample_1_1 <- sample_n(data_set_3, 500)
  sample_1_1_clean <- na.omit(sample_1_1)
  
  # Paramaters
  model_1_1 <- lm(y ~ x1 + x2, data = sample_1_1_clean)
  data_frame_1_1[i,'int'] <- model_1_1$coefficients[1]
  data_frame_1_1[i,'x1'] <- model_1_1$coefficients[2]
  data_frame_1_1[i,'x2'] <- model_1_1$coefficients[3]
  
  # MSE
  data_frame_1_1[i, 'MSE'] <- mse(model_1_1)
  
  # Confidence Interval
  ci <- confint(model_1_1)
  
  if(ci[1,1] < 29.3 && ci[1,2] > 29.3) {
    data_frame_1_1[i,'True int'] = T
  } else {data_frame_1_1[i,'True int'] = F}
  
  if(ci[2,1] < 5.6 && ci[2,2] > 5.6) {
    data_frame_1_1[i,'True x1'] = T
  } else {data_frame_1_1[i,'True x1'] = F}
  
  if(ci[3,1] < 3.8 && ci[3,2] > 3.8) {
    data_frame_1_1[i,'True x2'] = T
  } else {data_frame_1_1[i,'True x2'] = F}
}

summary_table['Set 3, Listwise', 'Mean int'] <- mean(data_frame_1_1$int)
summary_table['Set 3, Listwise', 'Var int']  <- var(data_frame_1_1$int)
summary_table['Set 3, Listwise', 'Mean x1']  <- mean(data_frame_1_1$x1)
summary_table['Set 3, Listwise', 'Var x1']  <- var(data_frame_1_1$x1)
summary_table['Set 3, Listwise', 'Mean x2']  <- mean(data_frame_1_1$x2)
summary_table['Set 3, Listwise', 'Var x2']  <- var(data_frame_1_1$x2)
summary_table['Set 3, Listwise', 'Mean MSE']  <- mean(data_frame_1_1$MSE)
summary_table['Set 3, Listwise', 'Var MSE']  <- var(data_frame_1_1$MSE)

summary_table['Set 3, Listwise', 'True int %']  <- sum(data_frame_1_1$`True int`) / 1000
summary_table['Set 3, Listwise', 'True x1 %']  <- sum(data_frame_1_1$`True x2`) / 1000
summary_table['Set 3, Listwise', 'True x2 %']  <- sum(data_frame_1_1$`True x2`) / 1000

# Pairwise
data_frame_1_2<- data.frame()

for (i in 1:1000){
  sample_1_2 <- sample_n(data_set_3, 500)
  sample_1_2_clean <- sample_1_2
  
  # Paramaters
  model_1_2 <- lm(y ~ x1 + x2, data = sample_1_2_clean)
  data_frame_1_2[i,'int'] <- model_1_2$coefficients[1]
  data_frame_1_2[i,'x1'] <- model_1_2$coefficients[2]
  data_frame_1_2[i,'x2'] <- model_1_2$coefficients[3]
  
  # MSE
  data_frame_1_2[i, 'MSE'] <- mse(model_1_2)
  
  # Confidence Interval
  ci <- confint(model_1_2)
  
  if(ci[1,1] < 29.3 && ci[1,2] > 29.3) {
    data_frame_1_2[i,'True int'] = T
  } else {data_frame_1_2[i,'True int'] = F}
  
  if(ci[2,1] < 5.6 && ci[2,2] > 5.6) {
    data_frame_1_2[i,'True x1'] = T
  } else {data_frame_1_2[i,'True x1'] = F}
  
  if(ci[3,1] < 3.8 && ci[3,2] > 3.8) {
    data_frame_1_2[i,'True x2'] = T
  } else {data_frame_1_2[i,'True x2'] = F}
}

summary_table['Set 3, Pairwise', 'Mean int'] <- mean(data_frame_1_2$int)
summary_table['Set 3, Pairwise', 'Var int']  <- var(data_frame_1_2$int)
summary_table['Set 3, Pairwise', 'Mean x1']  <- mean(data_frame_1_2$x1)
summary_table['Set 3, Pairwise', 'Var x1']  <- var(data_frame_1_2$x1)
summary_table['Set 3, Pairwise', 'Mean x2']  <- mean(data_frame_1_2$x2)
summary_table['Set 3, Pairwise', 'Var x2']  <- var(data_frame_1_2$x2)
summary_table['Set 3, Pairwise', 'Mean MSE']  <- mean(data_frame_1_2$MSE)
summary_table['Set 3, Pairwise', 'Var MSE']  <- var(data_frame_1_2$MSE)

summary_table['Set 3, Pairwise', 'True int %']  <- sum(data_frame_1_2$`True int`) / 1000
summary_table['Set 3, Pairwise', 'True x1 %']  <- sum(data_frame_1_2$`True x2`) / 1000
summary_table['Set 3, Pairwise', 'True x2 %']  <- sum(data_frame_1_2$`True x2`) / 1000

# Arithetic Mean
data_frame_1_3<- data.frame()

for (i in 1:1000){
  sample_1_3 <- sample_n(data_set_3, 500)
  
  sample_1_3 <- sample_1_3 %>%
    mutate(y = ifelse(is.na(y),mean(na.omit(sample_1_3$y)),y)) 
  sample_1_3 <- sample_1_3 %>%
    mutate(x1 = ifelse(is.na(x1),mean(na.omit(sample_1_3$x1)),x1)) 
  sample_1_3 <- sample_1_3 %>%
    mutate(x2 = ifelse(is.na(x2),mean(na.omit(sample_1_3$x2)),x2)) 
  
  sample_1_3_clean <- sample_1_3
  
  # Paramaters
  model_1_3 <- lm(y ~ x1 + x2, data = sample_1_3_clean)
  data_frame_1_3[i,'int'] <- model_1_3$coefficients[1]
  data_frame_1_3[i,'x1'] <- model_1_3$coefficients[2]
  data_frame_1_3[i,'x2'] <- model_1_3$coefficients[3]
  
  # MSE
  data_frame_1_3[i, 'MSE'] <- mse(model_1_3)
  
  # Confidence Interval
  ci <- confint(model_1_3)
  
  if(ci[1,1] < 29.3 && ci[1,2] > 29.3) {
    data_frame_1_3[i,'True int'] = T
  } else {data_frame_1_3[i,'True int'] = F}
  
  if(ci[2,1] < 5.6 && ci[2,2] > 5.6) {
    data_frame_1_3[i,'True x1'] = T
  } else {data_frame_1_3[i,'True x1'] = F}
  
  if(ci[3,1] < 3.8 && ci[3,2] > 3.8) {
    data_frame_1_2[i,'True x2'] = T
  } else {data_frame_1_2[i,'True x2'] = F}
}

summary_table['Set 3, Art. Mean', 'Mean int'] <- mean(data_frame_1_3$int)
summary_table['Set 3, Art. Mean', 'Var int']  <- var(data_frame_1_3$int)
summary_table['Set 3, Art. Mean', 'Mean x1']  <- mean(data_frame_1_3$x1)
summary_table['Set 3, Art. Mean', 'Var x1']  <- var(data_frame_1_3$x1)
summary_table['Set 3, Art. Mean', 'Mean x2']  <- mean(data_frame_1_3$x2)
summary_table['Set 3, Art. Mean', 'Var x2']  <- var(data_frame_1_3$x2)
summary_table['Set 3, Art. Mean', 'Mean MSE']  <- mean(data_frame_1_3$MSE)
summary_table['Set 3, Art. Mean', 'Var MSE']  <- var(data_frame_1_3$MSE)

summary_table['Set 3, Art. Mean', 'True int %']  <- sum(data_frame_1_3$`True int`) / 1000
summary_table['Set 3, Art. Mean', 'True x1 %']  <- sum(data_frame_1_3$`True x2`) / 1000
summary_table['Set 3, Art. Mean', 'True x2 %']  <- sum(data_frame_1_3$`True x2`) / 1000

# Regression imputation
data_frame_1_4<- data.frame()

for (i in 1:1000){
  sample_1_4 <- sample_n(data_set_3, 500)
  
  y.lm <- lm(y ~ x1 + x2, data = sample_1_4)
  x1.lm <- lm(x1 ~ y + x2, data = sample_1_4)
  x2.lm <- lm(x2 ~ y + x1, data = sample_1_4)
  
  y_data <- data.frame(sample_1_4$x1,sample_1_4$x2)
  names(y_data) <- c('x1', 'x2')
  x1_data <- data.frame(sample_1_4$y,sample_1_4$x2)
  names(x1_data) <- c('y', 'x2')
  x2_data <- data.frame(sample_1_4$y,sample_1_4$x1)
  names(x2_data) <- c('y', 'x1')
  
  sample_1_4 <- sample_1_4 %>%
    mutate(y = ifelse(is.na(y),predict(y.lm,y_data),y)) 
  sample_1_4 <- sample_1_4 %>%
    mutate(x1 = ifelse(is.na(x1),predict(x1.lm,x1_data),x1))
  sample_1_4 <- sample_1_4 %>%
    mutate(x2 = ifelse(is.na(x2),predict(x2.lm,x2_data),x2))
  
  sample_1_4_clean <- sample_1_4
  
  # Paramaters
  model_1_4 <- lm(y ~ x1 + x2, data = sample_1_4_clean)
  data_frame_1_4[i,'int'] <- model_1_4$coefficients[1]
  data_frame_1_4[i,'x1'] <- model_1_4$coefficients[2]
  data_frame_1_4[i,'x2'] <- model_1_4$coefficients[3]
  
  # MSE
  data_frame_1_4[i, 'MSE'] <- mse(model_1_4)
  
  # Confidence Interval
  ci <- confint(model_1_4)
  
  if(ci[1,1] < 29.3 && ci[1,2] > 29.3) {
    data_frame_1_4[i,'True int'] = T
  } else {data_frame_1_4[i,'True int'] = F}
  
  if(ci[2,1] < 5.6 && ci[2,2] > 5.6) {
    data_frame_1_4[i,'True x1'] = T
  } else {data_frame_1_4[i,'True x1'] = F}
  
  if(ci[3,1] < 3.8 && ci[3,2] > 3.8) {
    data_frame_1_4[i,'True x2'] = T
  } else {data_frame_1_4[i,'True x2'] = F}
}

summary_table['Set 3, Reg Imp', 'Mean int'] <- mean(data_frame_1_4$int)
summary_table['Set 3, Reg Imp', 'Var int']  <- var(data_frame_1_4$int)
summary_table['Set 3, Reg Imp', 'Mean x1']  <- mean(data_frame_1_4$x1)
summary_table['Set 3, Reg Imp', 'Var x1']  <- var(data_frame_1_4$x1)
summary_table['Set 3, Reg Imp', 'Mean x2']  <- mean(data_frame_1_4$x2)
summary_table['Set 3, Reg Imp', 'Var x2']  <- var(data_frame_1_4$x2)
summary_table['Set 3, Reg Imp', 'Mean MSE']  <- mean(data_frame_1_4$MSE)
summary_table['Set 3, Reg Imp', 'Var MSE']  <- var(data_frame_1_4$MSE)

summary_table['Set 3, Reg Imp', 'True int %']  <- sum(data_frame_1_4$`True int`) / 1000
summary_table['Set 3, Reg Imp', 'True x1 %']  <- sum(data_frame_1_4$`True x2`) / 1000
summary_table['Set 3, Reg Imp', 'True x2 %']  <- sum(data_frame_1_4$`True x2`) / 1000

# Stochastic regression imputation 
data_frame_1_5<- data.frame()

for (i in 1:1000){
  sample_1_5 <- sample_n(data_set_3, 500)
  
  y.lm <- lm(y ~ x1 + x2, data = sample_1_5)
  x1.lm <- lm(x1 ~ y + x2, data = sample_1_5)
  x2.lm <- lm(x2 ~ y + x1, data = sample_1_5)
  
  y_data <- data.frame(sample_1_5$x1,sample_1_5$x2)
  names(y_data) <- c('x1', 'x2')
  x1_data <- data.frame(sample_1_5$y,sample_1_5$x2)
  names(x1_data) <- c('y', 'x2')
  x2_data <- data.frame(sample_1_5$y,sample_1_5$x1)
  names(x2_data) <- c('y', 'x1')
  
  sample_1_5 <- sample_1_5 %>%
    mutate(y = ifelse(is.na(y),predict(y.lm,y_data) + rnorm(1, mean = 0, sd = sqrt(mse(y.lm))),y))
  sample_1_5 <- sample_1_5 %>%
    mutate(x1 = ifelse(is.na(x1),predict(x1.lm,x1_data) + rnorm(1, mean = 0, sd = sqrt(mse(x1.lm))),x1))
  sample_1_5 <- sample_1_5 %>%
    mutate(x2 = ifelse(is.na(x2),predict(x2.lm,x2_data) + rnorm(1, mean = 0, sd = sqrt(mse(x2.lm))),x2))
  
  sample_1_5_clean <- sample_1_5
  
  # Paramaters
  model_1_5 <- lm(y ~ x1 + x2, data = sample_1_5_clean)
  data_frame_1_5[i,'int'] <- model_1_5$coefficients[1]
  data_frame_1_5[i,'x1'] <- model_1_5$coefficients[2]
  data_frame_1_5[i,'x2'] <- model_1_5$coefficients[3]
  
  # MSE
  data_frame_1_5[i, 'MSE'] <- mse(model_1_5)
  
  # Confidence Interval
  ci <- confint(model_1_5)
  
  if(ci[1,1] < 29.3 && ci[1,2] > 29.3) {
    data_frame_1_5[i,'True int'] = T
  } else {data_frame_1_5[i,'True int'] = F}
  
  if(ci[2,1] < 5.6 && ci[2,2] > 5.6) {
    data_frame_1_5[i,'True x1'] = T
  } else {data_frame_1_5[i,'True x1'] = F}
  
  if(ci[3,1] < 3.8 && ci[3,2] > 3.8) {
    data_frame_1_5[i,'True x2'] = T
  } else {data_frame_1_5[i,'True x2'] = F}
  print(i)
}

summary_table['Set 3, Stocastic Reg', 'Mean int'] <- mean(data_frame_1_5$int)
summary_table['Set 3, Stocastic Reg', 'Var int']  <- var(data_frame_1_5$int)
summary_table['Set 3, Stocastic Reg', 'Mean x1']  <- mean(data_frame_1_5$x1)
summary_table['Set 3, Stocastic Reg', 'Var x1']  <- var(data_frame_1_5$x1)
summary_table['Set 3, Stocastic Reg', 'Mean x2']  <- mean(data_frame_1_5$x2)
summary_table['Set 3, Stocastic Reg', 'Var x2']  <- var(data_frame_1_5$x2)
summary_table['Set 3, Stocastic Reg', 'Mean MSE']  <- mean(data_frame_1_5$MSE)
summary_table['Set 3, Stocastic Reg', 'Var MSE']  <- var(data_frame_1_5$MSE)

summary_table['Set 3, Stocastic Reg', 'True int %']  <- sum(data_frame_1_5$`True int`) / 1000
summary_table['Set 3, Stocastic Reg', 'True x1 %']  <- sum(data_frame_1_5$`True x2`) / 1000
summary_table['Set 3, Stocastic Reg', 'True x2 %']  <- sum(data_frame_1_5$`True x2`) / 1000

# Hot-Deck imputation
data_frame_1_6<- data.frame()

for (i in 1:1000){
  sample_1_6 <- sample_n(data_set_3, 500)
  
  sample_1_6 <- sample_1_6 %>%
    mutate(y = ifelse(is.na(y),sample(na.omit(sample_1_6$y), 1),y)) 
  sample_1_6 <- sample_1_6 %>%
    mutate(x1 = ifelse(is.na(x1),sample(na.omit(sample_1_6$x1), 1),x1)) 
  sample_1_6 <- sample_1_6 %>%
    mutate(x2 = ifelse(is.na(x2),sample(na.omit(sample_1_6$x2), 1),x2)) 
  
  sample_1_6_clean <- sample_1_6
  
  # Paramaters
  model_1_6 <- lm(y ~ x1 + x2, data = sample_1_6_clean)
  data_frame_1_6[i,'int'] <- model_1_6$coefficients[1]
  data_frame_1_6[i,'x1'] <- model_1_6$coefficients[2]
  data_frame_1_6[i,'x2'] <- model_1_6$coefficients[3]
  
  # MSE
  data_frame_1_6[i, 'MSE'] <- mse(model_1_6)
  
  # Confidence Interval
  ci <- confint(model_1_6)
  
  if(ci[1,1] < 29.3 && ci[1,2] > 29.3) {
    data_frame_1_6[i,'True int'] = T
  } else {data_frame_1_6[i,'True int'] = F}
  
  if(ci[2,1] < 5.6 && ci[2,2] > 5.6) {
    data_frame_1_6[i,'True x1'] = T
  } else {data_frame_1_6[i,'True x1'] = F}
  
  if(ci[3,1] < 3.8 && ci[3,2] > 3.8) {
    data_frame_1_6[i,'True x2'] = T
  } else {data_frame_1_6[i,'True x2'] = F}
}

summary_table['Set 3, Hot-Deck', 'Mean int'] <- mean(data_frame_1_6$int)
summary_table['Set 3, Hot-Deck', 'Var int']  <- var(data_frame_1_6$int)
summary_table['Set 3, Hot-Deck', 'Mean x1']  <- mean(data_frame_1_6$x1)
summary_table['Set 3, Hot-Deck', 'Var x1']  <- var(data_frame_1_6$x1)
summary_table['Set 3, Hot-Deck', 'Mean x2']  <- mean(data_frame_1_6$x2)
summary_table['Set 3, Hot-Deck', 'Var x2']  <- var(data_frame_1_6$x2)
summary_table['Set 3, Hot-Deck', 'Mean MSE']  <- mean(data_frame_1_6$MSE)
summary_table['Set 3, Hot-Deck', 'Var MSE']  <- var(data_frame_1_6$MSE)

summary_table['Set 3, Hot-Deck', 'True int %']  <- sum(data_frame_1_6$`True int`) / 1000
summary_table['Set 3, Hot-Deck', 'True x1 %']  <- sum(data_frame_1_6$`True x2`) / 1000
summary_table['Set 3, Hot-Deck', 'True x2 %']  <- sum(data_frame_1_6$`True x2`) / 1000

# Similar resonse pattern imputation 
data_frame_1_7<- data.frame()

for (i in 1:1000){
  sample_1_7 <- sample_n(data_set_3, 500)
  
  sample_1_7_clean <- impute.NN_HD(as.data.frame(sample_1_7))
  
  # Paramaters
  model_1_7 <- lm(y ~ ., data = sample_1_7_clean)
  data_frame_1_7[i,'int'] <- model_1_7$coefficients[1]
  data_frame_1_7[i,'x1'] <- model_1_7$coefficients[2]
  data_frame_1_7[i,'x2'] <- model_1_7$coefficients[3]
  
  # MSE
  data_frame_1_7[i, 'MSE'] <- mse(model_1_7)
  
  # Confidence Interval
  ci <- confint(model_1_7)
  
  if(ci[1,1] < 29.3 && ci[1,2] > 29.3) {
    data_frame_1_7[i,'True int'] = T
  } else {data_frame_1_7[i,'True int'] = F}
  
  if(ci[2,1] < 5.6 && ci[2,2] > 5.6) {
    data_frame_1_7[i,'True x1'] = T
  } else {data_frame_1_7[i,'True x1'] = F}
  
  if(ci[3,1] < 3.8 && ci[3,2] > 3.8) {
    data_frame_1_7[i,'True x2'] = T
  } else {data_frame_1_7[i,'True x2'] = F}
}

summary_table['Set 3, Similar Resp.', 'Mean int'] <- mean(data_frame_1_7$int)
summary_table['Set 3, Similar Resp.', 'Var int']  <- var(data_frame_1_7$int)
summary_table['Set 3, Similar Resp.', 'Mean x1']  <- mean(data_frame_1_7$x1)
summary_table['Set 3, Similar Resp.', 'Var x1']  <- var(data_frame_1_7$x1)
summary_table['Set 3, Similar Resp.', 'Mean x2']  <- mean(data_frame_1_7$x2)
summary_table['Set 3, Similar Resp.', 'Var x2']  <- var(data_frame_1_7$x2)
summary_table['Set 3, Similar Resp.', 'Mean MSE']  <- mean(data_frame_1_7$MSE)
summary_table['Set 3, Similar Resp.', 'Var MSE']  <- var(data_frame_1_7$MSE)

summary_table['Set 3, Similar Resp.', 'True int %']  <- sum(data_frame_1_7$`True int`) / 1000
summary_table['Set 3, Similar Resp.', 'True x1 %']  <- sum(data_frame_1_7$`True x2`) / 1000
summary_table['Set 3, Similar Resp.', 'True x2 %']  <- sum(data_frame_1_7$`True x2`) / 1000


# Indicator imputation
data_frame_1_8<- data.frame()

for (i in 1:1000){
  sample_1_8 <- sample_n(data_set_3, 500)
  
  sample_1_8 <- sample_1_8 %>%
    mutate(y_ind = ifelse(is.na(y),1,0)) %>%
    mutate(y = ifelse(is.na(y),0,y)) %>%
    mutate(x1_ind = ifelse(is.na(x1),1,0)) %>% 
    mutate(x1 = ifelse(is.na(x1),0,x1)) %>%
    mutate(x2_ind = ifelse(is.na(x2),1,0)) %>%
    mutate(x2 = ifelse(is.na(x2),0,x2)) 
  
  sample_1_8_clean <- sample_1_8
  
  # Paramaters
  model_1_8 <- lm(y ~ ., data = sample_1_8_clean)
  data_frame_1_8[i,'int'] <- model_1_8$coefficients[1]
  data_frame_1_8[i,'x1'] <- model_1_8$coefficients[2]
  data_frame_1_8[i,'x2'] <- model_1_8$coefficients[3]
  
  # MSE
  data_frame_1_8[i, 'MSE'] <- mse(model_1_8)
  
  # Confidence Interval
  ci <- confint(model_1_8)
  
  if(ci[1,1] < 29.3 && ci[1,2] > 29.3) {
    data_frame_1_8[i,'True int'] = T
  } else {data_frame_1_8[i,'True int'] = F}
  
  if(ci[2,1] < 5.6 && ci[2,2] > 5.6) {
    data_frame_1_8[i,'True x1'] = T
  } else {data_frame_1_8[i,'True x1'] = F}
  
  if(ci[3,1] < 3.8 && ci[3,2] > 3.8) {
    data_frame_1_8[i,'True x2'] = T
  } else {data_frame_1_8[i,'True x2'] = F}
}

summary_table['Set 3, Indicator', 'Mean int'] <- mean(data_frame_1_8$int)
summary_table['Set 3, Indicator', 'Var int']  <- var(data_frame_1_8$int)
summary_table['Set 3, Indicator', 'Mean x1']  <- mean(data_frame_1_8$x1)
summary_table['Set 3, Indicator', 'Var x1']  <- var(data_frame_1_8$x1)
summary_table['Set 3, Indicator', 'Mean x2']  <- mean(data_frame_1_8$x2)
summary_table['Set 3, Indicator', 'Var x2']  <- var(data_frame_1_8$x2)
summary_table['Set 3, Indicator', 'Mean MSE']  <- mean(data_frame_1_8$MSE)
summary_table['Set 3, Indicator', 'Var MSE']  <- var(data_frame_1_8$MSE)

summary_table['Set 3, Indicator', 'True int %']  <- sum(data_frame_1_8$`True int`) / 1000
summary_table['Set 3, Indicator', 'True x1 %']  <- sum(data_frame_1_8$`True x2`) / 1000
summary_table['Set 3, Indicator', 'True x2 %']  <- sum(data_frame_1_8$`True x2`) / 1000
