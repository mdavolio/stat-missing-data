
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

mean_y <- mean(data_frame_1_1$y)
var_y <- var(data_frame_1_1$y)
mean_x1 <- mean(data_frame_1_1$x1)
var_x1 <- var(data_frame_1_1$x1)
mean_x2 <- mean(data_frame_1_1$x2)
var_x2 <- var(data_frame_1_1$x2)
mean_mse <- mean(data_frame_1_1$MSE)
var_mse <- var(data_frame_1_1$MSE)

True_y <- sum(data_frame_1_1$`True y`) / 1000
True_x1 <- sum(data_frame_1_1$`True x2`) / 1000
True_x1 <- sum(data_frame_1_1$`True x2`) / 1000

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

mean_y <- mean(data_frame_1_2$y)
var_y <- var(data_frame_1_2$y)
mean_x1 <- mean(data_frame_1_2$x1)
var_x1 <- var(data_frame_1_2$x1)
mean_x2 <- mean(data_frame_1_2$x2)
var_x2 <- var(data_frame_1_2$x2)
mean_mse <- mean(data_frame_1_2$MSE)
var_mse <- var(data_frame_1_2$MSE)

True_y <- sum(data_frame_1_2$`True y`) / 1000
True_x1 <- sum(data_frame_1_2$`True x2`) / 1000
True_x1 <- sum(data_frame_1_2$`True x2`) / 1000