
####################
#                  #
#   Homework 8     #
#                  #
####################


## Your answers may be submitted as an annotated R file. ##
###########################################################

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


