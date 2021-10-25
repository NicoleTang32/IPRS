rm(list=ls())

library(pROC)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(riskRegression)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source('./IPRS_prediction_model.R')
source('./IPRS_stratification_population.R')
source('./IPRS_calibrationplot.R')
source('./IPRS_estimate_decile.R')


#Enter the analysis file. See the Example file for the format
example<-read.csv("Example.csv",header = T)
SNP<-example[,grep("SNP",colnames(example))]

#get the IPRS's value in IPRS_Analysis_Data and the AUC and ROC curve of the IPRS prediction model
IPRS_prediction_model<-IPRS_prediction_model(Y,E,example,SNP)
IPRS_Analysis_Data<-IPRS_prediction_model$IPRS_Analysis_Data
IPRS_AUC<-print(IPRS_prediction_model$IPRS_AUC)

#get the bar charts of prevalence of different populations according to the predictive values of the IPRS prediction model
IPRS_stratification_population(Y,E,IPRS_Analysis_Data)

#get the calibration plot and the Brier scores of the IPRS prediction model
IPRS_calibrationplot(Y,E,IPRS_Analysis_Data)

#get Estimate of disease risk corresponding to IPRS deciles
IPRS_estimate_decile(Y,E,IPRS_Analysis_Data)

