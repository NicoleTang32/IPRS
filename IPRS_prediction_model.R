library(pROC)

IPRS_prediction_model<-function(Outcome,Environment,Analysis_Data,SNP_Data){
  Outcome<-deparse(substitute(Outcome))
  Environment<-deparse(substitute(Environment))
  IPRS_Analysis_Data<-Analysis_Data
  IPRS_SNP_Data<-SNP_Data
  IPRS_coef <- data.frame(SNP=1:length(IPRS_SNP_Data),IPRSm_coef=c(NA),IPRSi_coef=c(NA))
  IPRS_Analysis_Data[,"IPRSm"]<-0
  IPRS_Analysis_Data[,"IPRSi"]<-0
  for (i in 1:length(IPRS_SNP_Data)) {
    fit<-summary(glm(IPRS_Analysis_Data[,Outcome]~IPRS_SNP_Data[,i]+IPRS_Analysis_Data[,Environment]+IPRS_SNP_Data[,i]*IPRS_Analysis_Data[,Environment],family="binomial"))
    IPRS_coef[i,"IPRSm_coef"]<-fit$coefficients[2,1]
    IPRS_coef[i,"IPRSi_coef"]<-fit$coefficients[4,1]
    if (IPRS_coef[i,"IPRSm_coef"]>=0){
      IPRS_Analysis_Data[,"IPRSm"]<- IPRS_Analysis_Data[,"IPRSm"]+IPRS_coef[i,"IPRSm_coef"]*IPRS_SNP_Data[,i]
    }else{
      IPRS_Analysis_Data[,"IPRSm"]<- IPRS_Analysis_Data[,"IPRSm"]+abs(IPRS_coef[i,"IPRSm_coef"])*(2-IPRS_SNP_Data[,i])
    }
    if (IPRS_coef[i,"IPRSi_coef"]>=0){
      IPRS_Analysis_Data[,"IPRSi"]<- IPRS_Analysis_Data[,"IPRSi"]+IPRS_coef[i,"IPRSi_coef"]*(IPRS_SNP_Data[,i]*IPRS_Analysis_Data[,Environment])
    }else{
      IPRS_Analysis_Data[,"IPRSi"]<- IPRS_Analysis_Data[,"IPRSi"]+abs(IPRS_coef[i,"IPRSi_coef"])*(2-IPRS_SNP_Data[,i]*IPRS_Analysis_Data[,Environment])
    }
  }
  IPRS_Analysis_Data$IPRS<-abs(IPRS_Analysis_Data$IPRSm+IPRS_Analysis_Data$IPRSi)
  IPRS_Analysis_Data<-IPRS_Analysis_Data[,-which(colnames(IPRS_Analysis_Data) %in% c("IPRSm","IPRSi"))]
  IPRS_prediction_model<-glm(IPRS_Analysis_Data[,Outcome]~IPRS_Analysis_Data[,"IPRS"]+IPRS_Analysis_Data[,Environment],family="binomial")
  
  predictive_value <- predict(IPRS_prediction_model,IPRS_Analysis_Data)
  IPRS_AUC<-auc(roc(IPRS_Analysis_Data[,Outcome],predictive_value)) 
  IPRS_ROC<-roc(IPRS_Analysis_Data[,Outcome],predictive_value)
  plot(IPRS_ROC,
       print.auc=TRUE,
       print.auc.x=0.4,print.auc.y=0.5,
       auc.polygon=T,
       auc.polygon.col="#fff7f7",
       grid=c(0.5,0.2),
       grid.col=c("black","black"),
       main="ROC curve of IPRS prediction model",
       col="#FF2E63",
       legacy.axes=TRUE)->p1
  return(list(IPRS_Analysis_Data=IPRS_Analysis_Data,IPRS_AUC=IPRS_AUC,p1=p1))
}

