
library(riskRegression)
IPRS_calibrationplot<-function(Outcome,Environment,IPRS_Analysis_Data){
  
  Outcome<-deparse(substitute(Outcome))
  Environment<-deparse(substitute(Environment))
  IPRS_Analysis_Data<-as.data.frame(IPRS_Analysis_Data)
  IPRS_Analysis_Data[,Outcome]<-as.factor(IPRS_Analysis_Data[,Outcome])
  IPRS_prediction_model<-glm(IPRS_Analysis_Data[,Outcome]~IPRS_Analysis_Data[,"IPRS"]+IPRS_Analysis_Data[,Environment],family="binomial")
  xb<-Score(list(IPRS=IPRS_prediction_model),Y~1,data=IPRS_Analysis_Data,
          plots="cal")
  plotCalibration(xb,brier.in.legend=TRUE,
                  method ="nne",
                  xlim = c(0, 1),
                  ylim = c(0, 1),
                  col=c("#0073c2"),
                  round = F)->p3
  return(p3)
  
}

