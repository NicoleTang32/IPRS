library(ggplot2)
library(ggthemes)

IPRS_estimate_decile<-function(Outcome,Environment,IPRS_Analysis_Data){
  Outcome<-deparse(substitute(Outcome))
  Environment<-deparse(substitute(Environment))
  IPRS_Analysis_Data<-IPRS_Analysis_Data
  Estimate_decile<-data.frame(decile=c(1:10),estimate=c(NA),lower=c(NA),upper=c(NA))
  Estimate_decile[Estimate_decile$decile==1,]$estimate<-0
  Estimate_decile[Estimate_decile$decile==1,]$lower<-0
  Estimate_decile[Estimate_decile$decile==1,]$upper<-0
  
  IPRS_Prevelance<-IPRS_Analysis_Data[,c(Outcome,Environment,"IPRS")]
  IPRS_Prevelance1<-IPRS_Prevelance[IPRS_Prevelance$IPRS<=quantile(IPRS_Prevelance$IPRS,0.1),]
  IPRS_Prevelance1$PRS_deciles<-"0"
  IPRS_Prevelance2<-IPRS_Prevelance[IPRS_Prevelance$IPRS>quantile(IPRS_Prevelance$IPRS,0.1)&IPRS_Prevelance$IPRS<=quantile(IPRS_Prevelance$IPRS,0.2),]
  IPRS_Prevelance2$PRS_deciles<-"1"
  IPRS_Prevelance3<-IPRS_Prevelance[IPRS_Prevelance$IPRS>quantile(IPRS_Prevelance$IPRS,0.2)&IPRS_Prevelance$IPRS<=quantile(IPRS_Prevelance$IPRS,0.3),]
  IPRS_Prevelance3$PRS_deciles<-"1"
  IPRS_Prevelance4<-IPRS_Prevelance[IPRS_Prevelance$IPRS>quantile(IPRS_Prevelance$IPRS,0.3)&IPRS_Prevelance$IPRS<=quantile(IPRS_Prevelance$IPRS,0.4),]
  IPRS_Prevelance4$PRS_deciles<-"1"
  IPRS_Prevelance5<-IPRS_Prevelance[IPRS_Prevelance$IPRS>quantile(IPRS_Prevelance$IPRS,0.4)&IPRS_Prevelance$IPRS<=quantile(IPRS_Prevelance$IPRS,0.5),]
  IPRS_Prevelance5$PRS_deciles<-"1"
  IPRS_Prevelance6<-IPRS_Prevelance[IPRS_Prevelance$IPRS>quantile(IPRS_Prevelance$IPRS,0.5)&IPRS_Prevelance$IPRS<=quantile(IPRS_Prevelance$IPRS,0.6),]
  IPRS_Prevelance6$PRS_deciles<-"1"
  IPRS_Prevelance7<-IPRS_Prevelance[IPRS_Prevelance$IPRS>quantile(IPRS_Prevelance$IPRS,0.6)&IPRS_Prevelance$IPRS<=quantile(IPRS_Prevelance$IPRS,0.7),]
  IPRS_Prevelance7$PRS_deciles<-"1"
  IPRS_Prevelance8<-IPRS_Prevelance[IPRS_Prevelance$IPRS>quantile(IPRS_Prevelance$IPRS,0.7)&IPRS_Prevelance$IPRS<=quantile(IPRS_Prevelance$IPRS,0.8),]
  IPRS_Prevelance8$PRS_deciles<-"1"
  IPRS_Prevelance9<-IPRS_Prevelance[IPRS_Prevelance$IPRS>quantile(IPRS_Prevelance$IPRS,0.8)&IPRS_Prevelance$IPRS<=quantile(IPRS_Prevelance$IPRS,0.9),]
  IPRS_Prevelance9$PRS_deciles<-"1"
  IPRS_Prevelance10<-IPRS_Prevelance[IPRS_Prevelance$IPRS>quantile(IPRS_Prevelance$IPRS,0.9)&IPRS_Prevelance$IPRS<=quantile(IPRS_Prevelance$IPRS,1),]
  IPRS_Prevelance10$PRS_deciles<-"1"
  
  #IPRS的其他十分位数分别和最低的十分位数比较得到的Estimate值
  IPRS_Prevelance2<-rbind(IPRS_Prevelance2,IPRS_Prevelance1)
  IPRS_deciles2<-glm(IPRS_Prevelance2[,Outcome]~IPRS_Prevelance2[,"PRS_deciles"],family="binomial")
  a<-summary(IPRS_deciles2)
  Estimate_decile[Estimate_decile$decile==2,]$estimate<-a$coefficients[2,1]
  Estimate_decile[Estimate_decile$decile==2,]$lower<-a$coefficients[2,1]-1.96*a$coefficients[2,2]
  Estimate_decile[Estimate_decile$decile==2,]$upper<-a$coefficients[2,1]+1.96*a$coefficients[2,2]
  
  IPRS_Prevelance3<-rbind(IPRS_Prevelance3,IPRS_Prevelance1)
  IPRS_deciles3<-glm(IPRS_Prevelance3[,Outcome]~IPRS_Prevelance3[,"PRS_deciles"],family="binomial")
  a<-summary(IPRS_deciles3)
  Estimate_decile[Estimate_decile$decile==3,]$estimate<-a$coefficients[2,1]
  Estimate_decile[Estimate_decile$decile==3,]$lower<-a$coefficients[2,1]-1.96*a$coefficients[2,2]
  Estimate_decile[Estimate_decile$decile==3,]$upper<-a$coefficients[2,1]+1.96*a$coefficients[2,2]
  
  IPRS_Prevelance4<-rbind(IPRS_Prevelance4,IPRS_Prevelance1)
  IPRS_deciles4<-glm(IPRS_Prevelance4[,Outcome]~IPRS_Prevelance4[,"PRS_deciles"],family="binomial")
  a<-summary(IPRS_deciles4)
  Estimate_decile[Estimate_decile$decile==4,]$estimate<-a$coefficients[2,1]
  Estimate_decile[Estimate_decile$decile==4,]$lower<-a$coefficients[2,1]-1.96*a$coefficients[2,2]
  Estimate_decile[Estimate_decile$decile==4,]$upper<-a$coefficients[2,1]+1.96*a$coefficients[2,2]
  
  IPRS_Prevelance5<-rbind(IPRS_Prevelance5,IPRS_Prevelance1)
  IPRS_deciles5<-glm(IPRS_Prevelance5[,Outcome]~IPRS_Prevelance5[,"PRS_deciles"],family="binomial")
  a<-summary(IPRS_deciles5)
  Estimate_decile[Estimate_decile$decile==5,]$estimate<-a$coefficients[2,1]
  Estimate_decile[Estimate_decile$decile==5,]$lower<-a$coefficients[2,1]-1.96*a$coefficients[2,2]
  Estimate_decile[Estimate_decile$decile==5,]$upper<-a$coefficients[2,1]+1.96*a$coefficients[2,2]
  
  IPRS_Prevelance6<-rbind(IPRS_Prevelance6,IPRS_Prevelance1)
  IPRS_deciles6<-glm(IPRS_Prevelance6[,Outcome]~IPRS_Prevelance6[,"PRS_deciles"],family="binomial")
  a<-summary(IPRS_deciles6)
  Estimate_decile[Estimate_decile$decile==6,]$estimate<-a$coefficients[2,1]
  Estimate_decile[Estimate_decile$decile==6,]$lower<-a$coefficients[2,1]-1.96*a$coefficients[2,2]
  Estimate_decile[Estimate_decile$decile==6,]$upper<-a$coefficients[2,1]+1.96*a$coefficients[2,2]
  
  IPRS_Prevelance7<-rbind(IPRS_Prevelance7,IPRS_Prevelance1)
  IPRS_deciles7<-glm(IPRS_Prevelance7[,Outcome]~IPRS_Prevelance7[,"PRS_deciles"],family="binomial")
  a<-summary(IPRS_deciles7)
  Estimate_decile[Estimate_decile$decile==7,]$estimate<-a$coefficients[2,1]
  Estimate_decile[Estimate_decile$decile==7,]$lower<-a$coefficients[2,1]-1.96*a$coefficients[2,2]
  Estimate_decile[Estimate_decile$decile==7,]$upper<-a$coefficients[2,1]+1.96*a$coefficients[2,2]
  
  IPRS_Prevelance8<-rbind(IPRS_Prevelance8,IPRS_Prevelance1)
  IPRS_deciles8<-glm(IPRS_Prevelance8[,Outcome]~IPRS_Prevelance8[,"PRS_deciles"],family="binomial")
  a<-summary(IPRS_deciles8)
  Estimate_decile[Estimate_decile$decile==8,]$estimate<-a$coefficients[2,1]
  Estimate_decile[Estimate_decile$decile==8,]$lower<-a$coefficients[2,1]-1.96*a$coefficients[2,2]
  Estimate_decile[Estimate_decile$decile==8,]$upper<-a$coefficients[2,1]+1.96*a$coefficients[2,2]
  
  IPRS_Prevelance9<-rbind(IPRS_Prevelance9,IPRS_Prevelance1)
  IPRS_deciles9<-glm(IPRS_Prevelance9[,Outcome]~IPRS_Prevelance9[,"PRS_deciles"],family="binomial")
  a<-summary(IPRS_deciles9)
  Estimate_decile[Estimate_decile$decile==9,]$estimate<-a$coefficients[2,1]
  Estimate_decile[Estimate_decile$decile==9,]$lower<-a$coefficients[2,1]-1.96*a$coefficients[2,2]
  Estimate_decile[Estimate_decile$decile==9,]$upper<-a$coefficients[2,1]+1.96*a$coefficients[2,2]
  
  IPRS_Prevelance10<-rbind(IPRS_Prevelance10,IPRS_Prevelance1)
  IPRS_deciles10<-glm(IPRS_Prevelance10[,Outcome]~IPRS_Prevelance10[,"PRS_deciles"],family="binomial")
  a<-summary(IPRS_deciles10)
  Estimate_decile[Estimate_decile$decile==10,]$estimate<-a$coefficients[2,1]
  Estimate_decile[Estimate_decile$decile==10,]$lower<-a$coefficients[2,1]-1.96*a$coefficients[2,2]
  Estimate_decile[Estimate_decile$decile==10,]$upper<-a$coefficients[2,1]+1.96*a$coefficients[2,2]
  if(summary(lm(formula = estimate ~ decile, Estimate_decile))$coefficients[2,4]==0){
    trendp_label<-paste0("P < 2.2e-16")
  }else{
    trendp_label<-paste0("P=",signif(summary(lm(formula = estimate ~ decile, Estimate_decile))$coefficients[2,4],4))
  }
  
  ggplot(Estimate_decile,aes(x = decile, y =estimate,color=decile))+
    geom_errorbar(aes(ymin=lower, ymax=upper),position=position_dodge(0.9), width=0.1,size=1)+
    geom_point(position=position_dodge(0.9),size=3)+
    scale_x_discrete(limit=c("1","2","3","4","5","6","7","8","9","10"))+
    scale_y_continuous(limits = c(-1,max(Estimate_decile$upper)+2))+
    scale_colour_gradient(low ='#56B1F7', high ='#132C44')+
    geom_hline(yintercept =0,linetype="dashed")+
    labs(y = "Estimate (95%CI)")+
    theme_few()+
    theme(legend.position ='none')+
    annotate(geom="text",label=trendp_label,x=6, y=max(Estimate_decile$upper)+1,size=7)->p4
  return(print(p4))
}

