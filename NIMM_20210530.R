## Study Design (Hypothetical / Dummy)
## IQ Pills --> Increase IQ of Students
## 12 classes selected from schools, three IQ measurement each
## Total of 36 measurement for Test
## Same 12 classes given Ref and IQ measured (Total 36)
## NI margin -10% (Higher Values are better)
## 
### The lower limit of the 90% confidence interval for the difference of 
### IQ (test - reference), should not be less than -10%. (NI margin)
options(scipen = 999)

mydata <- read.csv("IQ_Data.csv")
## mydata <- read.csv("IQ_Data v2.csv")
 
test = mean(mydata[mydata$Group=="Test",]$IQ)
reference = mean(mydata[mydata$Group=="Ref",]$IQ)
test - reference
library(rpivotTable)
rpivotTable(mydata)

library(lme4)

## M0 = Baseline Model
M0 <-lmer(formula = IQ  ~ 1 + (1|Class) ,# DV~1+(1|Suspected Variable)
          data = mydata,
          REML = T)
summary(M0)
#

library(jtools)
summ(M0)

# Intraclass Correlation Coefficient ==> ?evidence of clustering ==> need Multilevel Modeling

library(lmerTest)
ranova(M0)
## *** indicate significant variation in the intercepts which 
# would provide support for multilevel approach
 
 
str(mydata)
M1 <-lmer(formula = IQ  ~  1+  Group + (1 |Class) ,# DV~1+(1|Suspected Variable)
          data = mydata,
          REML = T)
M1out = summary(M1) 
fixef(M1)
mixed_model_st_error = M1out$coefficients[2,2]
mixed_model_st_error
confint95mixed = confint(M1, level = 0.95)
confint95mixed = confint95mixed[4,1:2]
unname(confint95mixed) 
confint95mixed = as.character(confint95mixed)
confint95mixed = as.numeric(confint95mixed)


confint90mixed = confint(M1, level = 0.9)
confint90mixed = confint90mixed[4,1:2]
unname(confint90mixed) 
confint90mixed = as.character(confint90mixed)
confint90mixed = as.numeric(confint90mixed)

confint(M1, level = 0.9)
CI2 = c("2-sided 90% CI. (5%-95%)", "2-sided 95% CI.(2.5%-97.5%)")
ymin2= c(confint90mixed[1], confint95mixed[1])
ymax2= c(confint90mixed[2], confint95mixed[2])
Mean_Diff2 = c((test-reference),(test-reference))
myplot2 = data.frame(CI2,ymin2, ymax2,Mean_Diff2)
str(myplot2)

library(ggplot2)



p2=ggplot(myplot2, aes(x=CI2, y=Mean_Diff2, group=CI2, color=CI2)) + 
  geom_pointrange(aes(  ymin=ymin2, ymax=ymax2), size = 1.5)+
  geom_hline(yintercept = 10, size=1, color = "grey") + 
  geom_hline(yintercept = -10, size=1, color = "grey") +
  geom_hline(yintercept = mean(Mean_Diff2), size=1, color = "black") +
  geom_hline(yintercept = 0, size=1, linetype = "dashed") +
  theme(  panel.background = element_blank()) +
  
  
  annotate("text", x=2.3, y=10.7, label= "UCL") + 
  annotate("text", x=0.5, y=0.7, label= "0") + 
  annotate("text", x=0.5, y=10.7, label= "10%", col="springgreen4") + 
  annotate("text", x=0.5, y=-9.3, label= "-10%", col="red") + 
  annotate("text", x = 2.3, y=-9.3, label = "LCL")+
  annotate("text", x = 1.5, y=mean(Mean_Diff2)+0.7, label = "Mean Diff (Test-Ref)")+
  annotate("text", x = 0.5, y=mean(Mean_Diff2)+0.7, label = round(mean(Mean_Diff2),2))+
  
   
  geom_hline(yintercept = confint95mixed[2], size=0.5, color = "lightBlue", linetype = "dashed") + 
  
  geom_hline(yintercept = confint95mixed[1], size=0.5, color = "lightBlue", linetype = "dashed") + 
  
  geom_hline(yintercept = confint90mixed[2], size=0.5, color = "pink", linetype = "dashed") + 
  
  geom_hline(yintercept = confint90mixed[1], size=0.5, color = "pink", linetype = "dashed") + 
  
  
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  theme(legend.title = element_blank()) +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
p2 = p2+ylim(-15, 15)+ theme(legend.position="bottom") + ggtitle("Mixed Effect Model")

p2 = p2 + theme(
  # plot.title = element_blank(),
  axis.title.x = element_blank(),
  #,
  axis.title.y = element_blank()
)

p2 = p2 +  theme(plot.title = element_text(hjust = 0.5))
p2
