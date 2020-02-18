rm(list=ls())
library(rio)
library(dplyr)
breastCancer=import("WiscBreastCancer.csv")
summary(breastCancer)
breastCancer = breastCancer[complete.cases(breastCancer), ]
breastCancer <- subset(breastCancer, select = -c(Id))
breastCancer$Class = as.factor(breastCancer$Class)
breastCancer %>% 
  group_by(Class) %>% 
  tally()
#set.seed(101) # Set Seed so that same sample can be reproduced in future also
sample = sample.int(n = nrow(breastCancer), size = floor(.80*nrow(breastCancer)), replace = F)
train = breastCancer[sample, ]
test  = breastCancer[-sample, ]
train %>% 
  group_by(Class) %>% 
  tally()
test %>% 
  group_by(Class) %>% 
  tally()
model1 <-glm(Class~.,family=binomial,data=train)
summary(model1)
model <- glm(Class~Cl.thickness+Cell.shape
             +Marg.adhesion+Bare.nuclei
             +Bl.cromatin+Normal.nucleoli,
             family=binomial,data=train)
summary(model)
anova(model, test="Chisq")
prediction<- predict(model, test)
#pred <- predict(logit,newdata=data) #gives you b0 + b1x1 + b2x2 + b3x3
probs <- exp(prediction)/(1+exp(prediction))
probs
output <- cbind(test, probs)
output <- output %>%
  mutate(probs = ifelse(probs >0.5,1,0))
output <- output %>%
  mutate(ClassNo = ifelse(Class == "malignant",1,0))

#output <- output %>% mutate(accurate = 1*(Class == probs))
#sum(output$accurate)/nrow(output)
#confusionMatrix(data = as.numeric(prediction>0.5), reference = output$Class)
confusionMatrix(as.factor(output$probs),as.factor(output$ClassNo))
probs1 = probs
output <- cbind(output, probs1)
ggplot(output, aes(x=probs1, y=as.numeric(output$ClassNo))) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, fullrange=TRUE, 
              method.args = list(family=binomial)) + 
  ylab("Cancer")

library(caret)
highlyCorrelated <- findCorrelation(cor(train[0:8]), cutoff=0.5)
print(highlyCorrelated)

importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)
