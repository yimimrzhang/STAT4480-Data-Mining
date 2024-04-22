################################
####### Assignment Six #########
################################

library(tidyverse)
organics<-read_csv("organics.csv",na=c(".", "NA", "", "?"))

organics <- organics %>% 
  mutate(across(where(is.character) | TargetBuy, as.factor)) 

organics <- as.data.frame(organics)  # for building DM models
row.names(organics) <- organics$ID
organics <- organics %>% select(-c(ID,TargetAmt,DemCluster))

library(caTools)
set.seed(4321)
split = sample.split(organics$TargetBuy, SplitRatio = 0.5)  



#################################
####### Transformation  #########
#################################

organics.xf<-organics

organics.xf <- organics.xf %>% 
  mutate(across(c(PromTime, PromSpend), ~ log(.+1)))


#############################
####### Imputation  #########
#############################

organics.imp<-organics.xf

vars.na <- organics.imp %>% select_if(colSums(is.na(.))>0) %>% names

mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

organics.imp <-organics.imp %>% 
  mutate(across(where(is.factor) & !TargetBuy, ~replace_na(.,mode(.[split])))) %>%  # Nominal Input: By Mode #
  mutate(across(where(is.numeric), ~replace_na(.,mean(.[split],na.rm = TRUE))))     # Numeric Input: By Mean #


# Create Missing Value Flag #
organics.imp[paste(vars.na, "NA", sep=".")] <- ifelse(is.na(organics[vars.na]), 1, 0) 

organics.imp <- organics.imp %>% mutate(across(ends_with(".NA"), as.factor))


#######################
#### Random Forest ####
#######################

## part 1 ##
organics.rf<-organics.imp

minor<-unname(summary(organics.rf$TargetBuy[split])[2])

library(randomForest)
set.seed(4321)
RF <- randomForest(TargetBuy ~., data=organics.rf[split,],
                   ntree = 201,
                   strata= organics.rf$TargetBuy[split], 
                   sampsize=c(minor,minor),
                   importance = TRUE)

# Make predictions #
library(caret)
RF.class<- predict(RF, newdata=organics.rf[!split,], type="response")
fscore<-confusionMatrix(table(RF.class,organics.rf$TargetBuy[!split]),
                        positive = "1")$byClass["F1"]   
fscore


# Variable importance #
RF$importance
varImpPlot(RF)  



## part 2 ##

#### Parameter Tuning: mtry ####
m<-seq(2,5)
fscore.seq<-numeric()

for(i in 1:length(m)){ 
  set.seed(4321)
  rf <- randomForest(TargetBuy ~., data=organics.rf[split,],
                     ntree = 201,
                     strata= organics.rf$TargetBuy[split], 
                     sampsize=c(minor,minor),
                     mtry=m[i])
  
  rf.class<- predict(rf, newdata=organics.rf[!split,], type="response")
  fscore.seq[i]<-confusionMatrix(table(rf.class,organics.rf$TargetBuy[!split]),
                                 positive = "1")$byClass["F1"]
}

plot(m, fscore.seq, pch=19 , col="blue", type="b",
     ylab="F-score",xlab="Number of Predictors considered at each split")


m.best<- m[which.max(fscore.seq)] 
max(fscore.seq)




## part 3 ##
set.seed(4321)
RF.final <- randomForest(TargetBuy ~., data=organics.rf[split,],
                         ntree = 201,
                         strata= organics.rf$TargetBuy[split], 
                         sampsize=c(minor,minor),
                         importance = TRUE,
                         mtry=m.best)

# Make predictions #
RF.final.class<- predict(RF.final, newdata=organics.rf[!split,], type="response")
RF.final.prob <- predict(RF.final, newdata=organics.rf[!split,], type="prob")[,2] # predicted prob of TargetBuy=1 

# Confusion matrix
confusionMatrix(table(RF.final.class,organics.rf$TargetBuy[!split]), positive = "1")

# ROC curve
library(pROC)
rocCurve.RF <- roc(organics.rf$TargetBuy[!split], RF.final.prob)
plot(rocCurve.RF, legacy.axes = TRUE, col= 'blue')
auc(rocCurve.RF)




