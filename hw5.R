#################################
####### Assignment Five #########
#################################

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


###############################
####### Decision Tree #########
###############################

library(rpart)
DT.001<- rpart(formula = TargetBuy ~ .,data = organics[split,],
               control=rpart.control(cp=0.001))

cp.seq=DT.001$cptable[,1]
MISC<-numeric()
for (i in 1:length(cp.seq)) {
  DT.predict = predict(prune(DT.001, cp=cp.seq[i]), organics[!split,],type="class")
  cm=table(DT.predict, organics$TargetBuy[!split])
  MISC[i]=(cm[1,2]+cm[2,1])/sum(cm)}

  
tree.final=prune(DT.001,cp=cp.seq[MISC==min(MISC)])
tree.class<-predict(tree.final, organics[!split,],type="class")
tree.prob <-predict(tree.final, organics[!split,], type = "prob")[,2]


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


#####################################
####### Logistic Regression #########
#####################################

organics.mdl<-organics.imp

levels(organics.mdl$TargetBuy) 

full = glm(TargetBuy ~ ., family=binomial, data=organics.mdl[split,])
summary(full)

null<-glm(TargetBuy ~ 1, family=binomial, data=organics.mdl[split,])

# Stepwise selection
reg.step <- step(null, scope=formula(full), direction="both", trace = FALSE)
summary(reg.step)


# Validation MISC
reg.step.prob<-predict(reg.step,organics.mdl[!split,], type = "response") 
reg.step.class <- ifelse(reg.step.prob > 0.5, 1, 0)
reg.step.misc<- 1-mean(reg.step.class == organics.mdl$TargetBuy[!split])
reg.step.misc


############################### Continue from here #################################

# part b.1 #
tree.misc<-min(MISC)

c(tree.misc, reg.step.misc)


# part b.2 #
TargetBuy.valid <- organics$TargetBuy[!split]

library(caret)
models.class <- data.frame(tree.class, reg.step.class) 
models.class %>% 
  mutate(across(everything(), as.factor)) %>% 
  summarise(across(everything(), ~ 1-confusionMatrix(., TargetBuy.valid, positive = "1")$byClass["Specificity"]))


# part b.3 #
models.class %>% 
  mutate(across(everything(), as.factor)) %>% 
  summarise(across(everything(), ~ confusionMatrix(., TargetBuy.valid, positive = "1")$byClass["F1"]))



# part b.4 #
library(pROC)
rocCurve.tree <- roc(TargetBuy.valid, tree.prob)
rocCurve.reg<- roc(TargetBuy.valid, reg.step.prob)



## Plot ROC Curves
plot(rocCurve.tree, legacy.axes = TRUE, col= 'blue')
plot.roc(rocCurve.reg, add=TRUE, legacy.axes = TRUE, col= 'red')
legend('topleft', legend=c('valid.reg', 'valid.tree'), 
       col=c("red","blue"),lty=c(1,1))

## Area under curve
auc(rocCurve.reg)














