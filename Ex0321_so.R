################################
####### Exercise 03/21 #########
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



###############################
####### Decision Tree #########
###############################

## part 1 ##

library(rpart)
DT.001<- rpart(formula = TargetBuy ~ .,data = organics[split,],
               control=rpart.control(cp=0.005))

library(caret)
library(pROC)
cp.seq=DT.001$cptable[,1]
fscore<-numeric(0)
fscore[1]<-0
for (i in 2:length(cp.seq)) {
  tree.prob = predict(prune(DT.001, cp=cp.seq[i]), organics[!split,],type="prob")[,2] 
  rocCurve.tree <- roc(organics$TargetBuy[!split], tree.prob, quiet=TRUE)
  treeThresh <-  coords(rocCurve.tree, x = "best", best.method = "closest.topleft", transpose = FALSE)
  tree.class <- as.factor(ifelse(tree.prob >= treeThresh$threshold, 1,0))
  fscore[i]<-confusionMatrix(tree.class,organics$TargetBuy[!split], positive = "1")$byClass["F1"]
}


plot(DT.001$cptable[,'nsplit']+1,fscore,
     type="o", xlab="Number of Leaves", ylab="F-score")


DT.001$cptable[which(fscore==max(fscore)),] # number of splits in the optimal tree
max(fscore) # max F-score=0.5606521


# Final model
tree.final=prune(DT.001,cp=cp.seq[fscore==max(fscore)])  
library(partykit)
plot(as.party(tree.final))




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

full = glm(TargetBuy ~ ., family=binomial, data=organics.mdl[split,])
summary(full)

null<-glm(TargetBuy ~ 1, family=binomial, data=organics.mdl[split,])

# Stepwise selection
reg.step <- step(null, scope=formula(full), direction="both", trace = FALSE)
summary(reg.step)


## part 2 ##

# Validation F-score
reg.step.prob<-predict(reg.step,organics.mdl[!split,], type = "response") 

rocCurve.reg <- roc(organics$TargetBuy[!split], reg.step.prob, quiet = TRUE)
regThresh <-  coords(rocCurve.reg, x = "best", best.method = "closest.topleft", transpose = FALSE)
regThresh
reg.class <- as.factor(ifelse(reg.step.prob >= regThresh$threshold, 1,0))
reg.fscore<-confusionMatrix(reg.class,organics$TargetBuy[!split],
                            positive = "1")$byClass["F1"]

reg.fscore  # best f-score=0.561067

confusionMatrix(reg.class,organics$TargetBuy[!split],
                positive = "1")






