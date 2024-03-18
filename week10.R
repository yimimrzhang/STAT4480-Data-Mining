########################################################
######## Predicted Modeling (Class Imbalance) ##########
########################################################

## import data ##
library(tidyverse)
data<-read_csv("pva97nk_raw.csv", na=c(".", "NA", "", "?"))
summary(data)


## data cleaning and define measure levels ##
data<- data %>% 
  mutate(across(where(is.character) & !c(StatusCat96NK, DemGender, DemHomeOwner),parse_number)) %>% 
  mutate(across(where(is.character) | c(TargetB, StatusCatStarAll, DemCluster), as.factor))


## data modification/correction ##
## Replace DemMedIncome ##
data <- data %>% mutate(DemMedIncome=replace(DemMedIncome,DemMedIncome==0, NA))


data <- as.data.frame(data)  # for building DM models
row.names(data) <- data$ID
data <- data %>% select(-c(ID,TargetD)) # use var names for consistency


## Partitioning ##
library(caTools)
set.seed(1234)
split = sample.split(data$TargetB, SplitRatio = 0.5) 

library(skimr) 
data %>% filter(split) %>% skim
data %>% filter(!split) %>% skim



####################################
######### Decision Tree ############
####################################

# Model building

library(rpart)
tree <- rpart(TargetB ~ ., data=data[split,],control=rpart.control(cp=0))
summary(tree)


# Model pruning on MISC
# cp.seq=tree$cptable[,1]
# misc<-numeric()
# for (i in 1:length(cp.seq)) {
#   tree.predict = predict(prune(tree, cp=cp.seq[i]), data[!split,],type="class")
#   cm=table(data$TargetB[!split], tree.predict) # confusion matrix
#   misc[i]=(cm[1,2]+cm[2,1])/sum(cm) # misclassification rate
# }
# 
# plot(tree$cptable[,'nsplit']+1,misc,
#      type="o", xlab="Number of Leaves", ylab="Misclassification Rate")
# 


# Model pruning on F-score USING alternative cutoff
library(pROC)
library(caret)
cp.seq=tree$cptable[,1]
fscore<-numeric()
fscore[1]<-0  # Set root node F-score zero
for (i in 2:length(cp.seq)) {
  tree.prob = predict(prune(tree, cp=cp.seq[i]), data[!split,],type="prob")[,2] 
  rocCurve.tree <- roc(data$TargetB[!split], tree.prob, quiet=TRUE)
  treeThresh <-  coords(rocCurve.tree, x = "best", best.method = "closest.topleft")
  tree.class <- as.factor(ifelse(tree.prob >= treeThresh$threshold, 1,0))
  fscore[i]<-confusionMatrix(tree.class,data$TargetB[!split],positive = "1")$byClass["F1"]
}

plot(tree$cptable[,'nsplit']+1,fscore,
     type="o", xlab="Number of Leaves", ylab="F-score")


# Final model
tree.final=prune(tree,cp=cp.seq[fscore==max(fscore)])  # max F-score=0.1165179
library(partykit)
plot(as.party(tree.final))



######################
### Transformation ###
######################

data.xf <- data  # pass on data

# numeric input xform
library(caret)
TransformParams <- data.xf %>% 
  filter(split) %>% 
  preProcess(method="YeoJohnson")
TransformParams$yj


# numeric input xform
data.xf <- data.xf %>% 
  mutate(across(starts_with(c("GiftCnt","GiftAvg")), ~ log(.+1)))

# nominal input xform
levels(data.xf$StatusCat96NK)<-c("A", "L", "N", "L", "N", "A")


######################
##### Imputation #####
######################

data.imp<-data.xf # pass on data


# check missing data
vars.na <- data.imp %>% select_if(colSums(is.na(.))>0) %>% names
vars.na

# numeric impute: By mean #
data.imp<-data.imp %>%
  mutate(across(where(is.numeric), ~replace_na(.,mean(.[split],na.rm = TRUE))))


# create missing value flag #
data.imp[paste(vars.na, "NA", sep=".")] <- ifelse(is.na(data.xf[vars.na]), 1, 0)  
data.imp <- data.imp %>% mutate(across(ends_with(".NA"), as.factor))



########################################
######### Logistic Regression ##########
########################################

data.mdl<-data.imp # pass on data

# summary(data.mdl)
# summary(data.mdl[split,])

# Build full model
full = glm(TargetB ~., family=binomial, 
           data=data.mdl[split,] %>% select(!GiftTimeFirst.NA)) # exclude GiftTimeFirst.NA
summary(full)
n<-sum(split)


# Backward
reg.bwd <- step(full, direction="backward",k=log(n), trace = FALSE)
summary(reg.bwd)
reg.bwd.prob<-predict(reg.bwd,data.mdl[!split,], type = "response")

# Use alternative cutoff
rocCurve.reg <- roc(data$TargetB[!split], reg.bwd.prob, quiet = TRUE)
regThresh <-  coords(rocCurve.reg, x = "best", best.method = "closest.topleft")
reg.class <- as.factor(ifelse(reg.bwd.prob >= regThresh$threshold, 1,0))
reg.fscore<-confusionMatrix(reg.class,data$TargetB[!split],
                           positive = "1")$byClass["F1"]

reg.fscore  # f-score=0.1303823

confusionMatrix(reg.class,data$TargetB[!split],
                positive = "1", mode= "everything")












