## import data ##
library(tidyverse)
data<-read_csv("pva97nko.csv", na=c(".", "NA", "", "?"))


## data cleaning and define measure levels ##
data<- data %>% 
  mutate(across(where(is.character) & !c(StatusCat96NK, DemGender, DemHomeOwner),parse_number)) %>% 
  mutate(across(where(is.character) | c(TargetB, StatusCatStarAll, DemCluster), as.factor))


## data modification/correction ##
## Replace DemMedIncome ##
data <- data %>% mutate(DemMedIncome=replace(DemMedIncome,DemMedIncome==0, NA))


data <- as.data.frame(data)  # for building DM models
row.names(data) <- data$ID
data <- data %>% select(-(2:3)) 


## Partitioning ##
library(caTools)
set.seed(1234)
split = sample.split(data$TargetB, SplitRatio = 0.5) 

####################################
######### Decision Tree ############
####################################

# Model building
library(rpart)
tree <- rpart(TargetB ~ ., data=data[split,],control=rpart.control(cp=0.005))

# Model pruning
cp.seq=tree$cptable[,1]
misc<-numeric()
for (i in 1:length(cp.seq)) {
  tree.predict <- predict(prune(tree, cp=cp.seq[i]),data[!split,],type="class")
  cm<-table(data$TargetB[!split], tree.predict)   # confusion matrix 
  misc[i]=(cm[1,2]+cm[2,1])/sum(cm) # misclassification rate
}

plot(tree$cptable[,'nsplit']+1,misc,
     type="o", xlab="Number of Leaves", ylab="Misclassification Rate")


# Final model
tree.final=prune(tree,cp=cp.seq[misc==min(misc)])
library(partykit)
plot(as.party(tree.final))
tree.class <- predict(tree.final, data[!split,], type = "class")
tree.misc<-min(misc)


######################
### Transformation ###
######################

data.xf <- data  # pass on data

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


########################################
######### Logistic Regression ##########
########################################

data.mdl<-data.imp # pass on data

# Build full model
full = glm(TargetB ~., family=binomial, data=data.mdl[split,])
summary(full)
n<-sum(split)


# Backward
reg.bwd <- step(full, direction="backward",k=log(n), trace = FALSE)
summary(reg.bwd)
reg.bwd.prob<-predict(reg.bwd,data.mdl[!split, ], type = "response")
reg.bwd.class <- ifelse(reg.bwd.prob > 0.5, 1, 0)
reg.bwd.misc<- 1-mean(reg.bwd.class == data.mdl$TargetB[!split])
reg.bwd.misc



############################################
################ Week 9 ####################
############################################

### Misclassification Rate ###
c(tree.misc,  reg.bwd.misc)



### Confusion Matrix ###
table(reg.bwd.class, data$TargetB[!split])


library(gmodels)
CrossTable(reg.bwd.class, data$TargetB[!split])



### Evaluations related to Confusion Matrix ###
library(caret)
reg.bwd.class <- as.factor(reg.bwd.class)
confusionMatrix(data = reg.bwd.class, 
                reference = data$TargetB[!split],
                positive = "1",      # Default is 1st level
                mode= "everything")



### ROC curves ###
library(pROC)
rocCurve.reg<- roc(response =  data$TargetB[!split], 
                   predictor = reg.bwd.prob,
                   levels = levels(data$TargetB[!split]))  # This function assumes that 2nd
                                                           # level is the event of interest, 
                                                           # and 1st level is control.
                   

# Reg: area under curve
auc(rocCurve.reg)

# DT
tree.prob <- predict(tree.final, data[!split,], type = "prob")[,2]
rocCurve.tree <- roc(data$TargetB[!split], tree.prob)


## Compare Area Under Curve
c(auc(rocCurve.tree), auc(rocCurve.reg))



## Plot ROC Curves
plot(rocCurve.tree, legacy.axes = TRUE, col= 'blue')
plot.roc(rocCurve.reg, add=TRUE, legacy.axes = TRUE, col= 'red')
legend('topleft', legend=c('valid.reg', 'valid.tree'), 
       col=c("red","blue"),lty=c(1,1))















 


















