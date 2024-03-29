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


## Partitioning ##
library(caTools)
set.seed(1234)
split = sample.split(data$TargetB, SplitRatio = 0.5) 

library(skimr) 
data %>% filter(split) %>% skim
data %>% filter(!split) %>% skim

############################################
################ Week 7 ####################
############################################

data <- as.data.frame(data)  # for building DM models
row.names(data) <- data$ID
data <- data %>% select(-(2:3)) 



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


data.xf <- data.xf %>% 
  mutate(across(starts_with(c("GiftCnt","GiftAvg")), ~ log(.+1)))
           


# nominal input xform
levels(data.xf$StatusCat96NK)
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
                                        # inspect missing values from previous dataframe


########################################
######### Logistic Regression ##########
########################################

data.mdl<-data.imp # pass on data


levels(data.mdl$TargetB) # check primary outcome 
                         # glm function uses 2nd factor level as primary

# Build full model
full <- glm(TargetB ~., family=binomial, data=data.mdl[split,])
summary(full)


# Set up null model
null <- glm(TargetB ~1, family=binomial, data=data.mdl[split,])
n<-sum(split) # training size (for BIC)


# Stepwise selection
reg.step <- step(null, scope=formula(full), direction="both", k=log(n))
summary(reg.step)
reg.step.prob<-predict(reg.step,data.mdl[!split,], type = "response") 
reg.step.class <- ifelse(reg.step.prob > 0.5, 1, 0)
reg.step.misc<- 1-mean(reg.step.class == data.mdl$TargetB[!split])
reg.step.misc


# Forward
reg.fwd <- step(null, scope=formula(full), direction="forward", k=log(n), trace = FALSE)
summary(reg.fwd)
reg.fwd.prob<-predict(reg.fwd,data.mdl[!split,], type = "response")
reg.fwd.class <- ifelse(reg.fwd.prob > 0.5, 1, 0)
reg.fwd.misc<- 1-mean(reg.fwd.class == data.mdl$TargetB[!split])
reg.fwd.misc


# Backward
reg.bwd <- step(full, direction="backward",k=log(n), trace = FALSE)
summary(reg.bwd)
reg.bwd.prob<-predict(reg.bwd,data.mdl[!split,], type = "response")
reg.bwd.class <- ifelse(reg.bwd.prob > 0.5, 1, 0)
reg.bwd.misc<- 1-mean(reg.bwd.class == data.mdl$TargetB[!split])

c(reg.step.misc, reg.fwd.misc, reg.bwd.misc)


# odds ratio estimate #
exp(coef(reg.bwd))


# variable importance #
library(caret)
varImp(reg.bwd) # absolute value of z stat
varImp(reg.bwd) %>% arrange((desc(Overall)) )


