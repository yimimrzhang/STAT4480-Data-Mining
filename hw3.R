################################
####### Assignment Three #######
################################

# parts a-d #
library(tidyverse)
organics<-read_csv("organics.csv",na=c(".", "NA", "", "?"))

organics <- organics %>% 
  mutate(across(where(is.character) | TargetBuy, as.factor)) 

organics <- as.data.frame(organics)  # for building DM models
row.names(organics) <- organics$ID
organics <- organics %>% select(-c(ID,TargetAmt,DemCluster))


# part e #
library(caTools)
set.seed(4321)
split <- sample.split(organics$TargetBuy, SplitRatio = 0.5)  


# part f #
library(rpart)
DT<- rpart(formula = TargetBuy ~ .,data = organics[split,])
summary(DT)
print(DT$cptable)

library(partykit)
plot(as.party(DT))

# part g.1 #
DT.001<- rpart(formula = TargetBuy ~ .,data = organics[split,],
           control=rpart.control(cp=0.001))

cp.seq=DT.001$cptable[,1]
MISC<-numeric()
for (i in 1:length(cp.seq)) {
  DT.predict = predict(prune(DT.001, cp=cp.seq[i]), organics[!split,],type="class")
  cm=table(DT.predict, organics$TargetBuy[!split])
  MISC[i]=(cm[1,2]+cm[2,1])/sum(cm)
}

plot(DT.001$cptable[,'nsplit']+1,MISC,
     type="o", xlab="Number of Leaves", ylab="Misclassification Rate")

min(MISC) # validation MISC


# part g.2 #
DT.final=prune(DT.001,cp=cp.seq[MISC==min(MISC)])
DT.final$cptable[,'nsplit']+1    # the last element


# part g.3 #
DT.final$variable.importance[1:3] 


# part g.4 #
pred.class <- predict(DT.final, organics[!split,] %>% slice(1), type="class")
pred.class
pred.prob <- predict(DT.final, organics[!split,] %>% slice(1), type="prob")
pred.prob




