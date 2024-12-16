attach("wfdata7.xlsx")
summary(wfdata7)
plot(wfdata7, lower.panel=NULL)
data(wfdata7)

# Regreassion

wfdata7.inerttemp<-lm(temp~pO2, wfdata7)
abline(reg=wfdata7.inerttemp)
summary(wfdata7.inerttemp)
coefficients(wfdata7.inerttemp)

# Training and Test Data

smp_size<-floor(0.8*nrow(wfdata7))
set.seed(123)
train_ind2<-sample(seq_len(nrow(wfdata7)), size= smp_size)
train2 <- wfdata7[train_ind2, ]
test2 <- wfdata7[-train_ind2, ]
twfdata7.inerttemp<-lm(percent_inertinite~temp, train2)
abline(reg=twfdata7.inerttemp)
summary(twfdata7.inerttemp)

new.data2<-data.frame(percent_inertinite=test2$percent_inertinite)
test2$output<-predict(twfdata7.inerttemp,new.data2)
sqrt(sum(test2$percent_inertinite-test2$output)^2/nrow(test2))

# Multivariate Regression

awfdata7.inerttemp<-lm(percent_inertinite~., wfdata7)
summary(awfdata7.inerttemp)

atwfdata7.inerttemp<-lm(percent_inertinite~., train)
summary(atwfdata7.inerttemp)

swfdata9.inertco2<-lm(percent_inertinite~pO2+biomass, wfdata7) # regression analysis of o2 and co2 with inertinite
summary(swfdata9.inertco2)

swfdata4.inertco2<-lm(percent_inertinite~pO2, wfdata7) # regression analysis of o2 with inertinite
summary(swfdata4.inertco2)

# Decision Trees
install.packages("party")
library(party)
tree2<-ctree(percent_inertinite~., data=wfdata7)
plot(tree2)

ttree3<-ctree(percent_inertinite~., data=train)
plot(ttree3)

tree.predict2<-predict(ttree,new.data=test)
tree.predict2
sqrt(sum(test$percent_inertinite-tree.predict2)^2/nrow(test))

tree.predict2<-predict(rftree, wfdata7)
tree.predict2
sqrt(sum(test$percent_inertinite-tree.predict)^2/nrow(test))


plot(tree.predict~wfdata7)

# Random Forest
install.packages("randomForest")
library(randomForest)
rftree2<-randomForest(percent_inertinite~., data=train2, mtry=1, ntree=500) # random forest using the training data, the number of variables to randomly sample as candidates at each split = on, number of trees= 500
rftree2
plot(rftree2)
summary(rftree2)

varImpPlot(rftree2, pch = 19) # looking at purity Scores
summary(varImpPlot(rftree2))
purity2 <- varImpPlot(rftree2, pch = 19) # finding exact values of purity scores
purity2

result2 <- data.frame(test2$percent_inertinite, predict(rftree2, test2[1:4], type = "response")) #testing the random forest model with the test data
result2

