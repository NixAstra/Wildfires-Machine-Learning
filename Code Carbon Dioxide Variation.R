attach("wfdata6.xlsx")
summary(wfdata6) # shows a summary of the data for each of the variables
plot(wfdata6, lower.panel=NULL) # Plotting the multi-plot to see the relationships between inertinite, co2, o2 and biomass
data(wfdata6)
plot(percent_inertinite~CO2_ppm, wfdata6)
plot(percent_inertinite~pO2, wfdata6)

# Regreassion
## Constructing the specific relationships between each of the variables, working out the R-squared values

wfdata.o2co2<-lm(CO2_ppm~pO2, wfdata6)
abline(reg=wfdata.o2co2)
summary(wfdata.o2co2)
coefficients(wfdata.o2co2)

wfdata7.inertco2<-lm(CO2_ppm~percent_inertinite, wfdata6)
abline(reg=wfdata7.inertco2)
summary(wfdata7.inertco2)


wfdata.bioco2<-lm(CO2_ppm~biomass, wfdata6)
abline(reg=wfdata.bioco2)
summary(wfdata.bioco2)

wfdata.bioo2<-lm(pO2~biomass, wfdata6)
abline(reg=wfdata.bioo2)
summary(wfdata.bioo2)


wfdata.inertbio<-lm(percent_inertinite~biomass, wfdata6)
abline(reg=wfdata.inertbio)
summary(wfdata.inertbio)

wfdata.inerto2<-lm(percent_inertinite~pO2, wfdata6)
abline(reg=wfdata.inerto2)
summary(wfdata.inerto2)


# Training and Test Data

smp_size<-floor(0.8*nrow(wfdata6)) # splits the data into an 80/20 split of a train and test data set
set.seed(123)
train_ind<-sample(seq_len(nrow(wfdata6)), size= smp_size) # setting the parameters of the train data set
train <- wfdata6[train_ind, ] # creating the train date set
test <- wfdata6[-train_ind, ] # creating the test data set
twfdata6.inertco2<-lm(percent_inertinite~CO2_ppm, train)
abline(reg=twfdata6.inertco2)
summary(twfdata6.inertco2)

new.data<-data.frame(percent_inertinite=test$percent_inertinite)
test$output<-predict(twfdata6.inertco2,new.data)
sqrt(sum(test$percent_inertinite-test$output)^2/nrow(test)) # calculating the error

# Multivariate Regression
## removed data each time based on which was the least significant relationship

awfdata6.inertco2<-lm(percent_inertinite~., wfdata6) # regression analysis on all of the data with inertinite
summary(awfdata6.inertco2)

atwfdata11.inertco2<-lm(percent_inertinite~., train) # regression analysis on the train data with inertinite
summary(atwfdata11.inertco2)

swfdata9.inertco2<-lm(percent_inertinite~pO2+biomass, wfdata6) # regression analysis of o2 and co2 with inertinite
summary(swfdata9.inertco2)

swfdata4.inertco2<-lm(percent_inertinite~pO2, wfdata6) # regression analysis of o2 with inertinite
summary(swfdata4.inertco2)


# Decision Trees
## creating and analysing the decision trees

install.packages("party")
library(party)
tree<-ctree(percent_inertinite~., data=wfdata6) # selecting the data needed to create a tree based on all of the data
plot(tree)

ttree<-ctree(percent_inertinite~., data=train) # selecting the data needed to create a tree based on the train data
plot(ttree)

tree.predict<-predict(ttree,new.data=test)
tree.predict
sqrt(sum(test$percent_inertinite-tree.predict)^2/nrow(test))

plot(tree.predict~wfdata2)

tree.predict6<-predict(rftree,wfdata6)
tree.predict6
sqrt(sum(test$percent_inertinite-tree.predict6)^2/nrow(test))



# Random Forest
## conducting random forest modelling using the training data

install.packages("randomForest")
library(randomForest)

rftree<-randomForest(percent_inertinite~., data=train, mtry=1, ntree=500) # random forest using the training data, the number of variables to randomly sample as candidates at each split = on, number of trees= 500
rftree
merror(rftree)
plot(rftree)
summary(rftree)

varImpPlot(rftree, pch = 19) # looking at purity Scores
summary(varImpPlot(rftree))
purity <- varImpPlot(rftree, pch = 19) # finding exact values of purity scores
purity

result <- data.frame(test$percent_inertinite, predict(rftree, test[1:4], type = "response")) #testing the random forest model with the test data
result

attach("wfdata8.xlsx")
summary(wfdata8) # shows a summary of the data for each of the variables
plot(wfdata8, lower.panel=NULL) # Plotting the multi-plot to see the relationships between inertinite, co2, o2 and biomass
data(wfdata8)


