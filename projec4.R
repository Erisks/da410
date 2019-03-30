#preparing and exploring data
head(beetles)
str(beetles)
beetles <- beetles[-1]  #removes the first variable(id) from the data set.
head(beetles)

table(beetles$species)

beetles$Bspecies <-factor(beetles$species, levels = c("1","2"),labels = c("H.oler","H.cardu"))

round(prop.table(table(beetles$Bspecies))*100, digits = 1)

#STEP 1 - normalizing numeric data
##the normalization function is created
nor <-function(x) {
  return ((x -min(x))/(max(x)-min(x)))   }

beetles_nor <- as.data.frame(lapply(beetles[2:5], nor))

summary(beetles_nor)

#STEP 2 - creating training and test dataset. ratio 26:13

bee_train <- beetles_nor[1:26,]
bee_test <- beetles_nor[27:39,]

#target value is species
bee_train_label <-beetles[1:26, 1]
bee_test_label <-beetles[27:39, 1]

#STEP 3 - training model on data
library(class)
#k is generally chosen as the square root of the number of observations.sqrt of 39
beetle_test_pred <- knn(train = bee_train, test = bee_test, cl=bee_train_label,k=5)

#STEP 4 - evaluade model performance
library(gmodels)
CrossTable(x=bee_test_label, y= beetle_test_pred,prop.chisq = FALSE)
error_rate = 1/sum(beetle_test_pred == beetles$species)
error_rate

#STEP 5 - improve the performance of the model (repeat step 3 and 4 with diff k) 
##training model on data
library(class)
#k = 4
beetle_test_pred4 <- knn(train = bee_train, test = bee_test, cl=bee_train_label,k=4)

## evaluade model performance
library(gmodels)
CrossTable(x=bee_test_label, y= beetle_test_pred4,prop.chisq = FALSE)

error_rate4 = 1/sum(beetle_test_pred4 == beetles$species)
error_rate4

#k = 3
beetle_test_pred3 <- knn(train = bee_train, test = bee_test, cl=bee_train_label,k=3)

## evaluade model performance
library(gmodels)
CrossTable(x=bee_test_label, y= beetle_test_pred3,prop.chisq = FALSE)

error_rate3 = 1/sum(beetle_test_pred3 == beetles$species)
error_rate3

#k = 1
beetle_test_pred1 <- knn(train = bee_train, test = bee_test, cl=bee_train_label,k=1)

## evaluade model performance
library(gmodels)
CrossTable(x=bee_test_label, y= beetle_test_pred1,prop.chisq = FALSE)

error_rate1 = 1/sum(beetle_test_pred1 == beetles$species)
error_rate1

#k = 7
beetle_test_pred7 <- knn(train = bee_train, test = bee_test, cl=bee_train_label,k=7)

## evaluade model performance
library(gmodels)
CrossTable(x=bee_test_label, y= beetle_test_pred7,prop.chisq = FALSE)

error_rate7 = 1/sum(beetle_test_pred7 == beetles$species)
error_rate7


