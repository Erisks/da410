##reading the data
admissions <- read.table(file = "C://Users/erili/Desktop/WINTER 2019/DA410-MULTIVARIATE-CHENG/admission.csv",
                            header = TRUE, sep =",")
head(admissions)
#scatter plot with the current data
plot(admissions$GPA,admissions$GMAT,col=admissions$De)

#MASS library for linear discirminat analysis 
library(MASS)

# 2. Train model using LDA by setting admit/not-admit/border with the same probabilities.
# train model
lda.same.prob <- lda(formula = De ~ .,
         data = admissions,
         prior = c(1,1,1)/3)
lda.same.prob

## predictions for students qith lda
student1 <- predict(lda.same.prob,newdata=data.frame(GPA=3.14,GMAT=470))
student2 <- predict(lda.same.prob,newdata=data.frame(GPA=3.08,GMAT=591))
student3 <- predict(lda.same.prob,newdata=data.frame(GPA=2.08,GMAT=641))
student4 <- predict(lda.same.prob,newdata=data.frame(GPA=3.22,GMAT=463))
student1$class
student2$class
student3$class
student4$class
## QDA
qda <- qda(formula = De ~ .,
         data = admissions,
         prior = c(1,1,1)/3)
qda

## predictions for students with qda
predict(qda,newdata=data.frame(GPA=3.14,GMAT=470))

# 3. Calculate the misclassfication rate
###Misclassification rate odf LDA rule
group <- predict(r, admissions, method = 'plug-in')$class
table(group, admissions$De)

#cross-validation rate of LDA rule:
correct <- rep(0, times=nrow(admissions))
for (j in 1:nrow(admissions))
{mydis <-lda(grouping = admissions$De[-j],
             x= admissions[-j,1:2],
             prior = c(1,1,1)/3)
mypred <-predict(mydis,newdata=admissions[j,1:2])$class
correct[j] <- (mypred == admissions$De[j])}
cv.misclass <- 1-mean(correct)
cv.misclass #the cross-validation misclasification rate for LDA here is: 

###################### PROBLEM 2 #######################################

# Train model using LDA by setting  
#admit is 50% while probability of not admit is 25% and probability of border is 25%. probabilities.
# train model
lda.diff.prob <- lda(formula = De ~ .,
         data = admissions,
         prior = c(0.5,0.25,0.25))
lda.diff.prob

## predictions for students qith lda
student01<- predict(lda.diff.prob,newdata=data.frame(GPA=3.14,GMAT=470))
student01
student02<- predict(lda.diff.prob,newdata=data.frame(GPA=3.08,GMAT=591))
student03<- predict(lda.diff.prob,newdata=data.frame(GPA=2.08,GMAT=641))
student04<- predict(lda.diff.prob,newdata=data.frame(GPA=3.22,GMAT=463))


## QDA
qda2<- qda(formula = De ~ .,
           data = admissions,
           prior = c(0.5,0.25,0.25))
qda2

## predictions for students with qda
predict(qda2,newdata=data.frame(GPA=3.14,GMAT=470))

# 3. Calculate the misclassfication rate
###Misclassification rate odf LDA rule
group2 <- predict(lda.diff.prob, admissions, method = 'plug-in')$class
table(group2, admissions$De)

#cross-validation rate of LDA diff prob rule:
correct <- rep(0, times=nrow(admissions))
for (j in 1:nrow(admissions))
{mydis <-lda(grouping = admissions$De[-j],
             x= admissions[-j,1:2],
             prior = c(0.5,0.25,0.25))
mypred <-predict(mydis,newdata=admissions[j,1:2])$class
correct[j] <- (mypred == admissions$De[j])}
cv.misclass2 <- 1-mean(correct)
cv.misclass2 #the cross-validation misclasification rate for LDA here is: 

### differences of the result from problem1.
###?library(klaR)
partimat(De~.,data=admissions,method="lda") 

library(broom)
augment(lda.same.prob,admissions)

plot(lda.same.prob)
lda.same.prob[4]
plot(lda.diff.prob)



########################PROBLEM 3########################
## QDA same probabilities
qda <- qda(formula = De ~ .,
           data = admissions,
           prior = c(1,1,1)/3)
qda


## QDA diff probabilities
qda2<- qda(formula = De ~ .,
           data = admissions,
           prior = c(0.5,0.25,0.25))
qda2

## predictions for students with qda
predict(qda2,newdata=data.frame(GPA=3.14,GMAT=470))
## predictions for students with qda
predict(qda,newdata=data.frame(GPA=3.14,GMAT=470))

############################
###Misclassification rate of QDA same prob rule
group3 <- predict(qda, admissions, method = 'plug-in')$class
table(group3, admissions$De)

#cross-validation rate of qda rule:
correct3 <- rep(0, times=nrow(admissions))
for (j in 1:nrow(admissions))
{mydis3 <-qda(grouping = admissions$De[-j],
             x= admissions[-j,1:2],
             prior = c(1,1,1)/3)
mypred3 <-predict(mydis3,newdata=admissions[j,1:2])$class
correct3[j] <- (mypred3 == admissions$De[j])}
cv.misclass3 <- 1-mean(correct3)
cv.misclass3 #the cross-validation misclasification rate for QDA here is: 

####################
###Misclassification rate of QDA diff prob rule
group4 <- predict(qda2, admissions, method = 'plug-in')$class
table(group4, admissions$De)

#cross-validation rate of qda2 rule:
correct4 <- rep(0, times=nrow(admissions))
for (j in 1:nrow(admissions))
{mydis4 <- qda(grouping = admissions$De[-j],
              x= admissions[-j,1:2],
              prior = c(0.5,0.25,0.25))
mypred4 <-predict(mydis4,newdata=admissions[j,1:2])$class
correct4[j] <- (mypred4 == admissions$De[j])}
cv.misclass4 <- 1-mean(correct4)
cv.misclass4 #the cross-validation misclasification rate for LDA here is: 


library(klaR)
####lda plot
partimat(formula = De ~ GPA + GMAT, data = admissions,method="lda", main = "Results of Linear classifications")

####qda plot
partimat(formula = De ~ GPA + GMAT, data = admissions,method="qda", main = "Results of quadratic classifications")

     