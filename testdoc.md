DA 410 Project 3
================
Erika Vargas
January 27, 2019

``` r
admissions <- read.table(file = "C://Users/erili/Desktop/WINTER 2019/DA410-MULTIVARIATE-CHENG/admission.csv",
                            header = TRUE, sep =",")
head(admissions)
```

    ##    GPA GMAT    De
    ## 1 2.96  596 admit
    ## 2 3.14  473 admit
    ## 3 3.22  482 admit
    ## 4 3.29  527 admit
    ## 5 3.69  505 admit
    ## 6 3.46  693 admit

Including Plots
---------------

You can also embed plots, for example:

``` r
plot(admissions$GPA,admissions$GMAT,col=admissions$De)
```

![](testdoc_files/figure-markdown_github/scatter%20plot%20with%20the%20current%20data-1.png)

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

problem 1
=========

``` r
#MASS library for linear discirminat analysis 
library(MASS)
```

    ## Warning: package 'MASS' was built under R version 3.5.2

``` r
# 2.Train model using LDA by setting admit/not-admit/border with the same probabilities.
# train model
lda.same.prob <- lda(formula = De ~ .,
         data = admissions,
         prior = c(1,1,1)/3)
lda.same.prob
```

    ## Call:
    ## lda(De ~ ., data = admissions, prior = c(1, 1, 1)/3)
    ## 
    ## Prior probabilities of groups:
    ##     admit    border  notadmit 
    ## 0.3333333 0.3333333 0.3333333 
    ## 
    ## Group means:
    ##               GPA     GMAT
    ## admit    3.403871 561.2258
    ## border   2.992692 446.2308
    ## notadmit 2.482500 447.0714
    ## 
    ## Coefficients of linear discriminants:
    ##              LD1         LD2
    ## GPA  5.017202736  1.85401003
    ## GMAT 0.008503148 -0.01448967
    ## 
    ## Proportion of trace:
    ##    LD1    LD2 
    ## 0.9644 0.0356

``` r
## predictions for students with lda
student1 <- predict(lda.same.prob,newdata=data.frame(GPA=3.14,GMAT=470))
student2 <- predict(lda.same.prob,newdata=data.frame(GPA=3.08,GMAT=591))
student3 <- predict(lda.same.prob,newdata=data.frame(GPA=2.08,GMAT=641))
student4 <- predict(lda.same.prob,newdata=data.frame(GPA=3.22,GMAT=463))
student1$class
```

    ## [1] border
    ## Levels: admit border notadmit

``` r
student2$class
```

    ## [1] admit
    ## Levels: admit border notadmit

``` r
student3$class
```

    ## [1] notadmit
    ## Levels: admit border notadmit

``` r
student4$class
```

    ## [1] border
    ## Levels: admit border notadmit

``` r
###Misclassification rate odf LDA rule
group <- predict(lda.same.prob, admissions, method = 'plug-in')$class
table(group, admissions$De)
```

    ##           
    ## group      admit border notadmit
    ##   admit       27      1        0
    ##   border       4     25        2
    ##   notadmit     0      0       26

``` r
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
```

    ## [1] 0.1058824

PROBLEM 2
=========

``` r
# Train model using LDA by setting  
#admit is 50% while probability of not admit is 25% and probability of border is 25%. probabilities.
# train model
lda.diff.prob <- lda(formula = De ~ .,
         data = admissions,
         prior = c(0.5,0.25,0.25))
lda.diff.prob
```

    ## Call:
    ## lda(De ~ ., data = admissions, prior = c(0.5, 0.25, 0.25))
    ## 
    ## Prior probabilities of groups:
    ##    admit   border notadmit 
    ##     0.50     0.25     0.25 
    ## 
    ## Group means:
    ##               GPA     GMAT
    ## admit    3.403871 561.2258
    ## border   2.992692 446.2308
    ## notadmit 2.482500 447.0714
    ## 
    ## Coefficients of linear discriminants:
    ##              LD1        LD2
    ## GPA  4.961868967  1.9973815
    ## GMAT 0.008915905 -0.0142394
    ## 
    ## Proportion of trace:
    ##    LD1    LD2 
    ## 0.9724 0.0276

``` r
## predictions for students qith lda
student01<- predict(lda.diff.prob,newdata=data.frame(GPA=3.14,GMAT=470))
student02<- predict(lda.diff.prob,newdata=data.frame(GPA=3.08,GMAT=591))
student03<- predict(lda.diff.prob,newdata=data.frame(GPA=2.08,GMAT=641))
student04<- predict(lda.diff.prob,newdata=data.frame(GPA=3.22,GMAT=463))

student01$class
```

    ## [1] border
    ## Levels: admit border notadmit

``` r
student02$class
```

    ## [1] admit
    ## Levels: admit border notadmit

``` r
student03$class
```

    ## [1] notadmit
    ## Levels: admit border notadmit

``` r
student04$class
```

    ## [1] border
    ## Levels: admit border notadmit

``` r
# 3. Calculate the misclassfication rate
###Misclassification rate odf LDA rule
group2 <- predict(lda.diff.prob, admissions, method = 'plug-in')$class
table(group2, admissions$De)
```

    ##           
    ## group2     admit border notadmit
    ##   admit       29      1        0
    ##   border       2     25        2
    ##   notadmit     0      0       26

``` r
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
```

    ## [1] 0.07058824

PROBLEM 3
=========

``` r
## QDA same probabilities
qda <- qda(formula = De ~ .,
           data = admissions,
           prior = c(1,1,1)/3)
qda
```

    ## Call:
    ## qda(De ~ ., data = admissions, prior = c(1, 1, 1)/3)
    ## 
    ## Prior probabilities of groups:
    ##     admit    border  notadmit 
    ## 0.3333333 0.3333333 0.3333333 
    ## 
    ## Group means:
    ##               GPA     GMAT
    ## admit    3.403871 561.2258
    ## border   2.992692 446.2308
    ## notadmit 2.482500 447.0714

``` r
## QDA diff probabilities
qda2<- qda(formula = De ~ .,
           data = admissions,
           prior = c(0.5,0.25,0.25))
qda2
```

    ## Call:
    ## qda(De ~ ., data = admissions, prior = c(0.5, 0.25, 0.25))
    ## 
    ## Prior probabilities of groups:
    ##    admit   border notadmit 
    ##     0.50     0.25     0.25 
    ## 
    ## Group means:
    ##               GPA     GMAT
    ## admit    3.403871 561.2258
    ## border   2.992692 446.2308
    ## notadmit 2.482500 447.0714

``` r
## predictions for students with qda
predict(qda2,newdata=data.frame(GPA=3.14,GMAT=470))
```

    ## $class
    ## [1] border
    ## Levels: admit border notadmit
    ## 
    ## $posterior
    ##       admit    border    notadmit
    ## 1 0.3858702 0.6125225 0.001607321

``` r
## predictions for students with qda
predict(qda,newdata=data.frame(GPA=3.14,GMAT=470))
```

    ## $class
    ## [1] border
    ## Levels: admit border notadmit
    ## 
    ## $posterior
    ##       admit    border    notadmit
    ## 1 0.2390577 0.7589507 0.001991564
