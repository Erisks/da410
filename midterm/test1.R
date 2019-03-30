
R <- length(goods[goods ==1])
consumer

producer <- length(goods[goods==2])
producer

consumer_data <- econ[goods==1, -1]
producer_data <- econ[goods==2, -1]

my.q <- ncol(consumer_data)

#mean vector for consumer and producer
m_consumer <- apply(consumer_data, 2, mean)
m_producer <- apply(producer_data, 2, mean)

# "pooled" sample covariance matrix:
S.econ<- ((consumer-1)*var(consumer_data)+(producer-1)*var(producer_data))/(consumer+producer-2)
S.econ





# Hotelling T^2, the F-statistic, and the P-value:
Hotelling <- ((consumer*producer)/(consumer+producer))* (t(m_consumer-m_producer) %*% solve(S.econ) %*% (m_consumer-m_producer) )  

Fstat <- ((consumer+producer-my.q-1)*Hotelling)/((consumer+producer-2)*my.q)

pvalue <- 1-pf(Fstat, my.q, consumer+producer-my.q-1)

print(paste("Hotelling T^2 =", round(Hotelling,4), "F=", round(Fstat,4), "P-value =", round(pvalue,4) ))




library(lattice)
res <- histogram(y1 ~ y2 | y3*y2, data=blood_d,
                 main="Histograms per group")
print(res)

library(tidyverse)
library(dplyr)
library(ggplot2)
blood_d%>%  ggplot(aes(x= y1, fill=blood_reagent))+     geom_histogram(binwidth = 1) 
blood_d%>%  ggplot(aes(x= y2, fill=blood_reagent))+     geom_histogram(binwidth = 1) 
blood_d%>%  ggplot(aes(x= y3, fill=blood_reagent))+     geom_histogram(binwidth = 1) 
blood_d%>%  ggplot(aes(x= y4, fill=blood_reagent))+     geom_histogram(binwidth = 1) 

hist(y1)
hist(y2)
hist(y3)

blood_d%>%     ggplot(aes(x=reagent1, fill=blood_reagent))+     geom_density(alpha=0.5)

ggplot(data = blod_data, x=y1, y=rea)


library(ggplot2)
ggplot(data=blood_d(blood_reagent), aes(x=y1, y=y2, col=y3, size=y4)) + geom_point()
#loading the data
blood_d <- read.table("~/Desktop/WINTER 2019/DA410-MULTIVARIATE-CHENG/multivariate_analysis - 3rd Ed/multivariate_analysis - 3rd Ed/REAGENT_DATA.txt", col.names = c('reagent', 'subject', 'y1', 'y2', 'y3'))

reagent <- as.factor(blood$reagent)

str(blood_d) # the data contains 5 variables and 80 observations
reagent # different types of regeant 

#conducting manova test to see if there are significant differences in the group mean vectors
R.manova <- manova(cbind(blood$y1, blood$y2, blood$y3) ~ reagent, 
                   data = blood_d)

summary(R.manova)
# comparison of the 4 tests

blood_reagent <- as.factor(blood_d$reagent)

str(blood_d) # the data contains 5 variables and 80 observations
blood_reagent # different types of regeant 

#conducting manova test to see if there are significant differences in the group mean vectors
Reagent.manova <- manova(cbind(blood_d$y1, blood_d$y2, blood_d$y3) ~ blood_reagent, 
                         data = blood_d)

summary(Reagent.manova)
#### reagents data testing for homogenity of variances 
homogenity <-bartlett.test(y1 ~ blood_reagent, data = blood_d)
homogenity
bartlett.test(y2 ~ blood_reagent, data=blood_d)

bartlett.test(y3 ~ blood_reagent, data=blood_d)

fligner.test(y1 ~ interaction(y2,y3,blood_reagent), data=blood_d)

class(blood_reagent)


#######Getting the mean for all individual methods and all 3 combined
method1_bar <- colMeans(blood_d[1:20,3:5], blood$reagent)

method2_bar <- colMeans(blood_d[21:40,3:5], blood$reagent)

method3_bar <- colMeans(blood_d[41:60,3:5], blood$reagent)

method4_bar <- colMeans(blood_d[61:80,3:5], blood$reagent)

method_all_bar <- (method1_bar + method2_bar + method3_bar + method4_bar)/4

#######Difference between each method bars and all bars combined.
method1_bar_diff <- method1_bar - method_all_bar

method2_bar_diff <- method2_bar - method_all_bar

method3_bar_diff <- method3_bar - method_all_bar

method4_bar_diff <- method4_bar - method_all_bar

H <- 20 * unname(method1_bar_diff%*%t(method1_bar_diff) + method2_bar_diff%*%t(method2_bar_diff) + 
                   method3_bar_diff%*%t(method3_bar_diff) + method4_bar_diff%*%t(method4_bar_diff))

H

######computes within matrices

"Compute.Within.Matrix" <- function(data, mean){
  ret <-matrix(as.numeric(0), nrow = 3, ncol = 3)
  for(i in 1:20){
    diff <- as.numeric(unname(data[i,] - mean))
    ret <- ret + diff%*%t(diff) 
  }
  return (ret)
}

E <- Compute.Within.Matrix(reagent1, method1_bar) + Compute.Within.Matrix(reagent2, method2_bar) +
  Compute.Within.Matrix(reagent3, method3_bar) + Compute.Within.Matrix(reagent4, method4_bar)

E
# wilks
Lambda <- det(E) / det(E+H)
Lambda

## roy 
lambda_1 <- eigen(solve(E)%*%H)$values[1]
theta <- lambda_1/(1 + lambda_1)
theta

### degrees of freedom
k <- length(unique(blood_d$reagent))
p <- length(blood_d[,3:5])
vh <- k - 1
ve <- dim(blood_d)[1] - k

t <- sqrt((p^2 * vh^2 - 4) / (p^2 + vh^2 -5))

df1 <- p * vh
df2 <- (ve + vh - .5 * (p + vh + 1)) * t - .5 * (p * vh - 2)

f <- (1 - (det(E) / det(E + H))^(1/t)) / (det(E) / det(E + H))^(1/t) * df2 / df1

f

# comparison of the 4 tests
#pillai
summary(Reagent.manova, test='Pillai')

#roy
summary(Reagent.manova, test='Roy')
#Roy output conversion -> (Roy output)/(1+Roy output)
roy <-0.1433/(1+0.1433)
roy

#wilks
summary(Reagent.manova, test='Wilks')

#Hotelling-Lawley
summary(Reagent.manova, test='Hotelling-Lawley')


##The test statistics rely on the error E and hypothesis H matrices.
n <- dim(blood_d[1]) / length(unique(blood_reagent))
total.means <- colMeans(blood_d[,3:5])  #variable means 

reagent.group <- split(blood_d[,3:5], blood_d$reagent)  #each reagent patients data

#mean by groups: colmeans for each variable in each reagent group 
reagent.means <- sapply(reagent.group, function(x) 
{
  apply(x, 2, mean)
}, simplify = 'data.frame')


#pillai
summary(Reagent.manova, test='Pillai')

#roy
summary(Reagent.manova, test='Roy')
#Roy output conversion -> (Roy output)/(1+Roy output)
roy <-0.1433/(1+0.1433)

#wilks
summary(Reagent.manova, test='Wilks')

#Hotelling-Lawley
summary(Reagent.manova, test='Hotelling-Lawley')

summary(Reagent.manova,
        test = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"),
        intercept = FALSE)


##The test statistics rely on the error E and hypothesis H matrices.
n <- dim(blood_d[1]) / length(unique(blood$reagent))
total.means <- colMeans(blood_d[,3:5])  #variable means 

reagent.group <- split(blood_d[,3:5], blood$reagent)  #each reagent patients data

#mean by groups: colmeans for each variable in each reagent group 
reagent.means <- sapply(reagent.group, function(x) 
  {
  apply(x, 2, mean)
}, simplify = 'data.frame')


