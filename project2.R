####################PART 1###########################################
##reading the data
test_score_Data <- read.table(file = "C://Users/erili/Desktop/WINTER 2019/DA410-MULTIVARIATE-CHENG/testscoredata.txt",
                              header = TRUE, sep ="")
attach(test_score_Data)
test_scores.noIDs <- test_score_Data[,-1]  # removing the ID column before doing the analysis

#######Use Hotelling's T^2 
#######Getting the mean for girls and boys
# number of observations for the 'girl' group and the 'boy' group:
girl <- length(sex[sex =='girl'])
boy <- length(sex[sex=='boy'])

# Splitting the data matrix (while removing the 3th column, sex)
# into two subsets, one for old and another for young:
x1 <- test_scores.noIDs[sex=='girl', -3]
x2 <- test_scores.noIDs[sex=='boy', -3]
my.q <- ncol(x1)

# Sample mean vectors for the 'girl' group and the 'boy' group:
m1 <- apply(x1, 2, mean)
m2 <- apply(x2, 2, mean)

# "pooled" sample covariance matrix:
S123 <- ((girl-1)*var(x1)+(boy-1)*var(x2))/(girl+boy-2)

# Hotelling T^2, the F-statistic, and the P-value:
T2 <- ((girl*boy)/(girl+boy))* (t(m1-m2) %*% solve(S123) %*% (m1-m2) )  # code on p. 140 of book is wrong here!
Fstat <- ((girl+boy-my.q-1)*T2)/((girl+boy-2)*my.q)
pvalue <- 1-pf(Fstat, my.q, girl+boy-my.q-1)

print(paste("Hotelling T^2 =", round(T2,4), 
            "F=", round(Fstat,4), "P-value =", round(pvalue,4) ))

##############################################
############################################

# A simpler approach (in terms of code):

summary(manova(cbind(math,reading) ~ sex),test="Hotelling")



#################PART 2###############################

sport <- as.factor( c('B','B','B','B','B','T','T','T','T','S','S','S','S','S','S'))
height <- c(66,65,68,64,67,63,61,62,60,62,65,63,62,63.5,66)
jump <-c(27,29,26,29,29,23,26,23,26,23,21,21,23,22,21.5)

athletes <- data.frame(sport,height,jump)
athletes
attach(athletes)

#(a) Conduct the MANOVA F-test using Wilks' Lambda to test for a difference in (height, jump) mean vectors across the three sports.######

s.manova <- manova(cbind(height, jump) ~ sport)
s.manova
summary(s.manova,test="Wilks")

#(b) Check to see whether the assumptions of your test are met.  Do you believe your inference is valid?###
  
# Checking model assumption of normality:
###############
chisplot <- function(x) {
  if (!is.matrix(x)) stop("x is not a matrix")
  ### determine dimensions
  n <- nrow(x)
  p <- ncol(x)
  xbar <- apply(x, 2, mean)
  S <- var(x)
  S <- solve(S)
  index <- (1:n)/(n+1)
  xcent <- t(t(x) - xbar)
  di <- apply(xcent, 1, function(x,S) x %*% S %*% x,S)
  quant <- qchisq(index,p)
  plot(quant, sort(di), ylab = "Ordered distances",
       xlab = "Chi-square quantile", lwd=2,pch=1)
}
###############
chisplot(residuals(s.manova))
# No strong evidence against normality -- we are safe.

############## checking for homogenity of variances
library(car)
leveneTest(jump ,sport, data= athletes)
leveneTest(height ,sport, data= athletes)


#####(c) Examine the sample mean vectors for each group.  Informally comment on the differences among the groups in terms of the specific variables.
# Examining the sample covariance matrices for each group:
by(athletes[,-1], sport, var)
# Is there evidence that the covariance matrices are significantly different across the 3 groups?
  ##### Differences in Means for doing Bonferroni multiple comparisons:
  means.by.grps <- cbind(tapply(height,sport,mean),
                         tapply(jump,sport,mean) )
means.by.grps

## The matrix E:
my.n <- nrow(athletes[,-1])
E <- (my.n - 1)*var(residuals(s.manova))
E


