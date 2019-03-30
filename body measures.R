##reading the data
body_measures <- read.table(file = "C://Users/erili/Desktop/WINTER 2019/DA410-MULTIVARIATE-CHENG/bodymeasgrps.txt",
                              header = TRUE, sep ="")
attach(body_measures)
bodym.noIDs <- body_measures[,-1]  # removing the ID column before doing the analysis



# number of observations for the 'young' group and the 'old' group:
l1 <- length(agegp[agegp=='young'])
l2 <- length(agegp[agegp=='old'])
# Splitting the data matrix (while removing the 9th column, agegp)
# into two subsets, one for old and another for young:
x1 <- bodym.noIDs[agegp=='young', -9]
x2 <- bodym.noIDs[agegp=='old', -9]
my.q <- ncol(x1)
# Sample mean vectors for the 'young' group and the 'old' group:
m1 <- apply(x1, 2, mean)
m2 <- apply(x2, 2, mean)