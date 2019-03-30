##### HW3#######
########6.27 (A)#####

###input the data
method1 <- matrix(c(5.4, 6.0, 6.3, 6.7, 5.2, 6.2, 6, 5.8, 6.1, 5.9, 6, 7, 4.8, 5, 4.9, 5, 5, 5.7, 5, 6.5, 
                    5.7, 6.1, 6.0, 6.6, 6, 6, 5.8, 6, 4, 5, 4, 5, 5.7, 5.4, 4.9,5, 5.6,5.2, 5.4, 5.8, 5.8, 
                    6.1,5.2, 6.4,5.3, 5.9, 5.8, 6), nrow = 12, ncol = 4, byrow = TRUE)
method1

method2 <-matrix(c(5, 5.3, 5.3, 6.5, 4.8, 4.9, 4.2, 5.6, 3.9,4.0,4.4, 5.0, 4.0, 5.1, 4.8, 5.8, 5.6, 5.4, 5.1,
                   6.2, 6.0, 5.5, 5.7, 6.0, 5.2, 4.8, 5.4, 6.0, 5.3, 5.1, 5.8, 6.4, 5.9, 6.1, 5.7, 6.0, 6.1, 6.0, 
                   6.1, 6.2, 6.2, 5.7, 5.9, 6.0, 5.1, 4.9, 5.3, 4.8), nrow = 12, ncol = 4, byrow = TRUE)
method2

method3 <-matrix(c(4.8, 5.0, 6.5, 7.0, 5.4, 5.0, 6.0, 6.4, 4.9, 5.1, 5.9, 6.5, 5.7, 5.2, 6.4, 6.4, 4.2, 4.6, 5.3, 
                   6.3, 6.0, 5.3, 5.8, 6.4, 5.1, 5.2, 6.2, 6.5, 4.8, 4.6, 5.7, 5.7, 5.3, 5.4, 6.8, 6.6, 4.6, 4.4, 5.7, 
                   5.6, 4.5, 4.0, 5.0, 5.9, 4.4, 4.2, 5.6, 5.5), nrow = 12, ncol = 4, byrow = TRUE)

method3

#######Getting the mean for all individual methods and all 3 combined
method1_bar <- colMeans(method1)

method2_bar <- colMeans(method2)

method3_bar <- colMeans(method3)

method_all_bar <- (method1_bar + method2_bar + method3_bar)/3

#######Difference between each method bars and all bars combined.
method1_bar_diff <- method1_bar - method_all_bar

method2_bar_diff <- method2_bar - method_all_bar

method3_bar_diff <- method3_bar - method_all_bar

H <- 12 * unname(method1_bar_diff%*%t(method1_bar_diff) + method2_bar_diff%*%t(method2_bar_diff) + 
                   method3_bar_diff%*%t(method3_bar_diff))

H

######computes within matrices

"Compute.Within.Matrix" <- function(data, mean){
  ret <-matrix(as.numeric(0), nrow = 4, ncol = 4)
  for(i in 1:12){
    diff <- as.numeric(unname(data[i,] - mean))
    ret <- ret + diff%*%t(diff) 
  }
  return (ret)
}

E <- Compute.Within.Matrix(method1, method1_bar) + Compute.Within.Matrix(method2, method2_bar) +
  Compute.Within.Matrix(method3, method3_bar)

E

######## FOUR MANOVA TESTS ########
##Wilks'Test Statistic

Lambda <- det(E) / det(E+H)
Lambda


##Pillai Statistic

install.packages("psych")
library(psych)

V.s <- tr(solve(E + H)%*%H)

V.s

##Lawley-Hotelling Statistic

U.s <- tr(solve(E)%*%H)

U.s


##Roy's Statistic

lambda_1 <- eigen(solve(E)%*%H)$values[1]
theta <- lambda_1/(1 + lambda_1)

theta

########6.28 (A)########


###loading the data 
beansData <- read.table("c:/Users/erili/Desktop/WINTER 2019/DA410-MULTIVARIATE-CHENG/snapbeans.txt", sep=" ", header = TRUE)
beansData 

S <-as.factor(S)
is.factor(S)
V <-as.factor(V)
is.factor(V)
#####MANOVA#####


s.manova <- manova(cbind(y1,y2,y3,y4) ~ beansData$S*beansData$V,data = beansData)

summary(s.manova,test="Wilks")

summary(s.manova,test="Roy")
#Roy output conversion -> (Roy output)/(1+Roy output)
#S
137.168/(1+137.168)
#V
11.445/(1+11.445)
#VS
2.649/(1+2.649)

summary(s.manova,test="Hotelling-Lawley")

summary(s.manova,test="Pillai")


