##################### PART 1 #####################
##reading the data
airpol.full <- read.delim(file = "C://Users/erili/Desktop/WINTER 2019/DA410-MULTIVARIATE-CHENG/airpoll.txt", header = TRUE,
                  sep =" ")

## extracting first 16 observations
city.names <- as.character(airpol.full[1:16,1])
airpol.data.sub <- airpol.full[1:16,2:8]
airpol.data.sub ## display the subset with 16 observations 
view(airpol.data.sub)

##################### PART 2 #####################
#####1########
# Invoke cov() to compute the sample covariance matrix.
airpol.data.sub.cov <- cov(airpol.data.sub)
# Display the sample covariance matrix.
airpol.data.sub.cov 

# Invoke cor() to compute the sample correlation matrix
airpol.data.sub.cor <- cor(airpol.data.sub)
airpol.data.sub.cor

#######2##### calculate distance matrix #####
std <- sapply(airpol.data.sub, sd) # finding standard deviations of variables
airpol.data.sub.std <- sweep(airpol.data.sub,2,std,FUN="/") # dividing each variable by its standard deviation
dis <- dist(airpol.data.sub.std)
library(kineticF)
dis.matrix <- dist2full(dis) # found distance matrix
round(dis.matrix,digits=2)

## ploting cluster dendrogram
hc <-hclust(dis)
plot(hc,labels = city.names, main = "Air Pollution ")

############3#################
####Using Everitt's chisplot function to check multivariate normality of entire data set:

#Copy the chisplot function into R
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


chisplot(as.matrix(airpol.data.sub))#### Using chisplot function 


colnames(dis.matrix)<-city.names
row.names(dis.matrix)<-city.names
dis.matrix <- as.matrix(dis)
dis.matrix
plot(hclust(dis))

