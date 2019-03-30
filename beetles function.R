beetles <- read.table(file = "C://Users/erili/Desktop/WINTER 2019/DA410-MULTIVARIATE-CHENG/multivariate_analysis - 3rd Ed/multivariate_analysis - 3rd Ed/T5_5_FBEETLES.DAT", col.names = c("Exp.Number", "species","y1", "y2", "y3", "y4"))
beetles

root<- beetles[,-1]
attach(root)
root.group <- split(root[,2:5], root$species)

root.means <- sapply(root.group, function(x) {
  apply(x, 2, mean)
}, simplify = 'data.frame')

E = matrix(data = 0, nrow = 4, ncol = 4)
for (i in 1:dim(E)[1]) {
  for (j in 1:i) {
    b <- c() 
    for (k in root.group) {
      a <- sum((k[,i] - mean(k[,i])) * (k[,j] - mean(k[,j])))
      b <- append(b, a)
    }
    E[i,j] <- sum(b)
    E[j,i] <- sum(b)
  }
}

N <- dim(root)[1]
k <- length(unique(root$species))
sp1 <- E / (N - k)
sp1

li.y <- apply(root[,2:5], 1, function(y) {
  sapply(root.group, function(x) {
    y.bar <- as.numeric(apply(x, 2, mean))
    y.bar %*% solve(sp1) %*% y - .5 * y.bar %*% solve(sp1) %*% y.bar
  }, simplify = 'data.frame')
})

root.prediction <- apply(t(li.y), 1, function(x) {
  which(x==max(x))
})
y.bar
li.y

root.prediction

root$species

table(root$species, root.prediction, dnn = c('Actual Group','Predicted Group'))

error_rate = 1/sum(root.prediction == root$species)
error_rate

#classification with LDA
library(MASS)

root.lda <- lda(species ~ ., data = root)
lda.pred <- predict(root.lda)$class
table(root$species, lda.pred, dnn = c('Actual Group','Predicted Group'))

### KNN
library(class)


