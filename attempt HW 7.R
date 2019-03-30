PROBE <- read.table("~/Desktop/WINTER 2019/DA410-MULTIVARIATE-CHENG/multivariate_analysis - 3rd Ed/multivariate_analysis - 3rd Ed/T3_6_PROBE.DAT", quote="\"", comment.char="", col.names= c("Subj.Numb", "y1","y2","y3","y4","y5"))
Prob_word <- PROBE[, 2:6]
Prob_subject <- PROBE[, 1]

#Use R
Prob_word.scaled <- scale(Prob_word, center = TRUE, scale = TRUE)
# 1. Correlation matrix
cor.m <- cor(Prob_word.scaled)
round(cor.m, 2)
# 2. Calculate eigenvectors/eigenvalues
res.eign <- eigen(cor.m)
res.eign
# 3. Calculate Propotion
res.eign$values/(sum(res.eign$values))

#Use S
# 1. Covariance matrix
cov.m <- cov(Prob_word.scaled)
cov.m
# 2. Calculate eigenvectors/eigenvalues
eigen(cov.m)
# 3. Calculate Propotion
prop.variance <-res.eign$values/(sum(res.eign$values))
prop.variance

cum.variance <-cumsum(prop.variance)
cum.variance


