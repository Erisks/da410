

#loading the data 
beansData <- read.table("c:/Users/erili/Desktop/WINTER 2019/DA410-MULTIVARIATE-CHENG/snapbeans.txt", sep=" ", header = TRUE)
attach(beansData)
#####MANOVA#####

s.manova <- manova(cbind(y1,y2,y3,y4) ~ as.factor(S)*as.factor(V),data = beansData)
summary(s.manova,test="Wilks")

summary(s.manova,test="Roy")
#Roy output conversion -> (Roy output)/(1+Roy output)
#theta-S
137.168/(1+137.168)
#theta-V
11.445/(1+11.445)
#theta-VS
2.649/(1+2.649)

summary(s.manova,test="Hotelling-Lawley")

summary(s.manova,test="Pillai")
