## loading the data

CalciumData <- matrix(c(35,3.5,2.80,35,4.9,2.70,40,30.0,4.38,10,2.8,3.21,6,2.7,2.73,20,2.8,2.81,35,4.6,2.88,35,10.9,2.90,35,8.0,3.28,30,1.6,3.20),nrow = 10, ncol = 3, byrow = TRUE)
CalciumData

library(dplyr)
library(tidyr)

calcium <- as.data.frame(CalciumData) colnames(c("y1","y2","y3")) #convert the matrix to a data frame
calcium
#getting the sample mean vector
Y <-colMeans(calcium)
Y
#####3.10######
#a). getting the sample covariance matrix S
 s <- var(calcium)
round(var(calcium),digits = 2)

#c) getting the sample correlation matrix
R<- cor(calcium)
round(R,digits = 3)

#####
my.S <-var(calcium)
D.minus.12<-diag(1/sqrt(diag(my.S)))

myR <- D.minus.12 %*% my.S %*% D.minus.12
myR
my.S
cov2cor(my.S)
p<-my.S*my.S

#####3.14######
a<- matrix(c(3,-1,2), nrow = 3)

z <-crossprod(a,Y)
z

###?????samplevariance <- crossprod(a, s)
samplevariance


######3.21######
head_measures <- tempfile()
cat(file=head_measures, "
191 155 179 145 
195 149 201 152
181 149 201 152
183 153 188 149
176 144 171 142
208 157 192 152
189 150 190 149
197 159 189 152
188 152 197 159
192 150 187 151 
179 158 186 148
183 147 174 147
174 150 185 152
190 159 195 157
188 151 187 158
163 137 161 130 
195 155 183 158
186 153 173 148 
181 145 182 146
175 140 165 137
192 154 185 152
174 143 178 147
176 139 176 143
197 167 200 158
190 163 187 150
", sep=" ")

options(scipen=999) # suppressing scientific notation
h <- read.table(head_measures, header=FALSE, col.names=c("firsty1",
                                                            "firsty2", "secondx1", "secondx2"))
attach(h) #attaching the data frame

 #a) mean vector for all 4 variables
colMeans(h)

#b) covariance matrix for all four varaiables 
cov(h)


###building a graph matrix
# Packages
library(car)
library(RColorBrewer)

# Let's use the car dataset proposed by R
data=mtcars

# Make the plot
my_colors <- brewer.pal(nlevels(as.factor(data$Popden)), "Set2")
scatterplotMatrix(~Rainfall+NOX+|Popden, data=data , main="Scatter plot with Three Cylinder Options")


cylint <- as.integer(factor(mtcars$cyl))
pairs(~mpg+disp+hp+wt, data=mtcars, col=cylint, pch=cylint)
library(car)
scatterplotMatrix(~mpg+disp+hp+wt|cyl, data=mtcars)

######### Correlation Matrix Color Image (Heat Map) in R##############
cmat <- cor(mtcars)
p <- nrow(cmat)
library(RColorBrewer)
imagebar(1:p, 1:p, cmat[,p:1], axes=F, zlim=c(-1,1), xlab="", ylab="", col=brewer.pal(7, "RdBu"))
axis(1, 1:p, labels=rownames(cmat))
axis(2, p:1, labels=colnames(cmat))
for(k in 1:p) { for(j in 1:k) { if(j < k) text(j, p+1-k, labels=round(cmat[j,k],2), cex=0.75) } }



