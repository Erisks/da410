# Manually input the data column-by-column.
y1 <- c(35,35,40,10,6,20,35,35,35,30)
y2 <- c(3.5,4.9,30.0,2.8,2.7,2.8,4.6,10.9,8.0,1.6)
y3 <- c(2.80,2.70,4.38,3.21,2.73,2.81,2.88,2.90,3.28,3.20)

# Concatenate the columns into a single data frame.
calciumdata <- data.frame(y1, y2, y3)

# Invoke cov() to compute the sample covariance matrix.
calciumdata.cov <- cov(calciumdata)
# Display the sample covariance matrix.
calciumdata.cov

# Invoke cor() to compute the sample correlation matrix
calciumdata.cor <- cor(calciumdata)
calciumdata.cor

#find sample mean of z and variance of z.
a <- matrix(c(3, -1, 2), nrow = 3)
ybar <- matrix(c(mean(y1), mean(y2), mean(y3)), nrow = 3)
zbar.alt <- t(a) %*% ybar
var.z.alt <- t(a) %*% calciumdata.cov %*% a
zbar.alt
var.z.alt

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
