# STEP : Data Setup
rm(list=ls()) # to remove all objects from a specified environment
cat("\f") # to clean console
library(readr)
# We selected COMIC.csv, BOOKMAN.csv, MONOTYPE.csv.
COMIC <- data.frame(read_csv("COMIC.csv"))
BOOKMAN <- data.frame(read_csv("BOOKMAN.csv"))
MONOTYPE <- data.frame(read_csv("MONOTYPE.csv"))
c.names = names(COMIC) # All three font files shares same categories of columns
c.discard = match(c("fontVariant", "m_label", "orientation", "m_top", "m_left", "originalH",
"originalW", "h", "w"),c.names) # discard 9 columns listed
# na.omit() function is to omit all rows that contain NA values
comic = na.omit(COMIC[,-c.discard])
bookman = na.omit(BOOKMAN[,-c.discard])
monotype = na.omit(MONOTYPE[,-c.discard])
# seperate into classes
CL1 = comic[comic[,match("strength", names(comic))] == 0.4 &
comic[,match("italic", names(comic))] == 0,]
CL2 = bookman[bookman[,match("strength", names(bookman))] == 0.4 &
bookman[,match("italic", names(bookman))] == 0,]
CL3 = monotype[bookman[,match("strength", names(monotype))] == 0.4 &
bookman[,match("italic", names(monotype))] == 0,]
#using 4bind to bind only classes of thoes rows
DATA = rbind(CL1, CL2, CL3)
str(DATA)
# STEP 1: Standardization
true_set = DATA[,1]
start_col = match("r0c0",names(DATA))
end_col = ncol(DATA)
X = DATA[,start_col: end_col]
m = sapply(X, mean) # same as apply(X, MARGIN = 2, mean)
s = sapply(X, sd)
SDATA = scale(X)
# STEP 2: Principal Component Analysis
R = cor(X)
# sigma = cov(X) # variance-covariance matrix "sigma"
# R=cov2cor(sigma) # either way to find correlation matrix
# all.equal(cor(X), cov2cor(sigma))
# STEP 3: Eigenvalues and Eigenvectors
D = eigen(R)$values
# or D = (summary(pca)$sdev)^2
Q = eigen(R)$vectors
#first 6 eigenvalues
D = eigen(R)$values; D[1:6]
# first 6x6 eigenvectors
Q = eigen(R)$vectors; Q[1:6,1:6]
layout(matrix(c(1:3), 1, 3))
# plot of the eigenvalues vs ordinal component r
plot(1:400, D, ylab = "eigenvalues L_r", xlab = "r", main = "Eigenvalues vs. r")
pca = princomp(X,cor=TRUE,scores=TRUE)
summary(pca)
plot(pca, type = "line")
# The variance explained by each principal component is obtained by squaring and then
# plot of the variance explained vs r
plot(1:400, D[1:400]/sum(D), xlab = "r", ylab = "variance explained", main = "Variance Explained vs. r")
# compute the proportion of variance explained by each principal component and then
# plot of the PVE(r) vs r
plot(1:400, cumsum(D)/sum(D), xlab = "r", ylab = "PVE(r)", main = "Percentage of Variance Explained:
PVE(r) vs. r", yaxt = "n")
axis(2, at = seq(0, 1, 0.05))
abline(a = 0.95, b = 0, col = "red")
#transpose of the matrix W^T, ==> principal components Y1(n)...Yr(n)
#Y1(n) = Q11 * X1(n) + Q21* X2(n) + … + Qr1 * Xr(n)
Qt = t(eigen(R)$vector)[1:6,1:6]; Qt
#-------------------------------------------------------------------------------------------
## Question 1 # K-means Clustering
names(k_out1)
# Reminder SDATA is our standardize 400 feature data set
set.seed(123)
k_out1 <- kmeans(SDATA,1,50)
k_out2 <- kmeans(SDATA,2,50)
k_out3 <- kmeans(SDATA,3,50)
k_out4 <- kmeans(SDATA,4,50)
k_out5 <- kmeans(SDATA,5,50)
k_out6 <- kmeans(SDATA,6,50)
k_out7 <- kmeans(SDATA,7,50)
k_out8 <- kmeans(SDATA,8,50)
k_out9 <- kmeans(SDATA,9,50)
k_out10 <- kmeans(SDATA,10,50)
###############################
SWS1 = sum(k_out1$withinss)
SWS2 = sum(k_out2$withinss)
SWS3 = sum(k_out3$withinss)
SWS4 = sum(k_out4$withinss)
SWS5 = sum(k_out5$withinss)
SWS6 = sum(k_out6$withinss)
SWS7 = sum(k_out7$withinss)
SWS8 = sum(k_out8$withinss)
SWS9 = sum(k_out9$withinss)
SWS10 = sum(k_out10$withinss)
Perf_1 <- 1-(SWS1/SWS1)
Perf_2 <- 1-(SWS2/SWS1)
Perf_3 <- 1-(SWS3/SWS1)
Perf_4 <- 1-(SWS4/SWS1)
Perf_5 <- 1-(SWS5/SWS1)
Perf_6 <- 1-(SWS6/SWS1)
Perf_7 <- 1-(SWS7/SWS1)
Perf_8 <- 1-(SWS8/SWS1)
Perf_9 <- 1-(SWS9/SWS1)
Perf_10 <- 1-(SWS10/SWS1)
k <- c(1:10)
Perf_k <- data.frame(Perf_1,Perf_2,Perf_3,Perf_4,Perf_5,
Perf_6,Perf_7,Perf_8,Perf_9,Perf_10)
plot(k,Perf_k, type = "b")
# SX = Y[,-1] # reminder, SX is the reduced model using PCA
k_out1 <- kmeans(SX,1,50)
k_out2 <- kmeans(SX,2,50)
k_out3 <- kmeans(SX,3,50)
k_out4 <- kmeans(SX,4,50)
k_out5 <- kmeans(SX,5,50)
k_out6 <- kmeans(SX,6,50)
k_out7 <- kmeans(SX,7,50)
k_out8 <- kmeans(SX,8,50)
k_out9 <- kmeans(SX,9,50)
k_out10 <- kmeans(SX,10,50)
SWS1 = sum(k_out1$withinss)
SWS2 = sum(k_out2$withinss)
SWS3 = sum(k_out3$withinss)
SWS4 = sum(k_out4$withinss)
SWS5 = sum(k_out5$withinss)
SWS6 = sum(k_out6$withinss)
SWS7 = sum(k_out7$withinss)
SWS8 = sum(k_out8$withinss)
SWS9 = sum(k_out9$withinss)
SWS10 = sum(k_out10$withinss)
Perf_1 <- 1-(SWS1/SWS1)
Perf_2 <- 1-(SWS2/SWS1)
Perf_3 <- 1-(SWS3/SWS1)
Perf_4 <- 1-(SWS4/SWS1)
Perf_5 <- 1-(SWS5/SWS1)
Perf_6 <- 1-(SWS6/SWS1)
Perf_7 <- 1-(SWS7/SWS1)
Perf_8 <- 1-(SWS8/SWS1)
Perf_9 <- 1-(SWS9/SWS1)
Perf_10 <- 1-(SWS10/SWS1)
k <- c(1:10)
Perf_k <- data.frame(Perf_1,Perf_2,Perf_3,Perf_4,Perf_5,
Perf_6,Perf_7,Perf_8,Perf_9,Perf_10)
plot(k,Perf_k, type = "b")
# Question 1 (continued): Elbow Method for choosing the best k
k1 = c(1:20)
plot(k1, sapply(k1, function(k) {kmeans(x = SX, centers = k, nstart = 50, iter.max = 50)$tot.withinss}),
type = "b", pch = 19, col = "black", frame = FALSE,
xlim = c(0, max(k1)),
xlab = "k", ylab = "Total Within-Clusters Sum of Squares",
main = "Elbow Method")
segments(min(k1), kmeans(x = SX, centers = min(k1), nstart = 50, iter.max = 50)$tot.withinss,
3, kmeans(x = SX, centers = 3, nstart = 50, iter.max = 50)$tot.withinss,
col = "red")
segments(3, kmeans(x = SX, centers = 3, nstart = 50, iter.max = 50)$tot.withinss,
max(k1), kmeans(x = SX, centers = max(k1), nstart = 50, iter.max = 50)$tot.withinss,
col = "red")
#------------------------------------------------------------------------------------------------------
# Question 2: K-Means Clustering
kbest = 3
set.seed(123)
KM = kmeans(SX, kbest, nstart = 50, iter.max = 50)
centers = KM$centers
size = KM$size
cluster = KM$cluster # a vector of nrow(DATA) x 1
c = centers[,1:3] #dimension of kbest x 3
print(c)
library(rgl)
plot3d(c, xlab = "x", ylab = "y", zlab = "z", col = "red")
t = max(size)
v = SX[which(cluster == which(size == t)),1:3]
colors = c("red", "green", "blue")
colors = colors[as.numeric(as.factor(true_set))][which(cluster == which(size == t))]
plot3d(v, xlab = "x", ylab = "y", zlab = "z", col = colors)
#------------------------------------------------------------------------------------------------------
# Question 3: Gini Index
# class 1: BOOKMAN; class 2: COMIC; class3: MONOTYPE
s = function(j) {
s = length(cluster[cluster==j])
return(s)
}
f = function(c) { #class frequencies
f_class = s(c)/length(cluster)
return(f_class)
}
for (i in 1:kbest){ # gini indexes for each cluster
cat(paste0("gin(CLU_",i,") ="), f(i)*(1-f(i)), "\n")
}
gini = function(kbest = 3) { # Impurity IMP(kbest) for clustering CLU_1,...,CLU_k
gini = 0
for (i in 1:kbest){
gini = gini + f(i)*(1-f(i))
}
return(gini)
}
cat("Impurity IMP(Kbest) =", gini(kbest = kbest),"\n")
A = function(m, j) {
A = length(intersect(which(as.integer(as.factor(true_set))==m),which(cluster==j)))
return(A)
}
fm = function(j) {
rbind(A(m = 1, j)/s(j), A(m = 2, j)/s(j), A(m = 3, j)/s(j))
}
FREQ_j = c()
for (k in 1:kbest) {
FREQ_j = append(FREQ_j, fm(k))
}
FREQ_j = matrix(FREQ_j, nrow = 3)
print(FREQ_j) # frequency: dimension of number of classes * kbest
A_mj = c()
for (m in 1:3) {
for (j in 1: kbest) {
A_mj = append(A_mj, A(m, j))
}
}
A_mj = matrix(A_mj, nrow = 3, byrow = TRUE)
print(A_mj)
TOP = function(j) {
top = which(A_mj[,j]==max(A(1, j), A(2, j), A(3, j)))
return(top)
}
#------------------------------------------------------------------------------------------------------
# Question 4: Confusion matrix / Decision Tree (Continued...)
# Question 4: Decision Tree (Continued...)
j = function(n) { # standard output out$cluster of the kmeans function
j = cluster[n]
return(j)
}
Pred = function(n) {
Pred = TOP(j(n))
return(Pred)
}
pred = c()
for (i in 1:nrow(SX)) {
pred = append(pred, Pred(i), after = length(pred))
}
pred = replace(pred, which(pred==1), "BOOKMAN")
pred = replace(pred, which(pred==2), "COMIC")
pred = replace(pred, which(pred==3), "MONOTYPE")
table_matrix = table(true_set, predict = pred[1:nrow(SX)])
CONF = prop.table(table_matrix, margin = 1)
print(CONF)
#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------
layout(matrix(c(1), 1, 1))
## Decision Tree with party's package
library(party)
tree_original = ctree(as.factor(TRAINSET_TARGET)~., data = TRAINSET)
tree_cut = ctree(as.factor(TRAINSET_TARGET)~., data = TRAINSET,
# to make the tree smaller and less complicated by controlling some parameters
# mincriterion is the confidence level
# minsplit means the min sample size when the branch will split into two
controls = ctree_control(mincriterion = 0.99, minsplit = 500))
plot(tree_original)
plot(tree_cut)
# Predict
predict(tree_original, TESTSET)
## Decision Tree with rpart's package
library(rpart)
tree_1 = rpart(as.factor(TRAINSET_TARGET)~., TRAINSET, method = "class")
library(rpart.plot)
rpart.plot(tree_1)
# Predict
predict(tree_1, TESTSET) #probability of all in test dataset
# Misclassification error for train data
tab1 = table(TRAINSET_TARGET, trainPred = predict(tree_original))
print(tab1)
1-sum(diag(tab1))/sum(tab1)
# Misclassification error with test data
testPred = predict(tree_original, newdata = TESTSET)
tab2 = table(TESTSET_TARGET, testPred)
print(tab2)
1-sum(diag(tab2))/sum(tab2)
