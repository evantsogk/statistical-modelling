library(data.table)

# load data
bacteria <- fread("bacteria.txt")

# scatter plot
plot(bacteria$X[group == 1], bacteria$Y[group == 1], main=expression(paste("Scatterplot of Y vs X")), 
     xlab="X",ylab="Y", col="blue", pch=19, xlim=c(0,16), ylim=c(10, 380))
points(bacteria$X[group == 2],bacteria$Y[group == 2],col="red",pch=15)
legend("topright", c("group 1", "group 2"), col=c("blue","red"), pch = c(19,15))

# log transformation
bacteria$log_Y <- log(bacteria$Y)
bacteria$Y
bacteria$log_Y

# sactter plot
plot(bacteria$X[group == 1], bacteria$log_Y[group == 1], main=expression(paste("Scatterplot of Y* vs X")), 
     xlab="X",ylab="Y*", col="blue", pch=19, xlim=c(0,16), ylim=c(2, 6.5))
points(bacteria$X[group == 2],bacteria$log_Y[group == 2],col="red",pch=15)
legend("topright", c("group 1", "group 2"), col=c("blue","red"), pch = c(19,15))

# create new variables
bacteria$x1 <- bacteria$X
bacteria$x2 <- bacteria$group - 1
bacteria$x3 <- bacteria$x1*bacteria$x2

# backward elimination
modela <- lm(log_Y ~ x1 + x2 + x3, data=bacteria)
summary(modela)
modelb <- lm(log_Y ~ x1 + x2, data=bacteria)
summary(modelb)
modelc <- lm(log_Y ~ x1, data=bacteria)
summary(modelc)

# fit lines
plot(bacteria$X[group == 1], bacteria$log_Y[group == 1], main=expression(paste("Scatterplot of Y* vs X")), 
     xlab="X",ylab="Y*", col="blue", pch=19, xlim=c(0,16), ylim=c(2, 6.5))
abline(lm(log_Y[group == 1] ~ X[group == 1], data=bacteria), col="blue")
points(bacteria$X[group == 2],bacteria$log_Y[group == 2],col="red",pch=15)
abline(lm(log_Y[group == 2] ~ X[group == 2], data=bacteria), col="red")
legend("topright", c("group 1", "group 2"), col=c("blue","red"), pch = c(19,15))