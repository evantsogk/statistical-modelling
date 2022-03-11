library(data.table)
library(ggplot2)
library(corrplot)
library(car)
library(olsrr)

# load data
vehicles <- fread("vehicles.txt") 
vehicles <- vehicles[, -"car"] # we don't need the car type variable

# fit linear model
model <- lm(mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb, data = vehicles)
model
summary(model)

# correlation
corr_mat <- cor(vehicles)
corr_mat
corrplot(corr_mat, type = "upper", tl.col = "black")

# VIF
vif(model)

# normality of residuals
qqnorm(model$res, pch=19, ylab = "Regular Residuals")
qqline(model$res)
qqnorm(rstandard(model), pch=19, ylab = "Standardized Residuals")
qqline(rstandard(model))
qqnorm(rstudent(model), pch=19, ylab = "Deleted Residuals")
qqline(rstudent(model))

# residuals vs fitted values
plot(model, which=1, pch=19) # regular
plot(model, which=3, pch=19) # standardized

# outliers
plot(hatvalues(model),pch=19)
hatvalues(model)

plot(cooks.distance(model),pch=19)
cooks.distance(model)

plot(dffits(model),pch=19)
dffits(model)

par(mfrow = c(2,2))
plot(dfbetas(model)[, 'cyl'], pch=19)
plot(dfbetas(model)[, 'hp'], pch=19)
plot(dfbetas(model)[, 'drat'], pch=19)
plot(dfbetas(model)[, 'qsec'], pch=19)
par(mfrow = c(2,2))
plot(dfbetas(model)[, 'vs'], pch=19)
plot(dfbetas(model)[, 'gear'], pch=19)
plot(dfbetas(model)[, 'carb'], pch=19)


# forward selection
model1 <- step(lm(vehicles$mpg ~ 1), scope=(~ vehicles$cyl + vehicles$disp + vehicles$hp + vehicles$drat + vehicles$wt + vehicles$qsec + vehicles$vs + vehicles$am + vehicles$gear + vehicles$carb), direction='forward', test="F")
summary(model1)
AIC(model1)

# backward elimination
model2 <- step(model,direction="backward",test="F")
summary(model2)
AIC(model2)

# stepwise selection
model3 <- step(lm(vehicles$mpg ~ 1), scope=(~ vehicles$cyl + vehicles$disp + vehicles$hp + vehicles$drat + vehicles$wt + vehicles$qsec + vehicles$vs + vehicles$am + vehicles$gear + vehicles$carb), direction='both', test="F")
summary(model3)
AIC(model3)

data.frame(ols_step_all_possible(model))

# best model
best_model <- model2

# normality of residuals
par(mfrow = c(1,2))
qqnorm(rstandard(best_model), pch=19, ylab = "Standardized Residuals")
qqline(rstandard(best_model))
qqnorm(rstudent(best_model), pch=19, ylab = "Deleted Residuals")
qqline(rstudent(best_model))

# residuals vs fitted values
par(mfrow = c(1,2))
plot(best_model, which=1, pch=19) # regular
plot(best_model, which=3, pch=19) # standardized

# added variable plots
par(mfrow = c(2,2))
avPlots(best_model, terms="wt", pch=19)
avPlots(best_model, terms="qsec", pch=19)
avPlots(best_model, terms="am", pch=19)

# partial residual plots
par(mfrow = c(2,2))
crPlots(best_model, terms="wt", pch=19)
crPlots(best_model, terms="qsec", pch=19)
crPlots(best_model, terms="am", pch=19)

# adjust
vehicles[, "wt_log"] <- log(vehicles[, "wt"])
model_new <- lm(mpg ~ wt_log + qsec, data = vehicles)
summary(model_new)
AIC(model_new)

best_model <- model_new
summary(best_model)

# outliers
par(mfrow = c(2,2))
plot(hatvalues(best_model),pch=19)
hatvalues(best_model)

plot(cooks.distance(best_model),pch=19)
cooks.distance(best_model)

plot(dffits(best_model),pch=19)
dffits(best_model)

par(mfrow = c(1,2))
plot(dfbetas(best_model)[, 'wt_log'], pch=19)
plot(dfbetas(best_model)[, 'qsec'], pch=19)

# confidence
confint(best_model, level=0.95)

# predict
newvalues <- data.frame(wt_log=0.5, qsec=18)
predict(best_model, newdata=newvalues, interval="confidence", level = 0.95)
predict(best_model, newdata=newvalues, interval="predict", level = 0.95)

