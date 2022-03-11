library(data.table)
library(car)
library(hnp)
library(pROC)

# load data
leukaemia <- fread("leukaemia.txt") 

# fit logistic model
model <- glm(response ~ age + smear + infiltrate + index + blasts + temperature, family=binomial, data = leukaemia)
summary(model)

# backward elimination
model_backw <- step(model, direction="backward", test="Chisq")
summary(model_backw)

# compare the two models with deviance
anova(model_backw, model, test="Chisq")

# current best model
model2 <- model_backw

# partial residual plots
par(mfrow=c(2, 2))
crPlot(model2, variable='age', pch=19)
crPlot(model2, variable='infiltrate', pch=19)
crPlot(model2, variable='index', pch=19)
crPlot(model2, variable='temperature', pch=19)

# half-normal plot of deviance residuals
hnp(model2, pch=19)

# outliers
par(mfrow = c(2,2))

plot(hatvalues(model2),pch=19)
hatvalues(model2)

plot(cooks.distance(model2),pch=19)
cooks.distance(model2)

plot(rstudent(model2),pch=19)
rstudent(model2)

# confidence intervals
confint.default(model2)
exp(confint.default(model2))

# ROC/AUC
roc(leukaemia$response, fitted.values(model2), smooth=TRUE, plot=TRUE)

