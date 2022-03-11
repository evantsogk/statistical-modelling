library(data.table)

# load data
asfalies <- fread("asfalies.txt") 

# fit poisson model
model <- glm(y ~ agecat + factor(cartype) + district + offset(log(n)), family=poisson, data = asfalies)
summary(model)

# deviance (compare with null model)
fit.null <- glm(y ~ 1, family=poisson, data = asfalies)
anova(fit.null, model, test='Chisq')

# deviance (variable importance)
anova(model, test="Chisq")

# deviance (model importance)
1 - pchisq(model$deviance, model$df.residual)

# backward elimination
model_back <- step(model, direction="backward", test="Chisq")
summary(model_back)

# confidence intervals
confint.default(model)
exp(confint.default(model))

# pearson residuals
r.pears <- residuals(model, type = "pearson")
r.pears
qqnorm(r.pears,pch=19)
qqline(r.pears)

# deviance residuals vs fitted values
res.dev <- residuals(model)
r.dev
plot(fitted.values(model), res.dev,xlab='fitted values', ylab='Deviance residuals')
abline(h=0)

# outliers
par(mfrow = c(2,2))

plot(hatvalues(model),pch=19)
hatvalues(model)

plot(cooks.distance(model),pch=19)
cooks.distance(model)

plot(rstudent(model),pch=19)
rstudent(model)



