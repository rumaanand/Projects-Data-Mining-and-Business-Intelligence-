library(glmnet); library(car)
# section 6.8, page 263 JWHT
college = read.csv("http://www-bcf.usc.edu/~gareth/ISL/College.csv")
head(college)
row.names(college) = college$X
college$X=NULL
set.seed(12345)
train = runif(nrow(college))<.5    # pick train/test split
table(train)  # Question 2
hist(college$Apps)  # Question 3

# Question 4
fit = lm(Apps ~ ., college, subset=train)
plot(fit, pch=16)

# Question 5
# I am overwriting a variable, which is not good practice, but it simplifies the code below substantially
college$Apps = sqrt(college$Apps)  
#college$Apps = log(college$Apps^2)  
hist(college$Apps)

# Question 6: full model
fit = lm(Apps ~ ., college, subset=train)
plot(fit, which=1)
yhat = predict(fit, college[!train,])
mean((college$Apps[!train] - yhat)^2)       # compute test set MSE
summary(fit)
vif(fit)

# Question 7: stepwise model
fit2 = step(fit)
yhat = predict(fit2, college[!train,])
mean((college$Apps[!train] - yhat)^2)       # compute test set MSE
summary(fit)

# Question 8: ridge model
x = model.matrix(Apps ~ ., college)
fit.ridge = glmnet(x[train,], college$Apps[train], alpha=0)
plot(fit.ridge, xvar="lambda")
fit.cv = cv.glmnet(x[train,], college$Apps[train], alpha=0) # find optimal lambda
fit.cv$lambda.min        # optimal value of lambda
plot(fit.cv)          # plot MSE vs. log(lambda)
yhat = predict(fit.ridge, s=fit.cv$lambda.min, newx=x[!train,])  # find yhat for best model
mean((college$Apps[!train] - yhat)^2)      # compute test set MSE

# Question 9: lasso model
fit.lasso = glmnet(x[train,], college$Apps[train], alpha=1)
plot(fit.lasso, xvar="lambda")
fit.cv = cv.glmnet(x[train,], college$Apps[train], alpha=1)
yhat = predict(fit.lasso, s=fit.cv$lambda.min, newx=x[!train,])
mean((college$Apps[!train] - yhat)^2)       # compute test set MSE

# Question 10: improved model with transformations
plot(college[train,-1], pch=16, cex=.5)
par(mfrow=c(2,4))
for(i in 3:10) plot(college[,i], college$Apps, pch=16, cex=.5, main=names(college)[i])
for(i in 11:18) plot(college[,i], college$Apps, pch=16, cex=.5, main=names(college)[i])
par(mfrow=c(1,1))
plot(Apps ~ log(Accept), college, pch=16)
plot(Apps ~ log(Enroll), college, pch=16)
plot(Apps ~ log(F.Undergrad), college, pch=16)

# forward stepwise model
fit= lm(Apps ~ 1, college, subset=train)
fit2 = step(fit, scope=~Private+Accept+sqrt(Accept)+log(Accept)+Enroll+sqrt(Enroll)+log(Enroll)+Top10perc+Top25perc+F.Undergrad+sqrt(F.Undergrad)+log(F.Undergrad)+P.Undergrad+sqrt(P.Undergrad)+log(P.Undergrad)+Outstate+Room.Board+Books+sqrt(Books)+log(Books)+Personal+PhD+I(PhD^2)+Terminal+S.F.Ratio+perc.alumni+Expend+Grad.Rate)
plot(fit2, pch=16, cex=.5)
yhat = predict(fit2, college[!train,])
mean((college$Apps[!train] - yhat)^2)

# ridge model
x = model.matrix(Apps ~ Private+Accept+sqrt(Accept)+log(Accept)+Enroll+sqrt(Enroll)+log(Enroll)+Top10perc+Top25perc+F.Undergrad+sqrt(F.Undergrad)+log(F.Undergrad)+P.Undergrad+sqrt(P.Undergrad)+log(P.Undergrad)+Outstate+Room.Board+Books+sqrt(Books)+log(Books)+Personal+PhD+Terminal+S.F.Ratio+perc.alumni+Expend+Grad.Rate, college)
fit.ridge = glmnet(x[train,], college$Apps[train], alpha=0)
round(fit.ridge$beta[,100], 4)
plot(fit.ridge, xvar="lambda")
fit.cv = cv.glmnet(x[train,], college$Apps[train], alpha=0)
yhat = predict(fit.ridge, s=fit.cv$lambda.min, newx=x[!train,])
mean((college$Apps[!train] - yhat)^2)

# lasso model
fit.lasso = glmnet(x[train,], college$Apps[train], alpha=1)
fit.cv = cv.glmnet(x[train,], college$Apps[train], alpha=1)
plot(fit.lasso, xvar="lambda"); abline(v=log(fit.cv$lambda.min))
predict(fit.lasso, s=fit.cv$lambda.min, type="coefficients")[1:29,]
yhat = predict(fit.lasso, s=fit.cv$lambda.min, newx=x[!train,])
mean((college$Apps[!train] - yhat)^2)
