train = (all$train==1)  # create logical train variable
table(train)

x = model.matrix(logtarg ~ ., all[, c(3, 4:38)])
dim(x) # notice that we have a problem in that all cases with missing logtarg are dropped
x = model.matrix(id ~ ., all[, c(1, 4:38)]) # as a fix, use a y without missing values.
dim(x) # now we have all 16,781 cases

# fit ridge model
fit.ridge = glmnet(x[train,], all$logtarg[train], alpha=0)  # here y=logtarg, using only training cases
plot(fit.ridge, xvar="lambda")
fit.cv = cv.glmnet(x[train,], all$logtarg[train], alpha=0) # find optimal lambda
plot(fit.cv)
fit.cv$lambda.min        # optimal value of lambda
yhat = predict(fit.ridge, s=fit.cv$lambda.min, newx=x[train,])  # find yhat for best model, train data
sqrt(mean((all$logtarg[train] - yhat)^2))      # compute test set MSE
yhat = predict(fit.ridge, s=fit.cv$lambda.min, newx=x[!train,])  # find yhat for best model, test data

write.csv(data.frame(id=all$id[!train], logtarg=yhat), "ridge.csv", row.names=F)

