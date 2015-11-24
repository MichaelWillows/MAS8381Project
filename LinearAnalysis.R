library(ElemStatLearn)
library(leaps)
library(glmnet)

##### Best Subset Selection #####
# Subset selection, we have 13 variables as predictors
regfit=regsubsets(Income~., marketing, nvmax=20)
summary(regfit)
#We get the up to 13 variable model. Each model is formed by the predictors that participated in fitting the regression
# and gave the lowest RSS.
#In order to select the best of the 13 models we have to check the Cp(lowest), the BIC(lowest) and the adjusted R^2(highest)
# they all adjust the training error, so we use the validation and cross validation that estimate the test error directly

# to see what we can use to compare:
reg.summary= summary(regfit)
names(reg.summary)

#to obtain the R^2
reg.summary$rsq
#and we see that the R^2 statistics increases fron 18,68% when only 1 variable is included in the model to 44.57%, which means that  the model explains 45.19% of the variability of the response data around its mean when we use all of the predictors.

# we plot the RSS, adjusted r^2, Cp and BIC for all the models at once to decide which model to  select
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab = "Number of variables", ylab="RSS", type ="l")
#for the adjusted r^2 we want the highest value, this shows us the best model
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")
i=which.max(reg.summary$adjr2)
points(i,reg.summary$adjr2[i], col = "red",cex=2, pch=20)
#For the Cp and BIC we want the lowest values so:
plot(reg.summary$cp, xlab="Number of variables", ylab="Cp", type= "l")
j=which.min(reg.summary$cp)
points(j, reg.summary$cp[i],col = "red",cex=2, pch=20)
plot(reg.summary$bic, xlab="Number of variables", ylab="BIC", type ="l")
k=which.min(reg.summary$bic)
points(k, reg.summary$bic[k],col = "red",cex=2, pch=20)
par(mfrow=c(1,1))
#We present BIC here as a selection of criteria but they all adjust the training error so we are going to use
#the cross validation that estimates the test error directly
#If we use BIC as the selection criteria, it is suggested that we use a subset of 11 predictors that are believed to be related to the response as BIC reaches its lowest value when it uses three predictors.
#To see which three predictors we plot in R:
plot(regfit, scale="bic")

#Cross validation
#We will try to choose among the models of different sizes using cross validation,so we will perform the subset selection within each of the k training sets.
###cross validation
predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

k = 10
p = 42
folds = sample(rep(1:k, length = nrow(marketing)))
cv.errors = matrix(NA, k, p)
for (i in 1:k) {
  best.fit = regsubsets(Income ~ ., data =marketing[folds != i, ], nvmax = p)
  for (j in 1:p) {
    pred = predict(best.fit,marketing[folds == i, ], id = j)
    cv.errors[i, j] = mean((marketing$Income[folds == i] - pred)^2)
  }
}
rmse.cv = sqrt(apply(cv.errors, 2, mean))
plot(rmse.cv, pch = 19, type = "b")

vmax=which.min(rmse.cv) 
reg.best=regsubsets(Income~.,data = marketing,nvmax=13)
coef(best.fit,13)


######
####Lasso
#we define x and y
set.seed(1)
x=model.matrix(Income~.,marketing)[,-1] 
y=marketing$Income
#we set the training and the test sets
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

#we fit the lasso to the training data
grid=10^seq(10,-5,length=100)
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
#cross validation
cvout=cv.glmnet(x[train,],y[train],alpha=1)
plot(cvout)
bestlam=cvout$lambda.min
bestlam
pred=predict(lasso.mod,model.matrix(Income~.,data=marketing[test,])[,-1],s=bestlam)
mean((y[test]-pred)^2)
#Estimate and print the coefficients
lasso.coef=predict(cvout ,type="coefficients",s=bestlam)[1:43,]
lasso.coef

#lasso Darren
y=marketing[,1]
grid=10^seq(10,-5,length=100)
xmat=model.matrix(Income~.,data=marketing)[,-1]
cor(xmat)
mod=glmnet(xmat,y,alpha=1,lambda=grid) 
plot(mod)
plot(cvout)
bestlam=0.3
lasso.coef=predict(mod,type="coefficients",s=bestlam)
lasso.coef

#Rigde Darren
# Ridge
mod=glmnet(xmat,y,alpha=0,lambda=grid)
plot(mod)
cvout=cv.glmnet(xmat,y,alpha=0)
plot(cvout)
bestlam=cvout$lambda.min
bestlam
ridge.coef=predict(mod,type="coefficients",s=bestlam)
ridge.coef