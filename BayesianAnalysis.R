#Finding the item we are going to model, y, and on what, x.
y = marketing$Income
x = model.matrix(Income ~ ., data = marketing)

#Finding the number of parameters and number of observations
n = length(y)
p = ncol(x)

require(rjags)
#saturated mode
data = list(y = y, X = x, n = n, p = p)
init = list(tau = 1, beta = rep(0, p))
modelstring = "
model {
for (i in 1:n) {
mean[i] =  inprod(X[i,],beta)
y[i]~dnorm(mean[i],tau)
}
for (j in 1:p) {
beta[j]~dnorm(0,0.001)
}
tau~dgamma(1,0.001)
}
"
model = jags.model(textConnection(modelstring), data = data, inits = init)
update(model, n.iter = 1000)
output = coda.samples(model = model, variable.names = c("beta", "tau"), n.iter = 1000, thin = 1)
summary(output)
plot(output)


#Variable selection with random effects and prior inclusion 
data = list(y = y, X = x, n = nrow(x), p = ncol(x))
init = list(tau = 1, taub = 1, pind = 0.5, betaT = rep(0, ncol(x)), ind = rep(0, ncol(x)))
modelstring = "
model{
for(i in 1:n){
mean[i] = inprod(X[i,], beta)
y[i] ~ dnorm(mean[i],tau)
}
for (j in 1:p) {
ind[j] ~ dbern(pind)
betaT[j] ~ dnorm(0,taub)
beta[j] = ind[j]*betaT[j]
}
tau ~ dgamma(1, 0.001)
taub ~ dgamma(1, 0.001)
pind ~ dbeta(2,8)
}
"
model = jags.model(textConnection(modelstring), data = data, inits = init)
update(model, n.iter = 100)
output = coda.samples(model = model, variable.names = c("beta", "ind", "tau", "taub", "pind"),
                      n.iter = 10000, thin = 10)
summary(output)
plot(output)
autocorr.plot(output)

