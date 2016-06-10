model1<-lm(deaths ~ pop, data = mort)
#gleason model
model2<-lm(deaths ~ log(1+pop), data = mort)
#nonlinear Arrhenius model
model3 <- nls(deaths~b0*pop^b1,data=mort,start=list(b0=1,b1=1))
#Poisson and negative binomial models
model4<-glm(deaths~log(1+pop),data=mort,family=poisson)
library(MASS)
model5<-glm.nb(deaths~log(1+pop),data=mort)
sapply(list(model1,model2,model3,model4,model5),AIC)
sapply(list(model1,model2,model3,model4,model5),logLik)

#graph Poisson and NB together
plot(deaths~log(1+pop),data=mort)
coef(model4)
poisson.func<-function(x) exp(coef(model4)[1]+coef(model4)[2]*x)
coef(model5)
NB.func<-function(x) exp(coef(model5)[1]+coef(model5)[2]*x)
curve(poisson.func,add=T,col=2)
curve(NB.func,add=T,col=4,lty=2)

#transformed response models
model6<-lm(log(1+deaths)~log(1+pop),data=mort)
model7<-lm(sqrt(deaths)~log(1+pop),data=mort)

#log-likelihoods are not comparable
logLik(model5)
logLik(model6)
logLik(model7)

#function to obtain normal log-likelihood for untransformed response
norm.test <- function(model,y) {
  #t.y<-log(y)
  s <- sqrt(sum(residuals(model)^2)/length(residuals(model)))
  LL <- sum(log(dnorm(y,mean=predict(model),sd=s)))
  LL
}
model6a<-lm(deaths~log(1+pop),data=mort)
logLik(model6a)
norm.test(model6a,mort$deaths)

#function to obtain log-likelihood of y when fit normal model to log(y)
norm.log <- function(model,y) {
  t.y<-log(y)
  s <- sqrt(sum(residuals(model)^2)/length(residuals(model)))
  LL <- sum(log(dnorm(t.y,mean=predict(model),sd=s)*1/y))
  LL
}
norm.log(model6,mort$deaths)

#function to obtain log-likelihood of y when fit normal model to sqrt(y)
norm.sqrt <- function(model,y) {
  t.y<-sqrt(y)
  s <- sqrt(sum(residuals(model)^2)/length(residuals(model)))
  LL <- sum(log(dnorm(t.y,mean=predict(model),sd=s)*1/(2*sqrt(y))))
  LL
}
norm.sqrt(model7,mort$deaths)

#obtain log-likelihoods of all models
c(sapply(list(model1,model2,model3,model4,model5),logLik),norm.log(model6,mort$deaths),norm.sqrt(model7,mort$deaths))

#obtain AIC of all models
c(sapply(list(model1,model2,model3,model4,model5),AIC),-2*norm.log(model6,mort$deaths)+2*3,-2*norm.sqrt(model7,mort$deaths)+2*3)

plot(deaths~log(1+pop), data=mort)
curve(poisson.func, add=T, col='pink', lwd=2)
curve(NB.func, add=T, col=2, lty=2)
lognorm.median <- function(x) exp(coef(model6)[1]+coef(model6)[2]*x)
sqrtnorm.median <- function(x) (coef(model7)[1]+coef(model7)[2]*x)^2
curve(lognorm.median, add=T, col='grey60', lwd=2)
curve(sqrtnorm.median, add=T, lty=2)
legend('topleft', c('Poisson (mean)', 'negative binomial (mean)', 'lognormal (median)', 'sqrt-normal (median)'), col=c('pink',2,'grey60',1), lty=c(1,2,1,2), lwd=c(2,1,2,1), cex=.9, bty='n')

model.names <- paste('model', 1:7, sep='')
#obtain log-likelihoods of all models
model.LL <- c(sapply(list(model1, model2, model3, model4, model5), logLik), norm.log(model6, mort$deaths), norm.sqrt(model7, mort$deaths))
#obtain AIC of all models
model.AIC <- c(sapply(list(model1, model2, model3, model4, model5), AIC), -2*norm.log(model6,mort$deaths)+2*3, -2*norm.sqrt(model7,mort$deaths)+2*3)
data.frame(model=model.names, LL=model.LL, AIC=model.AIC)
