install.packages("coefplot2",repos="http://www.math.mcmaster.ca/bolker/R", type="source")
library(coefplot2)
library(arm)
library(lmtest)
library(sandwich)
library(sjPlot)
library(fitdistrplus)
library(vcd)
library(countreg)
library(MASS)
library(lme4)
library(sjPlot)
library(sjmisc)
library(multcomp)
library(effects)
sjp.setTheme(theme = "forestgrey",axis.title.size = .85, axis.textsize = .85,  legend.size = .8, geom.label.size = 3.5)
mort <- read.csv("temp1.csv")
mort$year <- as.factor(mort$year)
y <- mort[mort$pop < mort$deaths,]
x  <- aggregate(cbind(deaths,pop)  ~ age + gen + dhb + year, data = mort, sum)
y <- x[x$pop < x$deaths,]
x <- x[x$pop >= x$deaths,]
fitg  <- fitdist(x$deaths, "nbinom")
summary(fitg)
plot(fitg)
cdfcomp(fitg, addlegend=T)
denscomp(fitg, addlegend=T)
ppcomp(fitg, addlegend=T)
qqcomp(fitg, addlegend=T)
fitf <- goodfit(x$deaths, type = "nbinomial")
plot(fitf, shade=T)
rootogram(x$deaths, fitted = "Negative Binomial")
rootogram(x$deaths, fitted = "Poisson")

a1 <-      glm(deaths ~  offset(log(pop)) + age + gen + dhb + year, data = x, family = "poisson")
1 - pchisq(summary(a1)$deviance,summary(a1)$df.residual)
x1 <- cbind(x,Mean = predict(a1, newdata=x, type="response"),SE = predict(a1, newdata=x, type="response", se.fit=T)$se.fit)
a2 <-   glm.nb(deaths ~  offset(log(pop)) + age + gen + dhb + year, data = x)
1 - pchisq(summary(a2)$deviance,summary(a2)$df.residual)
a3 <-   hurdle(deaths ~  offset(log(pop)) + age + gen + dhb + year | offset(log(pop)) + age + gen + dhb + year, data = x, dist = "negbin")
a4 <-   hurdle(deaths ~  offset(log(pop)) + age + gen + dhb + year | offset(log(pop)) + age + gen + dhb + year, data = x, dist = "negbin", zero = "negbin")
a5 <- zeroinfl(deaths ~  offset(log(pop)) + age + gen + dhb + year | offset(log(pop)) + age + gen + dhb + year, data = x, dist = "negbin")
a6 <- zeroinfl(deaths ~  offset(log(pop)) + age + gen + dhb + year | 1, data = x, dist = "negbin")
AIC(a1,a2,a3,a4,a5,a6)
clog <- function(x) log(x + 0.5)
cfac <- function(x, breaks = NULL) {
  if(is.null(breaks)) breaks <- unique(quantile(x, 0:10/ 10))
  x <- cut(x, breaks, include.lowest = TRUE, right = FALSE)
  levels(x) <- paste(breaks[-length(breaks)], ifelse(diff(breaks) > 1,
                                                     c(paste("-", breaks[-c(1, length(breaks))] - 1, sep = ""), "+"), ""), sep = "")
  return(x)
}
plot(clog(deaths) ~ clog(pop), data = x)
hist(x$deaths, breaks=seq(0,45,1), xlab=expression(paste("Number of ", italic(deaths))), ylab="Frequency", main="", col="grey")
fm <- list("ML-Pois" = a1, "NB" = a2, "Hurdle-NB-B" = a3, "Hurdle-NB-NB" = a4, "ZINB" = a5)
sapply(fm, function(x) coef(x)[1:39])
cbind("ML-Pois" = sqrt(diag(vcov(a1))),"Adj-Pois" = sqrt(diag(sandwich(a1))),sapply(fm[-1], function(x) sqrt(diag(vcov(x)))[1:39]))
rbind(logLik = sapply(fm, function(x) round(logLik(x) , digits = 0)), Df = sapply(fm, function(x) attr(logLik(x), "df")))
round(c("Obs" = sum(x$deaths < 1),"ML-Pois" = sum(dpois(0, fitted(a1))),
        "NB" = sum(dnbinom(0, mu = fitted(a2), size = a2$theta)),
        "NB-Hurdle-B" = sum(predict(a3, type = "prob")[,1]),
        "NB-Hurdle-NB" = sum(predict(a4, type = "prob")[,1]),
        "ZINB" = sum(predict(a5, type = "prob")[,1])))
t(sapply(fm[3:5], function(x) round(x$coefficients$zero, digits = 3)))

summary(glht(a2, linfct=mcp(age = "Tukey")))
summary(glht(a2, linfct=mcp(year = "Tukey")))
summary(glht(a2, linfct=mcp(dhb = "Tukey")))
### glm.nb
nbmm1 <- glmer.nb(deaths ~  offset(log(pop)) + age+gen+dhb + (1|dhb), data = x)
nbmm2 <- glmer.nb(deaths ~  offset(log(pop)) + age+gen+year + (1|year), data = x)

a2 <- glm.nb(deaths ~ pop, data = x)
# plot the observed data
plot(x$pop, x$deaths, xlab="Population", ylab=expression(paste("Number of", "  ", italic(deaths))), pch=16, col=rgb(4,139,154,150,maxColorValue=255))
# pull values for intercept and beta from the summary and put them in the exponential equation
curve(exp(summary(a2)$coefficients[1,1]+summary(a2)$coefficients[2,1]*x),from=range(x$pop)[1],to=range(x$pop)[2],add=T, lwd=2, col="orangered")
# pull the standard error as well to plot the equations for confidence envelope
curve(exp(summary(a2)$coefficients[1,1]+1.96*summary(a2)$coefficients[1,2]+summary(a2)$coefficients[2,1]*x+1.96*summary(a2)$coefficients[2,2]),from=range(x$pop)[1],to=range(x$pop)[2],add=T,lty=2, col="orangered")
curve(exp(summary(a2)$coefficients[1,1]-1.96*summary(a2)$coefficients[1,2]+summary(a2)$coefficients[2,1]*x-1.96*summary(a2)$coefficients[2,2]),from=range(x$pop)[1],to=range(x$pop)[2],add=T,lty=2, col="orangered")

ms <- function(dat, mod){
  cbind(dat, Mean = predict(mod, newdata = dat, type = "response"), 
        SE = predict(mod, newdata = dat, type="response", se.fit = T)$se.fit)
}
gf <- function(mod) {
  1 - pchisq(summary(mod)$deviance,summary(mod)$df.residual)
}
