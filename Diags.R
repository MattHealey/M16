library(MASS)
library(countreg)
library(pscl)
library(lme4)
mort.normal  <-       glm(deaths ~  offset(log(1+pop)) + age + dhb + year + gen, data = mort)
mort.pois    <-       glm(deaths ~  offset(log(1+pop)) + age + dhb + year + gen, data = mort, family = "poisson")
mort.nb      <-    glm.nb(deaths ~  offset(log(1+pop)) + age + dhb + year + gen + eth + quin, data = mort)
mort.nb2     <-    glm.nb(deaths ~  offset(log(1+pop)) + age + dhb + year + gen + eth + quin + pop, data = mort)
mort.nb3     <-    glm.nb(deaths ~  age + dhb + year + gen + eth + quin + pop, data = mort)
mort.zip     <-  zeroinfl(deaths ~  offset(log(1+pop)) + age + dhb + year + gen| dhb, data = mort, link = "logit", dist = "poisson", trace = TRUE)
mort.zinb    <-  zeroinfl(deaths ~  offset(log(1+pop)) + age + dhb + year + gen| dhb, data = mort, link = "logit", dist = "negbin", trace = TRUE, EM = FALSE)
mort.mnb     <-  glmer.nb(deaths ~  offset(log(1+pop)) + dhb + (1|year) + pop, data = mort)
# AIC values 
AIC(mort.normal, mort.pois, mort.nb, mort.zip, mort.zinb) 
# AIC weights 
compare.models <- list( ) 
compare.models[[1]] <- mort.normal 
compare.models[[2]] <- mort.pois 
compare.models[[3]] <- mort.nb 
compare.models[[4]] <- mort.zip 
compare.models[[5]] <- mort.zinb 
compare.names <- c("Typical", "Poisson", "NB", "ZIP", "ZINB") 
names(compare.models) <- compare.names 
compare.results <- data.frame(models = compare.names) 
compare.results$aic.val <- unlist(lapply(compare.models, AIC)) 
compare.results$aic.delta <- compare.results$aic.val-min(compare.results$aic.val) 
compare.results$aic.likelihood <-  exp(-0.5* compare.results$aic.delta) 
compare.results$aic.weight <- compare.results$aic.likelihood/sum(compare.results$aic.likelihood) 
options(scipen = 9, digits = 10)
compare.results
# BIC values 
AIC(mort.normal, k = log(nrow(mort))) 
AIC(mort.pois, k = log(nrow(mort))) 
AIC(mort.nb, k = log(nrow(mort))) 
AIC(mort.zip, k = log(nrow(mort))) 
AIC(mort.zinb, k = log(nrow(mort)))
# observed zero counts 
# actual 
sum(mort$deaths < 1) 
# typical 
sum(dnorm(0, fitted(mort.normal))) 
# poisson 
sum(dpois(0, fitted(mort.pois))) 
# nb 
sum(dnbinom(0, mu = fitted(mort.nb), size = mort.nb$theta)) 
# zip 
sum(predict(mort.zip, type = "prob")[,1]) 
# zinb 
sum(predict(mort.zinb, type = "prob")[,1]) 
#Diagnostics 
# normal residuals density plot 
plot(density(residuals(mort.normal))) 
# histogram plot with fitted probabilities 
# predicted values for typical regression 
normal.y.hat <- predict(mort.normal, type = "response") 
normal.y <- mort.normal$y 
normal.yUnique <- 0:max(normal.y) 
normal.nUnique <- length(normal.yUnique) 
phat.normal <- matrix(NA, length(normal.y.hat), normal.nUnique) 
dimnames(phat.normal) <- list(NULL, normal.yUnique)
for (i in 1:normal.nUnique) { 
  phat.normal[, i] <- dnorm(mean = normal.y.hat, sd =
                              sd(residuals(mort.normal)),x = 
                              normal.yUnique[i]) 
} 
# mean of the normal predicted probabilities for each value of the outcome 
phat.normal.mn <- apply(phat.normal, 2, mean) 
# probability of observing each value and mean predicted probabilities for 
#count regression models 
phat.pois <- predprob(mort.pois) 
phat.pois.mn <- apply(phat.pois, 2, mean) 
phat.nb <- predprob(mort.nb) 
phat.nb.mn <- apply(phat.nb, 2, mean) 
phat.zip <- predprob(mort.zip) 
phat.zip.mn <- apply(phat.zip, 2, mean) 
phat.zinb <- predprob(mort.zinb) 
phat.zinb.mn <- apply(phat.zinb, 2, mean) 
# histogram 
hist(mort$deaths, prob = TRUE, col = "gray90", 
     breaks=seq(min(mort$deaths)-0.5, 
                max(mort$deaths)+.5, 1), xlab = "Deaths", ylim=c(0,1)) 
# overlay predicted values 
lines(x = seq(0, 10, 1), y = phat.normal.mn, type = "b", lwd=2, lty=1, col="black") 
lines(x = seq(0, 10, 1), y = phat.pois.mn, type = "b", lwd=2, lty=2, col="red") 
lines(x = seq(0, 10, 1), y = phat.nb.mn, type = "b", lwd=2, lty=3, col="blue") 
lines(x = seq(0, 10, 1), y = phat.zip.mn, type = "b", lwd=2, lty=4, col="green") 
lines(x = seq(0, 10, 1), y = phat.zinb.mn, type = "b", lwd=2, lty=5, col="gray80") 
# legend 
legend(1, 0.7, c("Typical (Normal)","Poisson", "Negative Binomial", "Zero-Inflated Poisson", "Zero-Inflated Negative Binomial"), lty=seq(1:5), col = c("black","gray20","gray40","gray60","gray80"), lwd=2) 
# predicted vs. residual plots 
# typical 
plot(predict(mort.normal, type="response"), residuals(mort.normal), main="Typical Regression", ylab="Residuals", xlab="Predicted", ylim=c(-2,5)) 
abline(h=0,lty=1,col="gray") 
lines(lowess(predict(mort.normal,type="response"),residuals(mort.normal)), lwd=2, lty=2) 
# poisson 
plot(predict(mort.pois,type="response"),residuals(mort.pois), main="Poisson Regression", ylab="Residuals", xlab="Predicted", ylim=c(-2,5)) 
abline(h=0,lty=1,col="gray") 
lines(lowess(predict(mort.pois,type="response"),residuals(mort.pois)),lwd=2, lty=2) 
# negative binomial 
plot(predict(mort.nb,type="response"),residuals(mort.nb), main="Negative Binomial Regression", ylab="Residuals", xlab="Predicted", ylim=c(-2,5)) 
abline(h=0,lty=1,col="gray") 
lines(lowess(predict(mort.nb,type="response"),residuals(mort.nb)), lwd=2, lty=2) 
# ZIP 
plot(predict(mort.zip,type="response"),residuals(mort.zip), main="ZIP Regression", ylab="Residuals", xlab="Predicted", ylim=c(-2,5)) 
abline(h=0,lty=1,col="gray") 
lines(lowess(predict(mort.zip,type="response"),residuals(mort.zip)),lwd=2, lty=2) 
# ZINB 
plot(predict(mort.zinb,type="response"),residuals(mort.zinb), main="ZINB Regression", ylab="Residuals", xlab="Predicted", ylim=c(-2,5)) 
abline(h=0,lty=1,col="gray") 
lines(lowess(predict(mort.zinb,type="response"),residuals(mort.zinb)),lwd=2, lty=2) 
# plot Cook's D for the typical regression 
plot(cooks.distance(mort.normal), main="Cook's D Estimates", ylab="Cook's D", xlab="Observation") 
abline(h=(4/nrow(mort)), col="red", lwd=2) 
# plot Cook's D for the Poisson model 
plot(cooks.distance(mort.pois), main="Cook's D Estimates", ylab="Cook's D", xlab="Observation") 
# plot Cook's D for the negative binomial model 
plot(cooks.distance(mort.nb), main="Cook's D Estimates", ylab="Cook's D", xlab="Observation") 
abline(h=(4/nrow(count.data)), col="red", lwd=2) 
#
#
#CUT cook's D functions#
# linearity plots for negative binomial model 
plot(mort$gen,resid(mort.nb),xlab="Sex", ylab="Residuals") 
plot(mort$eth,resid(mort.nb),xlab="Eth",ylab="Residuals") 
plot(mort$year,resid(mort.nb),xlab="Year", ylab="Residuals") 
plot(mort$dhb,resid(mort.nb),xlab="DHB", ylab="Residuals") 
plot(mort$quin,resid(mort.nb),xlab="Quin (SES)", ylab="Residuals") 
#Select Final Model 
# define the NB models to compare 
cand.models <- list( ) 
cand.models[[1]]   <- glm.nb(deaths ~ offset(log(1+pop)) + age, data = mort) 
cand.models[[2]]   <- glm.nb(deaths ~ offset(log(1+pop)) + eth, data = mort) 
cand.models[[3]]   <- glm.nb(deaths ~ offset(log(1+pop)) + gen, data = mort) 
cand.models[[4]]   <- glm.nb(deaths ~ offset(log(1+pop)) + quin, data = mort) 
cand.models[[5]]   <- glm.nb(deaths ~ offset(log(1+pop)) + dhb, data = mort) 
cand.models[[6]]   <- glm.nb(deaths ~ offset(log(1+pop)) + year, data = mort) 
cand.models[[7]]   <- glm.nb(deaths ~ offset(log(1+pop)) + age + eth, data = mort) 
cand.models[[8]]   <- glm.nb(deaths ~ offset(log(1+pop)) + age + eth + gen, data = mort) 
cand.models[[9]]   <- glm.nb(deaths ~ offset(log(1+pop)) + age + eth + gen + quin, data = mort) 
cand.models[[10]]  <- glm.nb(deaths ~ offset(log(1+pop)) + age + eth + gen + quin + dhb, data = mort) 
cand.models[[11]]  <- glm.nb(deaths ~ offset(log(1+pop)) + age + eth + gen + quin + dhb + year, data = mort) 
cand.models[[12]]  <- glm.nb(deaths ~ offset(log(1+pop)) + age * eth, data = mort) 
cand.models[[13]]  <- glm.nb(deaths ~ offset(log(1+pop)) + age * eth * gen, data = mort) 
cand.models[[14]]  <- glm.nb(deaths ~ offset(log(1+pop)) + age * eth * gen * quin, data = mort) 
cand.models[[15]]  <- glm.nb(deaths ~ offset(log(1+pop)) + age * eth * gen * quin * dhb, data = mort) 
cand.models[[16]]  <- glm.nb(deaths ~ offset(log(1+pop)) + age * eth * gen * quin * dhb * year, data = mort) 
cand.models[[17]]  <- zeroinfl(deaths ~  offset(log(1+pop)) + age | offset(log(1+pop)) + age, data = mort, link = "logit", dist = "negbin", trace = TRUE, EM = FALSE)
cand.models[[18]]  <- zeroinfl(deaths ~  offset(log(1+pop)) + age | offset(log(1+pop)) + eth, data = mort, link = "logit", dist = "negbin", trace = TRUE, EM = FALSE)
cand.models[[19]]  <- zeroinfl(deaths ~  offset(log(1+pop)) + age | offset(log(1+pop)) + gen, data = mort, link = "logit", dist = "negbin", trace = TRUE, EM = FALSE)
cand.models[[20]]  <- zeroinfl(deaths ~  offset(log(1+pop)) + age | offset(log(1+pop)) + quin, data = mort, link = "logit", dist = "negbin", trace = TRUE, EM = FALSE)
cand.models[[21]]  <- zeroinfl(deaths ~  offset(log(1+pop)) + age | offset(log(1+pop)) + dhb, data = mort, link = "logit", dist = "negbin", trace = TRUE, EM = FALSE)
cand.models[[22]]  <- zeroinfl(deaths ~  offset(log(1+pop)) + age | offset(log(1+pop)) + year, data = mort, link = "logit", dist = "negbin", trace = TRUE, EM = FALSE)
cand.models[[23]]  <- zeroinfl(deaths ~  offset(log(1+pop)) + eth | offset(log(1+pop)) + age, data = mort, link = "logit", dist = "negbin", trace = TRUE, EM = FALSE)
cand.models[[24]]  <- zeroinfl(deaths ~  offset(log(1+pop)) + eth | offset(log(1+pop)) + eth, data = mort, link = "logit", dist = "negbin", trace = TRUE, EM = FALSE)
cand.models[[25]]  <- zeroinfl(deaths ~  offset(log(1+pop)) + eth | offset(log(1+pop)) + gen, data = mort, link = "logit", dist = "negbin", trace = TRUE, EM = FALSE)
cand.models[[26]]  <- zeroinfl(deaths ~  offset(log(1+pop)) + eth | offset(log(1+pop)) + quin, data = mort, link = "logit", dist = "negbin", trace = TRUE, EM = FALSE)
cand.models[[27]]  <- zeroinfl(deaths ~  offset(log(1+pop)) + eth | offset(log(1+pop)) + dhb, data = mort, link = "logit", dist = "negbin", trace = TRUE, EM = FALSE)
cand.models[[28]]  <- zeroinfl(deaths ~  offset(log(1+pop)) + eth | offset(log(1+pop)) + year, data = mort, link = "logit", dist = "negbin", trace = TRUE, EM = FALSE)
cand.models[[29]]  <- zeroinfl(deaths ~  offset(log(1+pop)) + gen | offset(log(1+pop)) + age, data = mort, link = "logit", dist = "negbin", trace = TRUE, EM = FALSE)
cand.models[[30]]  <- zeroinfl(deaths ~  offset(log(1+pop)) + gen | offset(log(1+pop)) + eth, data = mort, link = "logit", dist = "negbin", trace = TRUE, EM = FALSE)
cand.models[[31]]  <- zeroinfl(deaths ~  offset(log(1+pop)) + gen | offset(log(1+pop)) + gen, data = mort, link = "logit", dist = "negbin", trace = TRUE, EM = FALSE)
cand.models[[32]]  <- zeroinfl(deaths ~  offset(log(1+pop)) + gen | offset(log(1+pop)) + quin, data = mort, link = "logit", dist = "negbin", trace = TRUE, EM = FALSE)
cand.models[[33]]  <- zeroinfl(deaths ~  offset(log(1+pop)) + gen | offset(log(1+pop)) + dhb, data = mort, link = "logit", dist = "negbin", trace = TRUE, EM = FALSE)
cand.models[[34]]  <- zeroinfl(deaths ~  offset(log(1+pop)) + gen | offset(log(1+pop)) + year, data = mort, link = "logit", dist = "negbin", trace = TRUE, EM = FALSE)
cand.models[[35]]  <- zeroinfl(deaths ~  offset(log(1+pop)) + quin | offset(log(1+pop)) + age, data = mort, link = "logit", dist = "negbin", trace = TRUE, EM = FALSE)
cand.models[[36]]  <- zeroinfl(deaths ~  offset(log(1+pop)) + quin | offset(log(1+pop)) + eth, data = mort, link = "logit", dist = "negbin", trace = TRUE, EM = FALSE)
cand.models[[37]]  <- zeroinfl(deaths ~  offset(log(1+pop)) + quin | offset(log(1+pop)) + gen, data = mort, link = "logit", dist = "negbin", trace = TRUE, EM = FALSE)
cand.models[[38]]  <- zeroinfl(deaths ~  offset(log(1+pop)) + quin | offset(log(1+pop)) + quin, data = mort, link = "logit", dist = "negbin", trace = TRUE, EM = FALSE)
cand.models[[39]]  <- zeroinfl(deaths ~  offset(log(1+pop)) + quin | offset(log(1+pop)) + dhb, data = mort, link = "logit", dist = "negbin", trace = TRUE, EM = FALSE)
cand.models[[40]]  <- zeroinfl(deaths ~  offset(log(1+pop)) + quin | offset(log(1+pop)) + year, data = mort, link = "logit", dist = "negbin", trace = TRUE, EM = FALSE)

# name the models 
model.names <- c("Age", "Age+Eth", "Age+Eth+Gen", "Age+Eth+Gen+Quin"
                 , "Age+Eth+Gen+Quin+DHB", "Age+Eth+Gen+Quin+DHB+Year", "Age*Eth", 
                 "Age*Gen","Age*Quin", "Age*DHB", "Age*Year") 
names(cand.models) <- model.names 
# calculate and combine AIC, AIC weights, and BIC 
results <- data.frame(models = model.names) 
results$bic.val <- unlist(lapply(cand.models, BIC))
results$bic.rank <- rank(results$bic.val) 
results$aic.val <- unlist(lapply(cand.models, AIC))
results$aic.delta <- results$aic.val-min(results$aic.val) 
results$aic.likelihood <-  exp(-0.5* results$aic.delta) 
results$aic.weight <- results$aic.likelihood/sum(results$aic.likelihood) 
# sort models by AIC weight 
results <- results[rev(order(results[, "aic.weight"])),] 
results$cum.aic.weight <- cumsum(results[, "aic.weight"]) 
results
# final model 
mort.final.nb <- glm.nb() 



#mort.nb     <-    glm.nb(deaths ~  offset(log(1+pop)) + age + gen + eth + quin + dhb + year, data = mort)
#glm.nb.glmulti <- function(formula, data, ...) {
#  glm.nb(as.formula(paste(deparse(formula))), vi, data=data, method="ML", ...)
#}
#res <- glmulti(deaths ~  age + gen + eth + quin + dhb + year, data=mort, level=1,
#               fitfunction=glm.nb, crit="aicc", confsetsize=128)