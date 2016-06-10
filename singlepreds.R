x <- mort

ggplot(x, aes(age, deaths)) +
  geom_violin() +
  geom_jitter(size=1.5) +
  scale_y_log10() +
  stat_smooth(aes(x = age, y = deaths, group=1), method="loess")

a1 <-       glm(deaths ~  offset(log(1+pop)) + age -1, data = x, family = "poisson")
a2 <-    glm.nb(deaths ~  offset(log(1+pop)) + age -1, data = x)
exp(cbind("Poisson"=coef(a1), "NB"=coef(a2)))
cbind("Poisson"=coef(a1), "NB"=coef(a2))
with(x,tapply(deaths,age,mean))
with(x, tapply(deaths, age, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

m1 <- glm.nb(deaths ~ 1 + offset(log(1+pop)), data = x)
m2 <- update(m1, . ~ . + age + offset(log(1+pop)))
anova(m1, m2)
m1p <- glm(deaths ~ 1 + offset(log(1+pop)), data = x, family = "poisson")
ll2 <- 2*(logLik(m1)-logLik(m1p))
pchisq(ll2, df = 1 , lower.tail = F)
exp((est <- cbind(Estimate = coef(m1), confint(m1))))
exp((est <- cbind(Estimate = coef(m2), confint(m2))))

m3 <- update(m1, . ~ . + quin)
anova(m1,m3)
m4 <- update(m1, . ~ . + eth)
anova(m1,m4)

a1 <-       glm(deaths ~  offset(log(1+pop)) + age + eth + gen, data = x, family = "poisson")
a2 <-    glm.nb(deaths ~  offset(log(1+pop)) + age + eth + gen, data = x)
a2b <-   glm.nb(deaths ~  offset(log(1+pop)) + age * eth * gen, data = x)
a3 <-  zeroinfl(deaths ~  offset(log(1+pop)) + age + eth + gen    | offset(log(1+pop)) + age + eth + gen, data = x, dist = "negbin")
a3b <- zeroinfl(deaths ~  offset(log(1+pop)) + age + eth + gen    | offset(log(1+pop)) + age + eth + gen, data = x)
a4 <-  zeroinfl(deaths ~  offset(log(1+pop)) + age + eth + gen    | offset(log(1+pop)) + age + eth + gen, data = x, dist = "negbin")
a4b <- zeroinfl(deaths ~  offset(log(1+pop)) + age + eth + gen    | offset(log(1+pop)) + age + eth + gen, data = x)
a5 <-  zeroinfl(deaths ~  offset(log(1+pop)) + age + eth + gen    | offset(log(1+pop)) + age + eth + gen, data = x, dist = "negbin")
a5b <- zeroinfl(deaths ~  offset(log(1+pop)) + age + eth + gen    | offset(log(1+pop)) + age + eth + gen, data = x)
a6 <-  zeroinfl(deaths ~  offset(log(1+pop)) + age + eth + gen    | offset(log(1+pop)) + age + eth + gen, data = x, dist = "negbin")
a6b <- zeroinfl(deaths ~  offset(log(1+pop)) + age + eth + gen    | offset(log(1+pop)) + age + eth + gen, data = x)
a7 <-  zeroinfl(deaths ~  offset(log(1+pop)) + age + eth + gen    | offset(log(1+pop)) + 1  , data = x, dist = "negbin")
a7b <- zeroinfl(deaths ~  offset(log(1+pop)) + age + eth + gen    | offset(log(1+pop)) + 1  , data = x)
AIC(a1,a2,a2b,a3,a3b,a4,a4b,a5,a5b,a6,a7,a7b)
exp(cbind("Poisson"=coef(a1), "NB"=coef(a2)))

par(mfrow = c(1, 1),pty = "s")   
coefplot2(a1)
coefplot2(a2)
summary(glht(a2, mcp(age="Tukey")))
barplot(table(x$eth, x$deaths), 
        beside = T, 
        legend.text = T)


m1 <- zeroinfl(deaths ~ offset(log(1+pop)) + age + eth | offset(log(1+pop)) + gen,data = x, dist = "negbin")
m0 <- update(m1, . ~ 1)
pchisq(2 * (logLik(m1) - logLik(m0)), df = 3, lower.tail=FALSE)

m2 <- a2
binnedplot(fitted(m0),resid(m0))
binnedplot(fitted(m1),resid(m1))
binnedplot(fitted(m2),resid(m2))
output <- data.frame(resid = resid(m1), fitted = fitted(m1))
ggplot(output, aes(fitted, resid)) + geom_jitter(position = position_jitter(width = 0.25), 
                                                 alpha = 0.5) + stat_smooth(method = "loess")
ggplot(output, aes(fitted, resid)) +
  geom_jitter(position=position_jitter(width=.25), alpha=.5) +
  stat_quantile(method="rq")

output <- within(output, {
  broken <- cut(fitted, hist(fitted, plot=FALSE)$breaks)
})

ggplot(output, aes(broken, resid)) +
  geom_boxplot() +
  geom_jitter(alpha=.25)

ggplot(x, aes(deaths, fill = age)) +
  geom_histogram() +
  scale_x_log10() +
  facet_grid(age ~ ., margins=TRUE, scales="free_y")

dput(round(coef(m1, "count"), 4))
dput(round(coef(m1, "zero"), 4))
f <- function(data, i) {
  m <- zeroinfl(deaths ~ offset(log(1+pop)) + age + eth | offset(log(1+pop)) + gen,
                data = data[i, ], dist = "negbin",
                start = list(count = c(-6.7023, -1.8246, -1.558, 0.0264, -0.0376, -0.8164, 
                                        -0.2653),
                             zero = c(-8.5717, -10.7977)))
  as.vector(t(do.call(rbind, coef(summary(m)))[, 1:2]))
}

set.seed(10)
(res <- boot(x, f, R = 1200, parallel = "snow", ncpus = 4))

#######
zinb <- read.csv("http://www.ats.ucla.edu/stat/data/fish.csv")
zinb <- within(zinb, {
  nofish <- factor(nofish)
  livebait <- factor(livebait)
  camper <- factor(camper)
})
## histogram with x axis in log10 scale
ggplot(zinb, aes(count, fill = camper)) +
  geom_histogram() +
  scale_x_log10() +
  facet_grid(camper ~ ., margins=TRUE, scales="free_y")

summary(zinb)
m1 <- zeroinfl(count ~ child + camper | persons,
               data = zinb, dist = "negbin", EM = TRUE)
summary(m1)
m0 <- update(m1, . ~ 1)

pchisq(2 * (logLik(m1) - logLik(m0)), df = 3, lower.tail=FALSE)
summary(m2 <- glm.nb(count ~ child + camper, data = zinb))

dput(round(coef(m1, "count"), 4))

## structure(c(1.3711, -1.5152, 0.879), .Names = c("(Intercept)", 
## "child", "camper1"))

dput(round(coef(m1, "zero"), 4))

## structure(c(1.6028, -1.6663), .Names = c("(Intercept)", "persons"
## ))

f <- function(data, i) {
  require(countreg)
  m <- zeroinfl(count ~ child + camper | persons,
                data = data[i, ], dist = "negbin",
                start = list(count = c(1.3711, -1.5152, 0.879), zero = c(1.6028, -1.6663)))
  as.vector(t(do.call(rbind, coef(summary(m)))[, 1:2]))
}

set.seed(10)
(res <- boot(zinb, f, R = 1200, parallel = "snow", ncpus = 4))

# basic parameter estimates with percentile and bias adjusted CIs
parms <- t(sapply(c(1, 3, 5, 7, 9), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "bca"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              bcaLL = bca[4], bcaLL = bca[5]))
}))

## add row names
row.names(parms) <- names(coef(m1))
## print results
parms
## compare with normal based approximation
confint(m1)
## exponentiated parameter estimates with percentile and bias adjusted CIs
expparms <- t(sapply(c(1, 3, 5, 7, 9), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "bca"), h = exp)
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              bcaLL = bca[4], bcaLL = bca[5]))
}))

## add row names
row.names(expparms) <- names(coef(m1))
## print results
expparms

newdata1 <- expand.grid(0:3, factor(0:1), 1:4)
colnames(newdata1) <- c("child", "camper", "persons")
newdata1$phat <- predict(m1, newdata1)

ggplot(newdata1, aes(x = child, y = phat, colour = factor(persons))) +
  geom_point() +
  geom_line() +
  facet_wrap(~camper) +
  labs(x = "Number of Children", y = "Predicted Fish Caught")
