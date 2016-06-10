library(devtools)
library(psych)
library(lme4)
library(lmeNB)
library(MASS)
library(arm)
library(vcd)
library(multcomp)
library(texreg)
library(ggplot2)
library(gridExtra)
library(faraway)
library(qvcalc)
library(fitdistrplus)
library(margins)
library(sjPlot)
library(phia)
library("countreg")
options(scipen = 999)
install_github('leeper/margins')
library(margins)
contrasts(mort$age)  <- contr.treatment(levels(mort$age), base=which(levels(mort$age) == "C"))
contrasts(mort$dhb)  <- contr.treatment(levels(mort$dhb), base=which(levels(mort$dhb) == "Auckland"))
contrasts(mort$eth)  <- contr.treatment(levels(mort$eth), base=which(levels(mort$eth) == "European or other"))
contrasts(mort$gen)  <- contr.treatment(levels(mort$gen), base=which(levels(mort$gen) == "Female"))
contrasts(mort$quin) <- contr.treatment(levels(mort$quin), base=which(levels(mort$quin) == "01"))
contrasts(mort$year) <- contr.treatment(levels(mort$year), base=which(levels(mort$year) == "2014"))
da <- mort
science_theme = theme(panel.grid.major = element_line(size = 0.5,
                                                      color = "grey"),
                      axis.line = element_line(size = 0.7, color = "black"),
                      text = element_text(size = 14)) +
  theme_bw(base_size = 12,
           base_family = "Helvetica")

sjp.setTheme(theme = science_theme)
sjp.frq(da$deaths,
        geom.colors = "lightblue",
        title = NULL, type = 'hist')
sjp.frq(da$deaths,
        geom.colors = "lightblue",
        title = NULL, type = 'bars')

da$lpop    <- log(1+da$pop)
glm.null   <- glm.nb(deaths ~ offset(lpop),         data = da)
glm.age    <- glm.nb(deaths ~ offset(lpop) + age ,  data = da)
glm.eth    <- glm.nb(deaths ~ offset(lpop) + eth ,  data = da)
glm.gen    <- glm.nb(deaths ~ offset(lpop) + gen ,  data = da)
glm.quin   <- glm.nb(deaths ~ offset(lpop) + quin,  data = da)
glm.dhb    <- glm.nb(deaths ~ offset(lpop) + dhb ,  data = da)
glm.year   <- glm.nb(deaths ~ offset(lpop) + year,  data = da)
library(doBy)
orderBy(~ AIC, AIC(glm.null,glm.age,glm.eth,glm.gen,glm.quin,glm.dhb,glm.year))
glm.all    <- glm.nb(deaths ~ offset(lpop) + 0 + age * eth + dhb + quin, data = da)
glmer.null <- glmer.nb(deaths ~ offset(lpop)        + (1 | dhb), data = da, control=glmerControl(optimizer="bobyqa"), nAGQ = 9 )
glmer.age  <- glmer.nb(deaths ~ offset(lpop) + age  + (1 | dhb), data = mort, control=glmerControl(optimizer="bobyqa"), nAGQ = 9 )
glmer.eth  <- glmer.nb(deaths ~ offset(lpop) + age  + (1 | dhb), data = mort, control=glmerControl(optimizer="bobyqa"), nAGQ = 9 )
glmer.gen  <- glmer.nb(deaths ~ offset(lpop) + gen  + (1 | dhb), data = da, control=glmerControl(optimizer="bobyqa"), nAGQ = 9 )
glmer.quin <- glmer.nb(deaths ~ offset(lpop) + quin + (1 | dhb), data = da, control=glmerControl(optimizer="bobyqa"), nAGQ = 9 )
glmer.dhb  <- glmer.nb(deaths ~ offset(lpop) + dhb  + (1 | dhb), data = da, control=glmerControl(optimizer="bobyqa"), nAGQ = 9 )
glmer.all  <- glmer.nb(deaths ~ offset(lpop) + age  * eth + (1 | dhb), data = da, control=glmerControl(optimizer="bobyqa"), nAGQ = 9 )
V1 ~ (1|V2) + V3 + (0+V3|V2)
glmer.a  <- glmer.nb(deaths ~ offset(log(1+pop)) + year  + (1 | dhb) + (0+year|dhb), data = y)


glm.null$coefficients <- exp(glm.null$coefficients)
glm.age$coefficients <- exp(glm.age$coefficients)
glm.gen$coefficients <- exp(glm.gen$coefficients)
glm.eth$coefficients <- exp(glm.eth$coefficients)
glm.quin$coefficients <- exp(glm.quin$coefficients)
glm.dhb$coefficients <- exp(glm.dhb$coefficients)
glm.year$coefficients <- exp(glm.year$coefficients)
screenreg(l=list(glm.null,glm.age, glm.gen, glm.eth, glm.quin, glm.dhb, glm.year), single.row = T)
## fitdistrplus
hist(da$deaths, breaks = 0:max(da$deaths))
hist(da$pop, breaks = 0:max(da$pop))
par(mfrow=c(2,2))
## how does normal look?
qqp(da$deaths, "norm")
## log norm?
qqp(da$deaths, "lnorm")
## poisson
foo <- goodfit(da$deaths,type = "poisson")
summary(foo)
plot(foo) 
## NB
foo <- goodfit(da$deaths,type = "nbinomial")
plot(foo)
foo <- fitdist(da$deaths, "nbinom")
summary(foo)
plot(foo) 
Ord_plot(da$deaths, type = "nbinomial")
distplot(da$deaths, type="nbinomial")
descdist(da$deaths, discrete = T)
descdist(rnbinom(length(da$deaths), size = foo$estimate[[1]], mu = foo$estimate[[2]]),
         discrete = T, boot = 5000)
foo <- fitdist(da$pop, "nbinom")
foo
plot(foo)
descdist(da$pop, discrete = F)
descdist(rnbinom(length(da$pop), size = foo$estimate[[1]], mu = foo$estimate[[2]]),
         discrete = T, boot = 5000)
ggplot(da, aes(deaths, fill = dhb)) + geom_histogram(binwidth=1)+facet_grid(dhb~.,margins=TRUE,scales="free") + science_theme
ggplot(da, aes(deaths, fill = eth)) + geom_histogram(binwidth=1)+facet_grid(eth~.,margins=TRUE,scales="free") + science_theme
ggplot(da, aes(deaths, fill = gen)) + geom_histogram(binwidth=1)+facet_grid(gen~.,margins=TRUE,scales="free") + science_theme
ggplot(da, aes(deaths, fill = age)) + geom_histogram(binwidth=1)+facet_grid(age~.,margins=TRUE,scales="free") + science_theme
ggplot(da, aes(deaths, fill = quin)) + geom_histogram(binwidth=1)+facet_grid(quin~.,margins=TRUE,scales="free") + science_theme

par(mfrow = c(2,2))
res <- residuals(glm.null, type="deviance")
plot(predict(glm.null), res, cex = .1, pch = 21, type = "p", col = "black")
abline(h=0, lty=2)
qqnorm(res)
qqline(res)
halfnorm(residuals(glm.null))
plot(deaths ~ log(pop), data=da, cex = .2)
prs  <- predict(glm.null, type="response", se.fit=TRUE)
pris <- data.frame("pest"=prs[[1]], "lwr"=prs[[1]]-prs[[2]], "upr"=prs[[1]]+prs[[2]])
points(pris$pest ~ log(da$pop), col="red", cex = .1)
points(pris$lwr  ~ log(da$pop), col="pink", pch=19, cex = .1)
points(pris$upr  ~ log(da$pop), col="pink", pch=19, cex =.1)
par(mfrow = c(2,2))
plot(glm.null)
output <- data.frame(resid = resid(glm.null)[2], fitted = fitted(glm.null))
ggplot(output, aes(fitted, resid)) + geom_jitter(position=position_jitter(width=.25), alpha=.5) + stat_quantile(method="rq") + science_theme + theme_bw(base_size = 12, base_family = "Helvetica")

mp <- function(m1) {
newdata1 <- da
newdata1 <- cbind(newdata1, predict(m1, newdata1, type = "link", se.fit=TRUE))
newdata2 <- within(newdata1, {
  deaths <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})}
newdata2 <- mp(glm.age)
ggplot(newdata2, aes(newdata2$pop, deaths)) + science_theme +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = age), alpha = .2) +
  geom_line(aes(colour = age), size = .8) +
  labs(x = "log(pop)", y = "Predicted Deaths")
pa1 <- ggplot(newdata2, aes(log(newdata2$pop), deaths)) + labs(x = "Log population", y = "Predicted Deaths")
pa1 + geom_point() + facet_grid(. ~ age) + science_theme
pa1 <- pa1 + geom_ribbon(aes(ymin = LL, ymax = UL, fill = age), alpha = .25) + science_theme
pa1 + geom_line(aes(colour = age), size = 1) + labs(x = "log(pop)", y = "Predicted Deaths") + science_theme + geom_point()
pa1 <- ggplot(newdata2, aes(log(newdata2$pop), deaths)) + labs(x = "Log population", y = "Predicted Deaths")
pa1 + geom_point() + facet_grid(. ~ age) + science_theme
pa1 + geom_ribbon(aes(ymin = LL, ymax = UL, fill = age), alpha = 1) + science_theme + geom_point()
age.qvs <- qvcalc(glm.age, "age")
summary(age.qvs, digits=2)
plot(age.qvs, pch = 7, cex = .5, type = "o")
res <- residuals(glm.age, type="deviance")
plot(predict(glm.age), res)
abline(h=0, lty=2)
qqnorm(res)
qqline(res)
plot(deaths ~ lpop, data=da, cex = .2)
prs  <- predict(glm.age, type="response", se.fit=TRUE)
pris <- data.frame("pest"=prs[[1]], "lwr"=prs[[1]]-prs[[2]], "upr"=prs[[1]]+prs[[2]])
points(pris$lwr  ~ log(da$pop), col="pink", pch=19, cex = .1)
points(pris$upr  ~ log(da$pop), col="pink", pch=19, cex =.1)
par(mfrow = c(2,2))
plot(glm.age)
foo <- data.frame(resid = resid(glm.age), fitted = fitted(glm.age))
ggplot(foo, aes(fitted, resid)) + 
  geom_jitter(position=position_jitter(width=.25), alpha=.5) + 
  stat_quantile(method="rq") + 
  science_theme + 
  theme_bw(base_size = 12, base_family = "Helvetica")
glm.age    <- glm.nb(deaths ~ offset(lpop) + age ,  data = da)
mage1 <- glht(glm.age, linfct = mcp(age = "Tukey"))
plot(mage1)
summary(mage1)
ma <- margins(glm.age)[[1]]
plot(ma)
summary(ma)
#
glm.age    <- glm.nb(deaths ~ 0 + offset(log(1+pop)) + age,  data = da)
(est <- cbind(Estimate = coef(glm.age), confint(glm.age)))
exp(est)
mage1 <- glht(glm.age, linfct = mcp(age = "Tukey"))
plot(mage1)
summary(mage1)
ma <- margins(glm.age)[[1]]
plot(ma)
summary(ma)
#
#Simultaneous Tests for General Linear Hypotheses
#
#Multiple Comparisons of Means: Tukey Contrasts
#
#
#Fit: glm.nb(formula = deaths ~ offset(lpop) + age, data = da, init.theta = 2.963763297,
#            link = log)
#Linear Hypotheses:
#  Estimate Std. Error z value Pr(>|z|)
#  A - C == 0  2.91388    0.06510  44.757   <0.001 ***
#  B - C == 0  0.94581    0.06959  13.592   <0.001 ***
#  D - C == 0  0.36412    0.07301   4.988   <0.001 ***
#  E - C == 0  1.73090    0.06324  27.371   <0.001 ***
#  F - C == 0  1.85745    0.06316  29.409   <0.001 ***
#  B - A == 0 -1.96807    0.05402 -36.429   <0.001 ***
#  D - A == 0 -2.54976    0.05836 -43.688   <0.001 ***
#  E - A == 0 -1.18298    0.04556 -25.966   <0.001 ***
#  F - A == 0 -1.05643    0.04545 -23.244   <0.001 ***
#  D - B == 0 -0.58169    0.06332  -9.186   <0.001 ***
#  E - B == 0  0.78509    0.05176  15.167   <0.001 ***
#  F - B == 0  0.91164    0.05167  17.645   <0.001 ***
#  E - D == 0  1.36678    0.05627  24.288   <0.001 ***
#  F - D == 0  1.49333    0.05619  26.578   <0.001 ***
#  F - E == 0  0.12655    0.04273   2.961    0.035 *
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#(Adjusted p values reported -- single-step method)

(est <- cbind(Estimate = coef(glm.age), confint(glm.age)))
#Estimate      2.5 %    97.5 %
#(Intercept) -9.1071695 -9.2177995 -8.999215
#ageA         2.9138795  2.7867161  3.042897
#ageB         0.9458132  0.8096127  1.083344
#ageD         0.3641188  0.2205933  0.508491
#ageE         1.7309030  1.6071622  1.856680
#ageF         1.8574537  1.7337489  1.983204

round(exp(est),2)
#Estimate 2.5 % 97.5 %
#(Intercept)     0.00  0.00   0.00
#ageA           18.43 16.23  20.97
#ageB            2.57  2.25   2.95
#ageD            1.44  1.25   1.66
#ageE            5.65  4.99   6.40
#ageF            6.41  5.66   7.27

with(mort, tapply(deaths, age, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))
##A M (SD) = 1.54 (2.23)
##B M (SD) = 0.83 (1.13)
##C M (SD) = 0.40 (0.69)
##D M (SD) = 0.59 (0.95)
##E M (SD) = 2.28 (2.56)
##F M (SD) = 2.41 (2.65)

## ETH
names(da)
da$eth  <- relevel(da$eth, "European or other")
meth <- glm.nb(deaths ~ offset(da$lpop) + eth , data = da)
m1 <- glm.nb(deaths ~ offset(da$lpop) + eth , data = da)
newdata1 <- da
newdata1 <- cbind(newdata1, predict(m1, newdata1, type = "link", se.fit=TRUE))
newdata2 <- within(newdata1, {
  deaths <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

ggplot(newdata2, aes(newdata2$pop), deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = eth), alpha = .2) +
  geom_line(aes(colour = eth), size = .8) +
  labs(x = "log(pop)", y = "Predicted Deaths") + science_theme

pa1 <- ggplot(newdata2, aes(newdata2$pop, deaths))
pa1 + geom_point() + facet_grid(. ~ eth) + science_theme
pa1 <- pa1 + geom_ribbon(aes(ymin = LL, ymax = UL, fill = age), alpha = .05) + science_theme
pa1 + geom_line(aes(colour = age), size = .5) + labs(x = "log(pop)", y = "Predicted Deaths") + science_theme

par(mfrow = c(2,2))
res <- residuals(meth, type="deviance")
plot(predict(meth), res)
abline(h=0, lty=2)
qqnorm(res)
qqline(res)
halfnorm(residuals(meth))
plot(deaths ~ log(pop), data=mort, cex = .2)
prs  <- predict(meth, type="response", se.fit=TRUE)
pris <- data.frame("pest"=prs[[1]], "lwr"=prs[[1]]-prs[[2]], "upr"=prs[[1]]+prs[[2]])
points(pris$pest ~ log(mort$pop), col="red", cex = .1)
points(pris$lwr  ~ log(mort$pop), col="pink", pch=19, cex = .1)
points(pris$upr  ~ log(mort$pop), col="pink", pch=19, cex =.1)
par(mfrow = c(2,2))
plot(meth)
output <- data.frame(resid = resid(meth)[2], fitted = fitted(meth))
ggplot(output, aes(fitted, resid)) + geom_jitter(position=position_jitter(width=.25), alpha=.5) + stat_quantile(method="rq") + science_theme + theme_bw(base_size = 12, base_family = "Helvetica")


(est <- cbind(Estimate = coef(meth), confint(meth)))
#Estimate      2.5 %     97.5 %
#(Intercept) -7.8286887 -7.8779394 -7.7792351
#ethMaori     0.6445377  0.5690826  0.7199089

round(exp(est),2)
#Estimate 2.5 % 97.5 %
#(Intercept)     0.00  0.00   0.00
#ethMaori        1.91  1.77   2.05

with(mort, tapply(deaths, eth, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))
#ethM  M (SD) = 0.99 (1.88)"
#ethNM M (SD) = 1.70 (2.13)"

mgen <- glm.nb(deaths ~ offset(log(da$lpop)) + gen , data = da)
par(mfrow = c(2,2))
res <- residuals(mgen, type="deviance")
plot(predict(mgen), res)
abline(h=0, lty=2)
qqnorm(res)
qqline(res)
halfnorm(residuals(mgen))
plot(deaths ~ log(pop), data=mort, cex = .2)
prs  <- predict(mgen, type="response", se.fit=TRUE)
pris <- data.frame("pest"=prs[[1]], "lwr"=prs[[1]]-prs[[2]], "upr"=prs[[1]]+prs[[2]])
points(pris$pest ~ log(mort$pop), col="red", cex = .1)
points(pris$lwr  ~ log(mort$pop), col="pink", pch=19, cex = .1)
points(pris$upr  ~ log(mort$pop), col="pink", pch=19, cex =.1)
par(mfrow = c(2,2))
plot(mgen)
output <- data.frame(resid = resid(mgen)[2], fitted = fitted(mgen))
ggplot(output, aes(fitted, resid)) + geom_jitter(position=position_jitter(width=.25), alpha=.5) + stat_quantile(method="rq") + science_theme + theme_bw(base_size = 12, base_family = "Helvetica")
(est <- cbind(Estimate = coef(mgen), confint(mgen)))
#             Estimate      2.5 %     97.5 %
#(Intercept) -7.8600175 -7.9192317 -7.8007579
#genMa        0.5539891  0.4750682  0.6329781
exp(est)
#Estimate        2.5 %       97.5 %
#(Intercept) 0.0003858671 0.0003636816 0.0004094245
#genMa       1.7401809523 1.6081238425 1.8832106858

with(mort, tapply(deaths, gen, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))
#Fe M (SD) = 0.95 (1.43)
#Ma M (SD) = 1.73 (2.44)"

mquin <- glm.nb(deaths ~ offset(log(da$pop)) + quin , data = da)
par(mfrow = c(2,2))
res <- residuals(mquin, type="deviance")
plot(log(predict(mquin)), res)
abline(h=0, lty=2)
qqnorm(res)
qqline(res)
halfnorm(residuals(mquin))
plot(deaths ~ log(pop), data=mort, cex = .2)
prs  <- predict(mquin, type="response", se.fit=TRUE)
pris <- data.frame("pest"=prs[[1]], "lwr"=prs[[1]]-prs[[2]], "upr"=prs[[1]]+prs[[2]])
points(pris$pest ~ log(mort$pop), col="red", cex = .1)
points(pris$lwr  ~ log(mort$pop), col="pink", pch=19, cex = .1)
points(pris$upr  ~ log(mort$pop), col="pink", pch=19, cex =.1)
par(mfrow = c(2,2))
plot(mquin)
output <- data.frame(resid = resid(mquin)[2], fitted = fitted(mquin))
ggplot(output, aes(fitted, resid)) + geom_jitter(position=position_jitter(width=.25), alpha=.5) + stat_quantile(method="rq") + science_theme + theme_bw(base_size = 12, base_family = "Helvetica")

(est <- cbind(Estimate = coef(mquin), confint(mquin)))
#Estimate       2.5 %     97.5 %
#(Intercept) -8.0011587 -8.10420465 -7.8981172
#quinQ2       0.2089312  0.06795939  0.3499504
#quinQ3       0.3773562  0.24162591  0.5132009
#quinQ4       0.4835344  0.35137337  0.6158727
#quinQ5       0.8802481  0.75180090  1.0089389

exp(est)
#Estimate        2.5 %       97.5 %
#(Intercept) 0.0003350742 0.0003022655 0.0003714422
#quinQ2      1.2323601815 1.0703218404 1.4189971221
#quinQ3      1.4584237244 1.2733177699 1.6706301759
#quinQ4      1.6217963283 1.4210177920 1.8512715773
#quinQ5      2.4114979375 2.1208159584 2.7426892394

with(mort, tapply(deaths, quin, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

#Q1 M (SD) = 0.78 (1.41)
#Q2 M (SD) = 0.94 (1.54)
#Q3 M (SD) = 1.18 (1.67)
#Q4 M (SD) = 1.50 (1.94)
#Q5 M (SD) = 2.33 (2.91)

mdhb <- glm.nb(deaths ~ offset(log(da$pop)) + dhb , data = da)
par(mfrow = c(2,2))
res <- residuals(mdhb, type="deviance")
plot(log(predict(mdhb)), res)
abline(h=0, lty=2)
qqnorm(res)
qqline(res)
halfnorm(residuals(mdhb))
plot(deaths ~ log(pop), data=mort, cex = .2)
prs  <- predict(mdhb, type="response", se.fit=TRUE)
pris <- data.frame("pest"=prs[[1]], "lwr"=prs[[1]]-prs[[2]], "upr"=prs[[1]]+prs[[2]])
points(pris$pest ~ log(mort$pop), col="red", cex = .1)
points(pris$lwr  ~ log(mort$pop), col="pink", pch=19, cex = .1)
points(pris$upr  ~ log(mort$pop), col="pink", pch=19, cex =.1)
par(mfrow = c(2,2))
plot(mdhb)
output <- data.frame(resid = resid(mdhb)[2], fitted = fitted(mdhb))
ggplot(output, aes(fitted, resid)) + geom_jitter(position=position_jitter(width=.25), alpha=.5) + stat_quantile(mdhbod="rq") + science_theme + theme_bw(base_size = 12, base_family = "Helvetica")
(est <- cbind(Estimate = coef(mdhb), confint(mdhb)))
#Estimate       2.5 %       97.5 %
#(Intercept) -7.5223400 -7.59609985 -7.448069301
#dhbN2        0.1640926  0.05910162  0.269077772
#dhbN3       -0.1245253 -0.24058161 -0.008718887
#dhbS1       -0.1672060 -0.27944001 -0.055033454

exp(est)
#Estimate        2.5 %       97.5 %
#(Intercept) 0.0005408655 0.0005024071 0.0005825653
#dhbN2       1.1783234091 1.0608830376 1.3087569218
#dhbN3       0.8829159617 0.7861704823 0.9913190122
#dhbS1       0.8460253195 0.7562070901 0.9464534843

with(mort, tapply(deaths, dhb, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))
#N1 M (SD) = 1.88 (2.47)
#N2 M (SD) = 1.63 (2.26)
#N3 M (SD) = 0.74 (1.12)
#S1 M (SD) = 1.11 (1.86)

myear <- glm.nb(deaths ~ offset(da$lpop) + gen , data = da)
par(mfrow = c(2,2))
res <- residuals(myear, type="deviance")
plot(log(predict(myear)), res)
abline(h=0, lty=2)
qqnorm(res)
qqline(res)
halfnorm(residuals(myear))
plot(deaths ~ log(pop), data=mort, cex = .2)
prs  <- predict(myear, type="response", se.fit=TRUE)
pris <- data.frame("pest"=prs[[1]], "lwr"=prs[[1]]-prs[[2]], "upr"=prs[[1]]+prs[[2]])
points(pris$pest ~ log(mort$pop), col="red", cex = .1)
points(pris$lwr  ~ log(mort$pop), col="pink", pch=19, cex = .1)
points(pris$upr  ~ log(mort$pop), col="pink", pch=19, cex =.1)
par(mfrow = c(2,2))
plot(myear)
output <- data.frame(resid = resid(myear)[2], fitted = fitted(myear))
ggplot(output, aes(fitted, resid)) + geom_jitter(position=position_jitter(width=.25), alpha=.5) + stat_quantile() + science_theme + theme_bw(base_size = 12, base_family = "Helvetica")
(est <- cbind(Estimate = coef(myear), confint(myear)))
exp(est)
with(mort, tapply(deaths, year, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

screenreg(list(glm.null,glm.age,glm.gen,glm.eth,glm.quin,glm.dhb), custom.model.names = c("   Null", "    Age", "   Gender", "   Ethnicity", "   Quintile", "    DHB"))
texreg(list(glm.null,glm.age,glm.gen,glm.eth,glm.quin,glm.dhb), dcolumn = T, custom.model.names = c("   Null", "    Age", "   Gender", "   Ethnicity", "   Quintile", "    DHB"))
glm.null$deviance
glm.null$null.deviance

glm.age$deviance
glm.age$null.deviance

glm.eth$deviance
glm.eth$null.deviance


mage1   <- glht(mage, linfct = mcp(age = "Tukey"))
mdhb1   <- glht(mdhb, linfct = mcp(dhb = "Tukey"))
meth1   <- glht(meth, linfct = mcp(eth = "Tukey"))
mgen1   <- glht(mgen, linfct = mcp(gen = "Tukey"))
myear1  <- glht(myear, linfct = mcp(year = "Tukey"))
mquin1  <- glht(mquin, linfct = mcp(quin = "Tukey"))

da$age  <- relevel(da$age, "C")
da$eth  <- relevel(da$eth, "NonMaori")
da$gen  <- relevel(da$gen, "Female")
da$quin <- relevel(da$quin, "Q5")
da$dhb  <- relevel(da$dhb, "S1")
da$year <- relevel(da$year, "2002")

mnull   <- glm.nb(deaths ~ offset(log(pop)), data = da)
mage    <- glm.nb(deaths ~ offset(log(da$pop)) + age  , data = da)
meth    <- glm.nb(deaths ~ offset(log(da$pop)) + eth  , data = da)
mgen    <- glm.nb(deaths ~ offset(log(da$pop)) + gen  , data = da)
mquin   <- glm.nb(deaths ~ offset(log(da$pop)) + quin , data = da)
mdhb    <- glm.nb(deaths ~ offset(log(da$pop)) + dhb  , data = da)
myear   <- glm.nb(deaths ~ offset(log(da$pop)) + year , data = da)

mnull   <- glm.nb(deaths ~ offset(log(pop)), data = da)
mage    <- glm.nb(deaths ~ offset(log(da$pop)) + age  + 0 , data = da)
meth    <- glm.nb(deaths ~ offset(log(da$pop)) + eth  + 0 , data = da)
mgen    <- glm.nb(deaths ~ offset(log(da$pop)) + gen  + 0 , data = da)
mquin   <- glm.nb(deaths ~ offset(log(da$pop)) + quin + 0 , data = da)
mdhb    <- glm.nb(deaths ~ offset(log(da$pop)) + dhb  + 0 , data = da)
myear   <- glm.nb(deaths ~ offset(log(da$pop)) + year + 0 , data = da)


mage1 <- glht(mage, linfct = mcp(age = "Tukey"))
mdhb1 <- glht(mdhb, linfct = mcp(dhb = "Tukey"))
meth1 <- glht(meth, linfct = mcp(eth = "Tukey"))
mgen1 <- glht(mgen, linfct = mcp(gen = "Tukey"))
myear1 <- glht(myear, linfct = mcp(year = "Tukey"))
mquin1 <- glht(mquin, linfct = mcp(quin = "Tukey"))
plot(mage1)
plot(margins(glm.age)[[1]])
plot(mdhb1)
plot(margins(mdhb)[[1]])
plot(meth1)
plot(margins(meth)[[1]])
plot(mgen1)
plot(margins(mgen)[[1]])
plot(myear1)
plot(margins(myear)[[1]])
plot(mquin1)
plot(margins(mquin)[[1]])
c("Model", "res dev", "res df")
resdev = mnull$deviance
resdf = mnull$df.residual
1-pchisq(resdev,resdf)
c("mnull", resdev,resdf)
resdev = mage$deviance
resdf = mage$df.residual
1-pchisq(resdev,resdf)
c("mage", resdev,resdf)
resdev = meth$deviance
resdf = meth$df.residual
1-pchisq(resdev,resdf)
c("meth", resdev,resdf)
resdev = mgen$deviance
resdf = mgen$df.residual
1-pchisq(resdev,resdf)
c("mgen", resdev,resdf)
resdev = mdhb$deviance
resdf = mdhb$df.residual
1-pchisq(resdev,resdf)
c("mdhb", resdev,resdf)
resdev = myear$deviance
resdf = myear$df.residual
1-pchisq(resdev,resdf)
c("myear", resdev,resdf)
resdev = mquin$deviance
resdf = mquin$df.residual
1-pchisq(resdev,resdf)
c("mquin", resdev,resdf)

m1 <- lmeNBBayes(da$deaths ~ da$age + offset(log(da$pop)), 
                 data = da, ID = da$year|da$dhb, B = 10000, burnin = 500,
                 printFreq = 50000, M = NULL, probIndex = FALSE,
                 thin = 1,labelnp=NULL, epsilonM = 1e-4,
                 para = list(mu_beta = NULL,Sigma_beta = NULL,
                             max_aG=30,mu_lnD=NULL,sd_lnD=NULL),
                 DP=TRUE,thinned.sample=FALSE, proposalSD = NULL)

m2 <- glmer.nb(da$deaths ~ da$age + offset(log(da$pop)) + (1 | da$year), data =  da)



num.stems<-c(43184,4529,954,283,105,41,25,11,4,3,1)
sum(num.stems)
aphid.data<-rep(0:10,num.stems)
aphid.data
barplot(num.stems)
names(num.stems)<-0:10
barplot(num.stems)
dpois(aphid.data,lambda=1)
log(dpois(aphid.data,lambda=1))
sum(log(dpois(aphid.data,lambda=1)))
poisson.LL<-function(lambda) sum(log(dpois(aphid.data,lambda)))
poisson.LL(1)
poisson.LL(2)
poisson.LL(seq(1,50,1))
poisson.LL(1:2)
poisson.LL(1)
poisson.LL(2)
dpois(aphid.data,1)
dpois(aphid.data,2)
dpois(aphid.data,1:2)
poisson.LL(1)
poisson.LL(2)
sapply(1:2,poisson.LL)
plot(seq(0,5,1), sapply(seq(0,5,1), poisson.LL), type='l', xlab=expression(lambda), ylab='log-likelihood')
poisson.L<-function(lambda) prod(dpois(aphid.data,lambda))
plot(seq(0,5,1), sapply(seq(0,5,1), poisson.L), type='l', xlab=expression(lambda), ylab='likelihood')
poisson.negloglik<-function(lambda) -poisson.LL(lambda)
out.pois <- nlm(poisson.negloglik,3)
out.pois 
exp<-dpois(0:10,out.pois$estimate)*50
max(exp)
max(table(aphid.data))
sum(dpois(0:10,out.pois$estimate))
#P(X>9) = 1 - P(X <= 9)
1-ppois(10,out.pois$estimate)
#choice 1: modify observed and expected so that there are 11 categories
obs.counts<-c(num.stems,0)
exp.counts<-c((dpois(0:10,out.pois$estimate)),1-ppois(10,out.pois$estimate))*50
rbind(obs.counts, exp.counts)
obs.counts 
exp.counts 
#choice 2: add NB tail to last observed category
exp.pois<-c((dpois(0:9,out.pois$estimate)), 1-ppois(9,out.pois$estimate))*50
rbind(num.stems, exp.pois)
num.stems 
exp.pois  
sum(exp.pois)
out.bar<-barplot(num.stems)
out.bar
#relabel bars
out.bar<-barplot(num.stems, names.arg=c(0:9,'10+'))
points(out.bar, exp.pois, pch=16, cex=.9, type='o')
legend('topright', 'Poisson model', pch=16, col=1, lty=1, cex=.9, bty='n')
#negative binomial log-likelihood
NB.LL<-function(mu,theta) sum(log(dnbinom(aphid.data, mu=mu, size=theta)))
#alternative vector version
NBvec.pos<-function(p) sum(log(dnbinom(aphid.data, mu=p[1], size=p[2])))
#first function
NB.LL(3,4)
#incorrect use of 2nd function with two separate arguments
NBvec.pos(3,4)
#correct usage in which arguments are entered as a vector
NBvec.pos(c(3,4))


#negative log-likelihood for nlm
negNB.LL<-function(p) -NB.LL(p[1],p[2])
negNB.LL<-function(p){
mu<-p[1]
theta<-p[2]
LL<-sum(log(dnbinom(aphid.data, mu=mu, size=theta)))
-LL
}
out.NB <- nlm(negNB.LL, c(3,4), hessian=TRUE)
out.NB
optim(c(3,4), negNB.LL)
dnbinom(0:9, mu=out.NB$estimate[1], size=out.NB$estimate[2])
dnbinom(0:9, mu=out.NB$estimate[1], size=out.NB$estimate[2])*50
#anything left over?
sum(dnbinom(0:9,mu=out.NB$estimate[1], size=out.NB$estimate[2])*50)
NB.p <-c (dnbinom(0:9, mu=out.NB$estimate[1], size=out.NB$estimate[2]), 1-pnbinom(8, mu=out.NB$estimate[1], size=out.NB$estimate[2]))
NB.p*50->exp.NB
out.bar<-barplot(num.stems, ylim=c(0, max(c(exp.NB, exp.pois, num.stems))))
#add negative binomial
points(out.bar, exp.NB, col=2, pch=22, cex=.9, type='o')
#add poisson
points(out.bar, exp.pois, col=1, pch=16, cex=.9, type='o')
legend('topright', c('negative binomial', 'Poisson'), col=c(2,1), lty=1, pch=c(22,16), bty='n', cex=.8)


#### R code from lecture 10 ####

#read in slug data set
slugs<-read.table('http://www.bio.ic.ac.uk/research/mjcraw/statcomp/data/slugsurvey.txt',header=T)
slugtable<-data.frame(table(slugs$slugs,slugs$field))

#slugtable <- data.frame(table(mort$deaths,mort$gen))
#convert the category labels to character data and then to numbers
as.numeric(as.character(slugtable$Var1))->slugtable$Var1

#Poisson neg LL functions from last time
negpois2.LL<-function(p){
  z<-as.numeric(slugs$field)-1
  mu<-p[1]+p[2]*z
  LL<-sum(log(dpois(slugs$slugs,lambda=mu)))
  -LL
}
negpois1.LL<-function(p){
  z<-as.numeric(slugs$field)-1
  mu<-p[1]
  LL<-sum(log(dpois(slugs$slugs,lambda=mu)))
  -LL
}
#Estimate parameters for each model
nlm(negpois1.LL,2)->out.pois1
nlm(negpois2.LL,c(2,1))->out.pois2
slugtable$pred0<-dpois(slugtable$Var1,lambda=out.pois2$estimate[1]+
                         out.pois2$estimate[2]*(slugtable$Var2=='Rookery'))+ (1-ppois(10,lambda=out.pois2$estimate[1]+
                                                                                        out.pois2$estimate[2]* (slugtable$Var2=='Rookery')))* (slugtable$Var1==10)
#calculate predicted counts under Poisson model
slugtable$pred0.count <- slugtable$pred0*table(slugs$field)[as.numeric(slugtable$Var2)]
#add predicted counts to the bar plot of the observed counts


#neg LL functions for three NB models
negNB.LL<-function(p){
  mu<-p[1]
  theta<-p[2]
  LL<-sum(log(dnbinom(slugs$slugs,mu=mu,size=theta)))
  -LL
}

negNB.LL1<-function(p){
  z<-as.numeric(slugs$field)-1
  mu<-p[1]+p[2]*z
  theta<-p[3]
  LL<-sum(log(dnbinom(slugs$slugs,mu=mu,size=theta)))
  -LL
}

negNB.LL2<-function(p){
  z<-as.numeric(slugs$field)-1
  mu<-p[1]
  theta<-p[2]+p[3]*z
  LL<-sum(log(dnbinom(slugs$slugs,mu=mu,size=theta)))
  -LL
}

#Estimate parameters for each model
nlm(negNB.LL,c(2,1))->out.NB
nlm(negNB.LL1,c(2,1,1))->out.NB1

#observe that the single mean NB model has log-likelihood that exceeds either Poisson model
out.NB
out.pois1
out.pois2

#LR statistic
2*(out.pois1$minimum-out.NB$minimum)

#because H0: theta=infinity is a boundary value usual LRtest distribution does not hold
#need mixture of a chi-squared 1 and chi-squared 0 distributions
.5*qchisq(.95,1)+.5
#because LR-test statistic exceeds this value we reject H0


#obtain separate means NB model predicted probabilities and counts
slugtable$pred1<-dnbinom(slugtable$Var1,mu=out.NB1$estimate[1]+
                           out.NB1$estimate[2]*(slugtable$Var2=='Rookery'), size=out.NB1$estimate[3])+ (1-pnbinom(10,mu=out.NB1$estimate[1]+
                                                                                                                    out.NB1$estimate[2]*(slugtable$Var2=='Rookery'), size=out.NB1$estimate[3]))*(slugtable$Var1==10)
slugtable
slugtable$pred1.count <- slugtable$pred1*table(slugs$field)[as.numeric(slugtable$Var2)]
slugtable

#compare separate means Poisson and NB model in same graph
library(lattice)
xyplot(Freq~Var1|Var2,data=slugtable,xlab='Count category',
       panel=function(x,y,subscripts) {
         panel.xyplot(x,y,type='h',lineend=1,col='grey',lwd=10)
         panel.points(x,slugtable$pred0.count[subscripts], pch=16, cex=.7, col=1, type='o')
         panel.points(x,slugtable$pred1.count[subscripts], pch=1, cex=.7, col=2, type='o')
       })

#simulation-based Pearson test of separate means NB model
model.p1<-rbind(slugtable$pred1[1:11],slugtable$pred1[12:22])
n<-table(slugs$field)
replicate(9999,as.vector(sapply(1:2,function(x) rmultinom(1,n[x],model.p1[x,]))) ) -> sim.data
apply(sim.data,2,function(x) sum((x-slugtable$pred1.count)^2/slugtable$pred1.count)) -> pearson
sum((slugtable$Freq-slugtable$pred1.count)^2/slugtable$pred1.count)->actual
pearson<-c(pearson,actual)
pval<-sum(pearson>=actual)/length(pearson)
pval

#fit separate dispersions NB model
nlm(negNB.LL2,c(2,1,1))->out.NB2
out.NB2

#calculate predicted probabilities from separate dispersion NB model
slugtable$pred2<-dnbinom(slugtable$Var1,mu=out.NB2$estimate[1], size=out.NB2$estimate[2]+
                           out.NB2$estimate[3]*(slugtable$Var2=='Rookery'))+ (1-pnbinom(10,mu=out.NB2$estimate[1],
                                                                                        size=out.NB2$estimate[2]+ out.NB2$estimate[3]*(slugtable$Var2=='Rookery')))* (slugtable$Var1==10)
#calculate predicted counts
slugtable$pred2.count <- slugtable$pred2*table(slugs$field)[as.numeric(slugtable$Var2)]
slugtable

#generate panel graph with two negative binomials and observed data,
xyplot(Freq~Var1|Var2,data=slugtable,xlab='Count category',
       panel=function(x,y,subscripts) {
         panel.xyplot(x,y,type='h',lineend=1,col='grey',lwd=10)
         panel.points(x,slugtable$pred1.count[subscripts],pch=16,cex=.7,col=1,type='o')
         panel.points(x,slugtable$pred2.count[subscripts],pch=1,cex=.7,col=2,type='o')
       })


#add legend to graph
xyplot(Freq~Var1|Var2,data=slugtable,xlab='Count category',
       panel=function(x,y,subscripts) {
         panel.xyplot(x,y,type='h',lineend=1,col='grey',lwd=10)
         panel.points(x,slugtable$pred1.count[subscripts],pch=16,cex=.7,col=1,type='o')
         panel.points(x,slugtable$pred2.count[subscripts],pch=1,cex=.7,col=2,type='o')
       }, key=list(x=.6,y=.75,corner=c(0,0),points=list(pch=c(16,1),col=c(1,2),cex=.9),
                   text=list(c('NB:separate mean','NB:separate dispersion'),cex=.8)))

#add legend with Greek letters using paste function
xyplot(Freq~Var1|Var2,data=slugtable,xlab='Count category',
       panel=function(x,y,subscripts) {
         panel.xyplot(x,y,type='h',lineend=1,col='grey',lwd=10)
         panel.points(x,slugtable$pred1.count[subscripts],pch=16,cex=.7,col=1,type='o')
         panel.points(x,slugtable$pred2.count[subscripts],pch=1,cex=.7,col=2,type='o')
       }, key=list(x=.6,y=.75,corner=c(0,0),points=list(pch=c(16,1),col=c(1,2),cex=.9),
                   text=list(c(expression(paste('NB:separate ',mu)),expression(paste('NB:separate ',theta))),cex=.8)))

#do plot again this time with barchart panel function
xyplot(Freq~Var1|Var2,data=slugtable,xlab='Count category',
       panel=function(x,y,subscripts) {
         panel.barchart(x,y,horizontal=F,col='grey',origin=0)
         panel.points(x,slugtable$pred1.count[subscripts],pch=16,cex=.7,col=1,type='o')
         panel.points(x,slugtable$pred2.count[subscripts],pch=1,cex=.7,col=2,type='o')
       },key=list(x=.6,y=.75,corner=c(0,0),points=list(pch=c(16,1),col=c(1,2),cex=.9),
                  text=list(c(expression(paste('NB:separate ',mu)),expression(paste('NB:separate ',theta))),cex=.8)))

###refit negative binomial model to aphid data set

num.stems<-c(6,8,9,6,6,2,5,3,1,4)

#generate raw data from tabulated values
aphid.data<-rep(0:9,num.stems)
NB.LL<-function(mu,theta) sum(log(dnbinom(aphid.data,mu=mu,size=theta)))

#for nlm we need negative LL and a function of a vector
negNB.LL<-function(p) -NB.LL(p[1],p[2])

#obtain MLEs
nlm(negNB.LL,c(4,4))->out.NB
out.NB

### slicing the log-likelihood surface with planes

plot(seq(0.1,10,.1),sapply(seq(0.1,10,.1), function(x) NB.LL(out.NB$estimate[1],x)),xlab=expression(theta),ylab='log-likelihood',type='l')
lines(seq(0.1,10,.1),sapply(seq(0.1,10,.1), function(x) NB.LL(2,x)),col=2)
lines(seq(0.1,10,.1),sapply(seq(0.1,10,.1), function(x) NB.LL(5,x)),col=4,lty=2)

##set-up for 3-D graphs

mu<-seq(2,5,.1)
theta<-seq(1,6,.1)
#generate grid of values
g<-expand.grid(mu,theta)
dim(g)
g[1:4,]
NB.LL<-function(mu,theta) sum(log(dnbinom(aphid.data,mu=mu,size=theta)))
NB.LL.p<-function(p) sum(log(dnbinom(aphid.data,mu=p[1],size=p[2])))
#generate z-coordinate on grid
g$z<-apply(g,1,NB.LL.p)
g[1:4,]
#reorganize z as a matrix same shape as grid
zmat<-matrix(g$z,nrow=length(seq(2,5,.1)))
dim(zmat)

## 3-D graph of log-likelihood: using persp
persp(mu,theta,zmat,ylab='theta',xlab='mu',zlab='log-likelihood')
persp(mu,theta,zmat,ylab='theta',xlab='mu',zlab='log-likelihood',ticktype='detailed')
persp(mu,theta,zmat,ylab='theta',xlab='mu',zlab='log-likelihood',ticktype='detailed',theta=30,phi=30)

### 3-D graph using wireframe from lattice
names(g)
names(g)[1:2]<-c('mu','theta')
names(g)
wireframe(z~mu*theta,data=g)
wireframe(z~mu*theta,data=g,scales=list(arrows=F),drape=T,xlab=expression(mu),ylab=expression(theta))

### contour plots
contour(mu,theta,zmat,xlab=expression(mu),ylab=expression(theta))
contour(mu,theta,zmat,xlab=expression(mu),ylab=expression(theta),nlevels=30)
#add MLE to graph
points(out.NB$estimate[1],out.NB$estimate[2],pch=16,col=2,cex=1.2)
