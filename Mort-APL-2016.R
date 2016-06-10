library(ggplot2)
library(gridExtra)
library(MASS)
library(fitdistrplus)
library(sjPlot)
library(texreg)
library(lme4)
require(countreg)
require(boot)
## set contrasts. keep as treatment by default for models. Use sum for post-hoc. 
## set ref group for each factor to a priori lowest risk group (from lit!)
contrasts(mort$age) <- contr.treatment(levels(mort$age), base=which(levels(mort$age) == "C"))
contrasts(mort$dhb) <- contr.treatment(levels(mort$dhb), base=which(levels(mort$dhb) == "Auckland"))
contrasts(mort$eth) <- contr.treatment(levels(mort$eth), base=which(levels(mort$eth) == "European or other"))
contrasts(mort$gen) <- contr.treatment(levels(mort$gen), base=which(levels(mort$gen) == "Female"))
contrasts(mort$quin) <- contr.treatment(levels(mort$quin), base=which(levels(mort$quin) == "05"))
contrasts(mort$year) <- contr.treatment(levels(mort$year), base=which(levels(mort$quin) == "2014"))

sjp.setTheme(theme = science_theme)
sjp.frq(mort$deaths,
        geom.colors = "lightblue",
        title = NULL, type = 'hist')
glm.age  <- glm.nb(deaths ~ offset(log(1+pop))  + age,  data = mort)
exp(coef(glm.age))
glm.eth  <- glm.nb(deaths ~ offset(log(1+pop))  + eth,  data = mort)
glm.gen  <- glm.nb(deaths ~ offset(log(1+pop))  + gen,  data = mort)
glm.quin <- glm.nb(deaths ~ offset(log(1+pop))  + quin, data = mort)
glm.dhb  <- glm.nb(deaths ~ offset(log(1+pop))  + dhb,  data = mort)
glm.year <- glm.nb(deaths ~ offset(log(1+pop))  + year, data = mort)
screenreg(glm.age, single = T)

lk <- science_theme + theme(legend.key.size = unit(.5, "lines") , legend.position=c(.15,1), legend.justification = "top")
x <- mp(mort,glm.age,95)
g1 <- ggplot(x, aes(log(1+pop), deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = age), alpha = .5) +
  geom_line(aes(colour = age), size = 2)
nbage <- g1 + lk + coord_cartesian(ylim = c(0,5))
x <-mp(mort,glm.eth,95)
g1 <- ggplot(x, aes(log(1+pop), deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = eth), alpha = .5) +
  geom_line(aes(colour = eth), size = .8)
nbeth <- g1 + lk + coord_cartesian(ylim = c(0,5))
x <-mp(mort,glm.gen,95)
g1 <- ggplot(x, aes(log(1+pop), deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = gen), alpha = .5) +
  geom_line(aes(colour = gen), size = .8)
nbgen <- g1 + lk+ coord_cartesian(ylim = c(0,5))
x <-mp(mort,glm.quin,95)
g1 <- ggplot(x, aes(log(1+pop), deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = quin), alpha = .5) +
  geom_line(aes(colour = quin), size = .8)
nbquin <- g1 + lk+ coord_cartesian(ylim = c(0,12))
x <-mp(mort,glm.dhb,95)
g1 <- ggplot(x, aes(log(1+pop), deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = dhb), alpha = .5) +
  geom_line(aes(colour = dhb), size = .8)
nbdhb <- g1 + lk + coord_cartesian(ylim = c(0,5))
x <-mp(mort,glm.year,95)
g1 <- ggplot(x, aes(log(1+pop), deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = year), alpha = .5) +
  geom_line(aes(colour = year), size = .8)
nbyear <- g1 + lk + coord_cartesian(ylim = c(0,5))
grid.arrange(nbage, nbeth, nbgen, nbquin, nbdhb, nbyear, nrow=2)

lk <- science_theme + theme(legend.key.size = unit(.5, "lines") , legend.position=c(.15,1), legend.justification = "top")
x <- mp(mort,glm.age,95)
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = age), alpha = .6) +
  geom_line(aes(colour = age), size = .8)
nbage <- g1 + lk + coord_cartesian(ylim = c(0,12))
x <-mp(mort,glm.eth,95)
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = eth), alpha = .6) +
  geom_line(aes(colour = eth), size = .8)
nbeth <- g1 + lk + coord_cartesian(ylim = c(0,12))
x <-mp(mort,glm.gen,95)
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = gen), alpha = .6) +
  geom_line(aes(colour = gen), size = .8)
nbgen <- g1 + lk+ coord_cartesian(ylim = c(0,12))
x <-mp(mort,glm.quin,95)
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = quin), alpha = .6) +
  geom_line(aes(colour = quin), size = .8)
nbquin <- g1 + lk+ coord_cartesian(ylim = c(0,12))
x <-mp(mort,glm.dhb,95)
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = dhb), alpha = .6) +
  geom_line(aes(colour = dhb), size = .8)
nbdhb <- g1 + lk + coord_cartesian(ylim = c(0,12))
x <-mp(mort,glm.year,95)
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = year), alpha = .6) +
  geom_line(aes(colour = year), size = .8)
nbyear <- g1 + lk + coord_cartesian(ylim = c(0,12))
grid.arrange(nbage, nbeth, nbgen, nbquin, nbdhb, nbyear, nrow=2)

lk <- science_theme + theme(legend.key.size = unit(.5, "lines") , legend.position=c(.019,1), legend.justification = "top")
fit   <- glm.nb(deaths ~ offset(log(pop)) + age + gen, data = da)
x <- mp(fit)
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = age), alpha = .6) +
  geom_line(aes(colour = age)) + facet_grid(. ~ gen)
nbag <- g1 + lk + coord_cartesian(ylim = c(0,16), xlim = c(0,17000))
g1 <- ggplot(x, aes(lpop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = age), alpha = .6) +
  geom_line(aes(colour = age)) + facet_grid(. ~ gen)
nbag2 <- g1 + lk + coord_cartesian(ylim = c(0,10), xlim = c(4,10))
grid.arrange(nbag,nbag2)

lk <- science_theme + theme(legend.key.size = unit(.5, "lines") , legend.position=c(.039,1), legend.justification = "top")
fit   <- glm.nb(deaths ~ offset(da$lpop) + age + gen, data = da)
x <- mp(fit)
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = gen), alpha = .6) +
  geom_line(aes(colour = gen)) + facet_grid(. ~ age)
nbga1 <- g1 + lk + coord_cartesian(ylim = c(0,14), xlim = c(0,18000))
g1 <- ggplot(x, aes(lpop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = gen), alpha = .6) +
  geom_line(aes(colour = gen)) + facet_grid(. ~ age)
nbga2 <- g1 + lk + coord_cartesian(ylim = c(0,14), xlim = c(3,10))
grid.arrange(nbga1,nbga2)

lk <- science_theme + theme(legend.key.size = unit(.5, "lines") , legend.position=c(.03,1), legend.justification = "top")
fit   <- glm.nb(deaths ~ offset(da$lpop) + age + dhb, data = da)
x <- mp(fit)
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = age), alpha = .6) +
  geom_line(aes(colour = age)) + facet_grid(. ~ dhb)
nbad1 <- g1 + lk + coord_cartesian(ylim = c(0,12), xlim = c(0,18000))
g1 <- ggplot(x, aes(lpop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = age), alpha = .6) +
  geom_line(aes(colour = age)) + facet_grid(. ~ dhb)
nbad2 <- g1 + lk + coord_cartesian(ylim = c(0,12), xlim = c(3,10))
grid.arrange(nbad1,nbad2)

lk <- science_theme + theme(legend.key.size = unit(.5, "lines") , legend.position=c(.03,1), legend.justification = "top")
fit   <- glm.nb(deaths ~ offset(da$lpop) + age + dhb, data = da)
x <- mp(fit)
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = dhb), alpha = .6) +
  geom_line(aes(colour = dhb)) + facet_grid(. ~ age)
nbda1 <- g1 + lk + coord_cartesian(ylim = c(0,12), xlim = c(0,18000))
g1 <- ggplot(x, aes(lpop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = dhb), alpha = .6) +
  geom_line(aes(colour = dhb)) + facet_grid(. ~ age)
nbda2 <- g1 + lk + coord_cartesian(ylim = c(0,12), xlim = c(3,10))
grid.arrange(nbda1,nbda2)

lk <- science_theme + theme(legend.key.size = unit(.5, "lines") , legend.position=c(.03,1), legend.justification = "top")
fit   <- glm.nb(deaths ~ offset(da$lpop) + eth + dhb, data = da)
x <- mp(fit)
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = dhb), alpha = .6) +
  geom_line(aes(colour = dhb)) + facet_grid(. ~ eth)
nbed1 <- g1 + lk + coord_cartesian(ylim = c(0,12), xlim = c(0,15000))
g1 <- ggplot(x, aes(lpop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = dhb), alpha = .6) +
  geom_line(aes(colour = dhb)) + facet_grid(. ~ eth)
nbed2 <- g1 + lk + coord_cartesian(ylim = c(0,12), xlim = c(3,10))
grid.arrange(nbed1,nbed2)

lk <- science_theme + theme(legend.key.size = unit(.5, "lines") , legend.position=c(.04,1), legend.justification = "top")
fit   <- glm.nb(deaths ~ offset(da$lpop) + eth + dhb, data = da)
x <- mp(fit)
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = eth), alpha = .6) +
  geom_line(aes(colour = eth)) + facet_grid(. ~ dhb)
nbde1 <- g1 + lk + coord_cartesian(ylim = c(0,12), xlim = c(0,16000))
g1 <- ggplot(x, aes(lpop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = eth), alpha = .6) +
  geom_line(aes(colour = eth)) + facet_grid(. ~ dhb)
nbde2 <- g1 + lk + coord_cartesian(ylim = c(0,12), xlim = c(3,10))
grid.arrange(nbde1,nbde2)

lk <- science_theme + theme(legend.key.size = unit(.5, "lines") , legend.position=c(.019,1), legend.justification = "top")
fit   <- glm.nb(deaths ~ offset(da$lpop) + age + dhb, data = da)
x <- mp(fit)
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = age), alpha = .6) +
  geom_line(aes(colour = age)) + facet_grid(. ~ dhb)
nbay1 <- g1 + lk + coord_cartesian(ylim = c(0,12), xlim = c(0,18000))
g1 <- ggplot(x, aes(lpop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = age), alpha = .6) +
  geom_line(aes(colour = age)) + facet_grid(. ~ dhb)
nbay2 <- g1 + lk + coord_cartesian(ylim = c(0,12), xlim = c(3,10))
grid.arrange(nbay1,nbay2)

fit   <- glm.nb(deaths ~ offset(da$lpop) + eth + dhb, data = da)
x <- mp(fit)
g1 <- ggplot(x, aes(pop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = eth), alpha = .6) +
  geom_line(aes(colour = eth)) + facet_grid(. ~ dhb)
nbey1 <- g1 + lk + coord_cartesian(ylim = c(0,7), xlim = c(0,18000))
fit   <- glm.nb(deaths ~ offset(da$lpop) + eth + dhb, data = da)
g1 <- ggplot(x, aes(lpop, deaths)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = eth), alpha = .6) +
  geom_line(aes(colour = eth)) + facet_grid(. ~ dhb)
nbey2 <- g1 + lk + coord_cartesian(ylim = c(0,7), xlim = c(5,10))
grid.arrange(nbey1,nbey2)

