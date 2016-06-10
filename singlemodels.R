get_prior(deaths | offset(log(x$pop)) ~ year + (1|dhb) + (0+year|dhb), family = zero_inflated_negbinomial, data = x)
## age
temp1$year <- as.factor(temp1$year)
x <- aggregate(cbind(deaths,pop)  ~ age + gen + eth + dep + dhb + year, data = mort, sum)
y <- x[x$pop < x$deaths,]
rstan_options (auto_write=TRUE)
options (mc.cores=parallel::detectCores ()) # Run on multiple cores
ptm <- proc.time()
tproc <- proc.time() - ptm
fit1 <- brm(deaths | offset(log(1+x$pop)) ~ age + year + (1|dhb) + (0+year|dhb), family = negbinomial, data = x, chains = 1, iter = 500)
fit2 <- brm(deaths | offset(log(x$pop)) ~ age  + gen + eth + quin, family = negbinomial, data = x, chains = 1, iter = 500)
fit2 <- brm(deaths | offset(log(x$pop)) ~ 0 + main + spec + main:age + spec:dhb , family = zero_inflated_negbinomial, data = x, chains = 1, iter = 500)
fit3 <- brm(deaths | offset(log(x$pop)) ~ age  + gen, family = hurdle_negbinomial, data = x, chains = 1)

a2 <- zeroinfl(deaths ~ offset(log(pop)) + age  | offset(log(pop)) + age, data = x, dist = "negbin")
a3 <- glm.nb(deaths ~ offset(log(pop)) + age + gen, data = x)

x <- aggregate(cbind(deaths,pop)  ~ age + gen, data = mort, sum)

xlim <- c(min(x$pop)*0.95, max(x$pop)*1.05)
ylim <- c(floor(min(x$deaths)*0.95),
          ceiling(max(x$deaths)*1.05))
pois.mod <- glm(deaths ~ pop, data=x, 
                family=poisson(link="log"))
par(mfrow=c(2,2))
plot(pois.mod)
title(outer=TRUE, line = -1,
      main = list("Poisson (log) GLM", 
                  cex=1.25,col="black", font=2)) 

meanPred <- predict(pois.mod, type="response")
UpPred <- qpois(.95, meanPred)
LwPred <- qpois(.05, meanPred)

plotData <- lapply(
  seq(along=x$pop),
  function(i){
    y = seq(ylim[1], ylim[2])
    x = rep(x$pop[i], length(y))
    z0 = rep(0, length(y))
    z = dpois(y, meanPred[i])
    return(list(x=x, y=y, z0=z0, z=z))
  }
)

glmModelPlot(x = x$pop, y=x$deaths,
             xlim=xlim, ylim=ylim,
             meanPred = meanPred, LwPred = LwPred,
             UpPred = UpPred, plotData = plotData,
             main = "Poisson (log) GLM")
## eth

## gen

## dep

glmModelPlot <- function(x, y, xlim,ylim, meanPred,  LwPred, UpPred, 
                         plotData, main=NULL){
  ## Based on code by Arthur Charpentier:
  ## http://freakonometrics.hypotheses.org/9593
  par(mfrow=c(1,1))
  n <- 2
  N <- length(meanPred)
  zMax <- max(unlist(sapply(plotData, "[[", "z")))*1.5
  mat <- persp(xlim, ylim, matrix(0, n, n), main=main,
               zlim=c(0, zMax), theta=-30, 
               ticktype="detailed",box=FALSE)
  C <- trans3d(x, UpPred, rep(0, N),mat)
  lines(C, lty=2)
  C <- trans3d(x, LwPred, rep(0, N), mat)
  lines(C, lty=2)
  C <- trans3d(c(x, rev(x)), c(UpPred, rev(LwPred)),
               rep(0, 2*N), mat)
  polygon(C, border=NA, col=adjustcolor("yellow", alpha.f = 0.5))
  C <- trans3d(x, meanPred, rep(0, N), mat)
  lines(C, lwd=2, col="grey")
  C <- trans3d(x, y, rep(0,N), mat)
  points(C, lwd=2, col="#00526D")
  for(j in N:1){
    xp <- plotData[[j]]$x
    yp <- plotData[[j]]$y
    z0 <- plotData[[j]]$z0
    zp <- plotData[[j]]$z
    C <- trans3d(c(xp, xp), c(yp, rev(yp)), c(zp, z0), mat)
    polygon(C, border=NA, col="light blue", density=40)
    C <- trans3d(xp, yp, z0, mat)
    lines(C, lty=2)
    C <- trans3d(xp, yp, zp, mat)
    lines(C, col=adjustcolor("blue", alpha.f = 0.5))
  }
}

install.packages("glmulti")
library(glmulti)

rma.glmulti <- function(formula, data, ...) {
  rma(as.formula(paste(deparse(formula))), vi, data=data, method="ML", ...)
}
res <- glmulti(yi ~ length + wic + feedback + info + pers + imag + meta, data=dat,
               level=1, fitfunction=rma.glmulti, crit="aicc", confsetsize=128)
