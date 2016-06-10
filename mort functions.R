## predicted with CIs for plotting
mp <- function(da,m1,ci) {
  newdata1 <- da
  newdata1 <- cbind(newdata1, predict(m1, newdata1, type = "link", se.fit=TRUE))
  newdata2 <- within(newdata1, {
    deaths <- exp(fit)
    LL <- exp(fit - qnorm((100-ci)/200, 0, 1) * se.fit)
    UL <- exp(fit + qnorm((100-ci)/200, 0, 1) * se.fit)
  })
}
## mean/SD
ms <- function(dat, mod){
  cbind(dat, Mean = predict(mod, newdata = dat, type = "response"), 
        SE = predict(mod, newdata = dat, type="response", se.fit = T)$se.fit)
}
## goodness of fit
gf <- function(mod) {
  1 - pchisq(summary(mod)$deviance,summary(mod)$df.residual)
}
## for sjPlot
science_theme = theme(panel.grid.major = element_line(size = 0.5,color = "grey"),
                      axis.line = element_line(size = 0.7, color = "black"),
                      text = element_text(size = 14)) + theme_bw(base_size = 12,base_family = "Helvetica")

## bootstrap compare
comp.func <- function (comp) {
  foo <- bootdist(comp, niter=51)
  print(foo)
  return(summary(foo))
  plot(foo, enhance=TRUE)
}

