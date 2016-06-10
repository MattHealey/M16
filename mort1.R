mort <- read.csv("mort.csv")
library(rstan)
library(brms)
rstan_options (auto_write=TRUE)
options (mc.cores=parallel::detectCores ()) # Run on multiple cores
ptm <- proc.time()
## formula: 'trait' specifies fixed effects for ZIH & nonZIH parts. dhb as random effect, and year within dhb with no random intercept
fit1 <- brm(deaths | offset(log(1 + mort$pop)) ~ 0 + trait * dhb + age + (1|dhb) + (0+year|trait), 
           family = zero_inflated_negbinomial, data = mort , chains = 1)
fit2 <- brm(deaths | offset(log(1 + mort$pop)) ~ 0 + year + age + (1|dhb) + (0+year|dhb), family = negbinomial, data = mort, chains = 1)
tproc <- proc.time() - ptm
save(fit1, tproc, file = "mortfit1.RData")
save(fit2, tproc, file = "mortfit2.RData")
rm(mort,fit1,fit2, ptm, tproc)

