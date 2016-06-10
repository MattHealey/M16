setwd("/Users/matt/Desktop/2015 Mort")
options(scipen = 9, digits = 10)
source("DenomsApr16.R")
library(ggplot2)
source("mort functions.R")
library(MASS)
library(fitdistrplus)
library(vcd)
x <- mort
## 1 row, two col for use with base plots
#par(mfrow=c(1,2))

# check fit: bootstrap and plot (from fitdistrplus)
#Descriptive parameters of empirical distribution with skewness-kurtosis plot.
# deaths summary statistics
descdist(x$deaths,boot=500,discrete=T,obs.col="blue", obs.pch = 15, boot.col="red")
#------
#   min:  0   max:  10 
#median:  0 
#  mean:  0.167012617 
#estimated sd:  0.538781274 
#estimated skewness:  5.033040011 
#estimated kurtosis:  41.50339991 

#population summary statistics
descdist(x$pop,boot=500,discrete=T,obs.col="blue", obs.pch = 15, boot.col="red")
#------
#   min:  0   max:  5331 
#median:  137 
#  mean:  396.0808303 
#estimated sd:  682.2744992 
#estimated skewness:  3.228218519 
#estimated kurtosis:  15.32449313 

# fitdist from fitdistrplus
#Bootstrap resampling to simulate uncertainty in parameters of the distribution
summary(nb <- fitdist(x$deaths, "nbinom"))
##Fitting of the distribution ' nbinom ' by maximum likelihood 
#Parameters : 
#  estimate     Std. Error
#size 0.2575535823 0.007966691887
#mu   0.1670507723 0.002367609773
#Loglikelihood:  -22921.06873   AIC:  45846.13746   BIC:  45863.74232 
#Correlation matrix:
#  size               mu
#size  1.0000000000000 -0.0001880892714
#mu   -0.0001880892714  1.0000000000000

#Fitting of the distribution ' pois ' by maximum likelihood 
summary(ps <- fitdist(x$deaths, "pois"))
#Parameters : 
#  estimate     Std. Error
#lambda 0.1670126175 0.001843493009
#Loglikelihood:  -24947.45185   AIC:  49896.9037   BIC:  49905.70613 

# Density and Cumulative density distribution for NB and Pois
plot(nb); plot(ps)

## estimate parameters of distribution of deaths for NB and pois
comp <- list(nb,ps)
lapply(comp,comp.func)
#Parameter values obtained with parametric bootstrap 
#size           mu
#1 0.2468980147 0.1664690862
#2 0.2741132779 0.1659035378
#3 0.2650913591 0.1644453456
#4 0.2617175850 0.1711993234
#5 0.2525756593 0.1665085888
#6 0.2520086357 0.1664364399
#Parameter values obtained with parametric bootstrap 
#lambda
#1 0.1654861366
#2 0.1658731009
#3 0.1659955911
#4 0.1681522880
#5 0.1690088744
#6 0.1657714483
#[[1]]
#Parametric bootstrap medians and 95% percentile CI 
#Median         2.5%        97.5%
#  size 0.2599742094 0.2460888595 0.2840442208
#mu   0.1665416116 0.1634435905 0.1726844065
#[[2]]
#Parametric bootstrap medians and 95% percentile CI 
#Median         2.5%        97.5% 
#  0.1676448665 0.1626599602 0.1703162893

## rootograms and goodness of fit for both
## Negative Binomial
plot(foo<-goodfit(x$deaths,type = "nbinomial"), shade=T)
print(foo);summary(foo)
#Observed and fitted values for nbinomial distribution
#with parameters estimated by `ML' 
#count observed           fitted pearson residual
#0    43184 43204.4626979287   -0.09844611654
#1     4529  4376.7925045099    2.30068976719
#2      954  1082.7063512044   -3.91151172262
#3      283   320.5508643914   -2.09735266591
#4      105   102.7075384608    0.22620432160
#5       41    34.4087210029    1.12366131658
#6       25    11.8625621986    3.81435773554
#7       11     4.1721846840    3.34272002633
#8        4     1.4891655313    2.05753198101
#9        3     0.5375657435    3.35852705820
#10        1     0.1957978669    1.23986746614
#Goodness-of-fit test for nbinomial distribution
#                         X^2 df             P(> X^2)
#Likelihood Ratio 55.83572453  8 0.000000003036440655

## Poisson
plot(foo<-goodfit(x$deaths,type = "pois"), shade=T)
print(foo);summary(foo)
#Observed and fitted values for poisson distribution
#with parameters estimated by `ML' 
#count observed                   fitted pearson residual
#     0    43184 41581.724260660106665455      7.857529313
#     1     4529  6944.672588669874130574    -28.987594907
#     2      954   579.923971664770647294     15.533679774
#     3      283    32.284873392027996886     44.124621556
#     4      105     1.347995298780902518     89.275752039
#     5       41     0.045026444514020617    193.006894338
#     6       25     0.001253330722176663    706.131187555
#     7       11     0.000029903149127577   2011.560452402
#     8        4     0.000000624275399089   5062.526197382
#     9        3     0.000000011584652015  27888.697252495
#    10        1     0.000000000193478305  70384.405531851
#	 Goodness-of-fit test for poisson distribution
#                        X^2 df P(> X^2)
#Likelihood Ratio 4108.60231  9        0
 
## goodness of fit nb
gofstat(nb,chisqbreaks = 0:10, discrete=T)
observed <- table(x$deaths)
foo  <- dnbinom(as.numeric(names(observed)), size = unname(nb$estimate[1]), mu = unname(nb$estimate[2])) * sum(observed)
rootogram(observed, foo)
## goodness of fit pois
gofstat(ps,chisqbreaks = 0:10, discrete=T)
foo  <- dpois(as.numeric(names(observed)), lambda = unname(ps$estimate)) * sum(observed)
rootogram(observed, foo)
#compare nb with poiss
(foo<-distplot(observed, type = "nbinomial"))
(foo<-distplot(observed, type = "poisson"))
(foo<-Ord_plot(observed, type = "nbinomial"))
(foo<-Ord_plot(observed, type = "poisson"))
cdfcomp(list(nb,ps), legendtext=c("NegBin","Poisson"))
denscomp(list(nb,ps), legendtext=c("NegBin","Poisson"))
qqcomp(list(nb,ps), legendtext=c("NegBin","Poisson"))
ppcomp(list(nb,ps), legendtext=c("NegBin","Poisson"))
gofstat(list(nb,ps))
rm(rg,ps,nb,foo,observed,comp)
