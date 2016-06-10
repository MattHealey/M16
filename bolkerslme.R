## primary GLMM-fitting packages:
library(lme4)
library(glmmADMB)
library(MCMCglmm)
library(blme)
library(MASS)          ## for glmmPQL
library(nlme)          ## for intervals(), tundra example
## auxiliary
library(ggplot2)       ## for pretty plots generally
## ggplot customization:
theme_set(theme_bw())
library(grid)          ## for unit()
zmargin <- theme(panel.margin=unit(0,"lines")) ## to squash facets together ...
library(scales)        ## for squish()
library(gridExtra)     ## for grid.arrange()
library(proto)         ## for horizontal line range plot
source("geom-linerangeh.R")  ## for horizontal line ranges
library(coefplot2) ## coefficient plots
library(coda)      ## MCMC diagnostics
library(aods3)     ## overdispersion diagnostics
library(scape) ## pretty plots from MCMC fits
library(bbmle)     ## AICtab
library(pbkrtest)  ## parametric bootstrap
library(Hmisc)
## library(plotrix)  ## for plotCI
## library(emdbook)  ## for curve3d()
## for general-purpose reshaping and data manipulation:
library(reshape2)
library(plyr)
library(numDeriv)
mc1 <- read.csv("tundra.csv",na.strings=c("-","NA"))
ggplot(mc1,aes(x=Year,y=GS.NEE,colour=Site))+geom_point()+
  geom_smooth(method="lm",alpha=0.3)+
  ## oob=squish retains the (truncated) confidence bands;
  ## otherwise bands that went outside the limits would disappear
  ## (requires the 'scales' package be loaded)
  scale_y_continuous(limits=c(-150,400),oob=squish)+
  scale_colour_discrete(guide="none") ## suppress legend

x <- aggregate(cbind(deaths,pop) ~ age + year, data = mort, FUN=sum)
ggplot(mort,aes(x=pop,y=deaths,colour=age))+geom_point()+
  geom_smooth(method="lm",alpha=0.3)+
  ## oob=squish retains the (truncated) confidence bands;
  ## otherwise bands that went outside the limits would disappear
  ## (requires the 'scales' package be loaded)
  scale_y_continuous(limits=c(0,30),oob=squish)+
  scale_colour_discrete(guide="none") ## suppress legend

aggfun <- function(dat,agg=c("Year","Site"),response="Winter.adj",
                  baseYear=min(mc1$Year)) {
  ## select only site, year, and response
  sub1 <- na.omit(dat[,c("Site","Year",response)])
  ## compute means of non-aggregation variables
  agg1 <- aggregate(sub1[!names(sub1) %in% agg],by=sub1[agg],FUN=mean)
  ## compute sample size of non-aggregation variables
  aggn <- aggregate(sub1[response],by=sub1[agg],FUN=length)
  names(aggn)[ncol(aggn)] <- "n"   ## assumes response is last column
  ## put mean and sample size together
  agg2 <- merge(agg1,aggn)
  ## recompute centred year
  agg2$cYear <- agg2$Year - baseYear
  agg2
}
mc2 <- aggfun(mc1,response="GS.NEE")
## center year at the mean rather than the date of
## the first observation:
mc2B <- aggfun(mc1,response="GS.NEE",baseYear=mean(mc1$Year))
ggplot(mc2,aes(x=cYear,y=GS.NEE,colour=Site))+
  geom_point(aes(size=n),alpha=0.7)+
  geom_smooth(method="lm",alpha=0.3,aes(weight=n))+
  scale_y_continuous(limits=c(-150,400),oob=squish)+
  scale_colour_discrete(guide="none")+
  scale_size_continuous(range=c(2,5),breaks=c(1,3,6))
cmod_lme <- lme(GS.NEE ~ cYear,
                data=mc2, method="REML",
                random = ~ 1 + cYear | Site, 
                correlation=corCAR1(form=~cYear|Site),
                weights=varFixed(~I(1/n)),
                control=list(maxIter=10000, niterEM=10000))
summary(cmod_lme)
cmod2_lme <- update(cmod_lme,data=mc2B)
cmod_lmer <- lmer(GS.NEE ~ cYear + (1+cYear|Site),
                  data=mc2B, REML=TRUE,
                  weights=n)
summary(cmod_lmer)
colvec <- c("#ff1111","#007eff") ## second colour matches lattice default
grid.arrange(plot(cmod2_lme,type=c("p","smooth")),
             plot(cmod2_lme,sqrt(abs(resid(.)))~fitted(.),
                  col=ifelse(mc2$Site=="Toolik, AK",colvec[1],colvec[2]),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(cmod2_lme,resid(.,type="pearson")~cYear,
                  type=c("p","smooth")),
             qqnorm(cmod2_lme,abline=c(0,1),
                    col=ifelse(mc2$Site=="Toolik, AK",colvec[1],colvec[2])))
ggplot(mc2,aes(x=cYear,y=GS.NEE,colour=(Site=="Toolik, AK")))+
  geom_point(aes(size=n),alpha=0.7)+
  geom_smooth(method="lm",alpha=0.3,aes(weight=n,group=Site))+
  scale_colour_discrete(name="Site",breaks=0:1,labels=c("other","Toolik"))+
  scale_y_continuous(limits=c(-150,400),oob=squish)+
  scale_size_continuous(range=c(2,5),breaks=c(1,3,6))
grid.arrange(plot(ACF(cmod2_lme),alpha=0.05),
             plot(ACF(cmod2_lme,resType="normalized"),alpha=0.05),
             nrow=1)
