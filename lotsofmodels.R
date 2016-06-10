
a1 <-       glm(deaths ~  offset(log(1+pop)) + 1, data = x, family = "poisson")
a1b <-      glm(deaths ~  offset(log(1+pop)) + age, data = x, family = "poisson")
a2 <-    glm.nb(deaths ~  offset(log(1+pop)) + 1, data = x)
a2b <-   glm.nb(deaths ~  offset(log(1+pop)) + age, data = x)
a2c <- zeroinfl(deaths ~  offset(log(1+pop)) + 1     | offset(log(1+pop)) + 1, data = x, dist = "negbin")
a2d <- zeroinfl(deaths ~  offset(log(1+pop)) + 1     | offset(log(1+pop)) + 1, data = x)
a3 <-  zeroinfl(deaths ~  offset(log(1+pop)) + age     | offset(log(1+pop)) + age, data = x, dist = "negbin")
a3b <- zeroinfl(deaths ~  offset(log(1+pop)) + age     | offset(log(1+pop)) + age, data = x)
a4 <-  zeroinfl(deaths ~  offset(log(1+pop)) + age     | offset(log(1+pop)) + eth, data = x, dist = "negbin")
a4b <- zeroinfl(deaths ~  offset(log(1+pop)) + age     | offset(log(1+pop)) + eth, data = x)
a5 <-  zeroinfl(deaths ~  offset(log(1+pop)) + age     | offset(log(1+pop)) + gen, data = x, dist = "negbin")
a5b <- zeroinfl(deaths ~  offset(log(1+pop)) + age     | offset(log(1+pop)) + gen, data = x)
a6 <-  zeroinfl(deaths ~  offset(log(1+pop)) + age     | offset(log(1+pop)) + quin, data = x, dist = "negbin")
a6b <- zeroinfl(deaths ~  offset(log(1+pop)) + age     | offset(log(1+pop)) + quin, data = x)
a7 <-  zeroinfl(deaths ~  offset(log(1+pop)) + age     | offset(log(1+pop)) + dhb  , data = x, dist = "negbin")
a7b <- zeroinfl(deaths ~  offset(log(1+pop)) + age     | offset(log(1+pop)) + dhb  , data = x)
a8 <-  zeroinfl(deaths ~  offset(log(1+pop)) + age     | offset(log(1+pop)) + 1  , data = x, dist = "negbin")
a8b <- zeroinfl(deaths ~  offset(log(1+pop)) + age     | offset(log(1+pop)) + 1  , data = x)
library(doBy)
orderBy(~ AIC, AIC(a1,a1b,a2,a2b,a2c,a2d,a3,a3b,a4,a4b,a5,a5b,a6,a7,a7b,a8,a8b))
a1 <-       glm(deaths ~  offset(log(1+pop)) + 1, data = x, family = "poisson")
a1b <-      glm(deaths ~  offset(log(1+pop)) + age, data = x, family = "poisson")
a2 <-    glm.nb(deaths ~  offset(log(1+pop)) + 1, data = x)
a2b <-   glm.nb(deaths ~  offset(log(1+pop)) + age, data = x)
a2c <- zeroinfl(deaths ~  offset(log(1+pop)) + 1     | offset(log(1+pop)) + 1, data = x, dist = "negbin")
a2d <- zeroinfl(deaths ~  offset(log(1+pop)) + 1     | offset(log(1+pop)) + 1, data = x)
a3 <-  zeroinfl(deaths ~  offset(log(1+pop)) + age     | offset(log(1+pop)) + age, data = x, dist = "negbin")
a3b <- zeroinfl(deaths ~  offset(log(1+pop)) + age     | offset(log(1+pop)) + age, data = x)
a4 <-  zeroinfl(deaths ~  offset(log(1+pop)) + age     | offset(log(1+pop)) + eth, data = x, dist = "negbin")
a4b <- zeroinfl(deaths ~  offset(log(1+pop)) + age     | offset(log(1+pop)) + eth, data = x)
a5 <-  zeroinfl(deaths ~  offset(log(1+pop)) + age     | offset(log(1+pop)) + gen, data = x, dist = "negbin")
a5b <- zeroinfl(deaths ~  offset(log(1+pop)) + age     | offset(log(1+pop)) + gen, data = x)
a6 <-  zeroinfl(deaths ~  offset(log(1+pop)) + age     | offset(log(1+pop)) + quin, data = x, dist = "negbin")
a6b <- zeroinfl(deaths ~  offset(log(1+pop)) + age     | offset(log(1+pop)) + quin, data = x)
a7 <-  zeroinfl(deaths ~  offset(log(1+pop)) + age     | offset(log(1+pop)) + dhb  , data = x, dist = "negbin")
a7b <- zeroinfl(deaths ~  offset(log(1+pop)) + age     | offset(log(1+pop)) + dhb  , data = x)
a8 <-  zeroinfl(deaths ~  offset(log(1+pop)) + age     | offset(log(1+pop)) + 1  , data = x, dist = "negbin")
a8b <- zeroinfl(deaths ~  offset(log(1+pop)) + age     | offset(log(1+pop)) + 1  , data = x)
library(doBy)
orderBy(~ AIC, AIC(a1,a1b,a2,a2b,a2c,a2d,a3,a3b,a4,a4b,a5,a5b,a6,a7,a7b,a8,a8b))

a1 <-       glm(deaths ~  offset(log(1+pop)) + 1, data = x, family = "poisson")
a1b <-      glm(deaths ~  offset(log(1+pop)) + age + eth + gen + quin, data = x, family = "poisson")
a2 <-    glm.nb(deaths ~  offset(log(1+pop)) + 1, data = x)
a2b <-   glm.nb(deaths ~  offset(log(1+pop)) + age + eth + gen + quin, data = x)
a2c <- zeroinfl(deaths ~  offset(log(1+pop)) + 1     | offset(log(1+pop)) + 1, data = x, dist = "negbin")
a2d <- zeroinfl(deaths ~  offset(log(1+pop)) + 1     | offset(log(1+pop)) + 1, data = x)
a3 <-  zeroinfl(deaths ~  offset(log(1+pop)) + age + eth + gen + quin     | offset(log(1+pop)) + age, data = x, dist = "negbin")
a3b <- zeroinfl(deaths ~  offset(log(1+pop)) + age + eth + gen + quin     | offset(log(1+pop)) + age, data = x)
a4 <-  zeroinfl(deaths ~  offset(log(1+pop)) + age + eth + gen + quin     | offset(log(1+pop)) + age + eth, data = x, dist = "negbin")
a4b <- zeroinfl(deaths ~  offset(log(1+pop)) + age + eth + gen + quin     | offset(log(1+pop)) + age + eth, data = x)
a5 <-  zeroinfl(deaths ~  offset(log(1+pop)) + age + eth + gen + quin     | offset(log(1+pop)) + age + gen, data = x, dist = "negbin")
a5b <- zeroinfl(deaths ~  offset(log(1+pop)) + age + eth + gen + quin     | offset(log(1+pop)) + gen + gen, data = x)
a6 <-  zeroinfl(deaths ~  offset(log(1+pop)) + age + eth + gen + quin     | offset(log(1+pop)) + quin + gen, data = x, dist = "negbin")
a6b <- zeroinfl(deaths ~  offset(log(1+pop)) + age + eth + gen + quin     | offset(log(1+pop)) + quin + gen, data = x)
a7 <-  zeroinfl(deaths ~  offset(log(1+pop)) + age + eth + gen + quin     | offset(log(1+pop)) + dhb + gen,data = x, dist = "negbin")
a7b <- zeroinfl(deaths ~  offset(log(1+pop)) + age + eth + gen + quin     | offset(log(1+pop)) + dhb + gen  , data = x)
a8 <-  zeroinfl(deaths ~  offset(log(1+pop)) + age + eth + gen + quin     | offset(log(1+pop)) + 1  , data = x, dist = "negbin")
a8b <- zeroinfl(deaths ~  offset(log(1+pop)) + age + eth + gen + quin     | offset(log(1+pop)) + 1  , data = x)
library(doBy)
orderBy(~ AIC, AIC(a1,a1b,a2,a2b,a2c,a2d,a3,a3b,a4,a4b,a5,a5b,a6,a7,a7b,a8,a8b))

##### R code from lecture 7 ####
num.stems <- table(mort$deaths)
#num.stems<-c(6,8,9,6,6,2,5,3,1,4)
#generate raw data from tabulated values
aphid.data<-rep(0:10,num.stems)
table(aphid.data)
#graph tabulated data
barplot(num.stems)
#add labels
names(num.stems)<-0:10
barplot(num.stems)

#Poisson log-likelihood 
poisson.LL<-function(lambda) sum(log(dpois(aphid.data,lambda)))
poisson.LL(1)
poisson.LL(2)
poisson.LL(3)
poisson.LL(4)

#must use sapply to get correct values when given a vector
poisson.LL(2:3)
sapply(2:3,poisson.LL)
#plot log-likelihood
plot(seq(2,5,.01),sapply(seq(2,5,.01),poisson.LL),type='l',xlab=expression(lambda),ylab='log-likelihood')

#plot likelihood
poisson.L<-function(lambda) prod(dpois(aphid.data,lambda))
plot(seq(2,5,.01),sapply(seq(2,5,.01),poisson.L),type='l',xlab=expression(lambda),ylab='likelihood')

#construct negative log-likelihood for use with mlm
poisson.negloglik<-function(lambda) -poisson.LL(lambda)
#obtain minimum negative log-likelihood
nlm(poisson.negloglik,4)->out.pois
out.pois
#compare to graph of negative log-likelihood
plot(seq(2,5,.01),sapply(seq(2,5,.01),function(x) -poisson.LL(x)),type='l',xlab=expression(lambda),ylab='likelihood')
#can get Hessian matrix returned also
nlm(poisson.negloglik,4,hessian=T)->out.pois

#find x-coordinates of bars in bar plot
barplot(num.stems)->out.bar
out.bar
#we need to include tail probability or things don't sum to 1
dpois(0:9,lambda=out.pois$estimate)
sum(dpois(0:9,lambda=out.pois$estimate))

#probabilities of X=0 to 8 and P(X>8)
c(dpois(0:8,lambda=out.pois$estimate),1-ppois(8,out.pois$estimate))
#now things sum to 1
sum(c(dpois(0:8,lambda=out.pois$estimate),1-ppois(8,out.pois$estimate)))

#expected counts under the Poisson model
c(dpois(0:8,lambda=out.pois$estimate),1-ppois(8,out.pois$estimate))*50->exp.pois
exp.pois
#add expected counts to bar plot
points(out.bar,exp.pois,pch=16,cex=.8,type='o')
#redraw bar plot with expansion of y-axis to accommodate expected counts
barplot(num.stems,ylim=c(0,11))->out.bar
points(out.bar,exp.pois,pch=16,cex=.8,type='o')

#### negative binomial model ####
#log-likelihood for NB model
NB.LL<-function(mu,theta) sum(log(dnbinom(aphid.data,mu=mu,size=theta)))
NB.LL(4,4)
#for nlm we need negative LL and a function of a vector
negNB.LL<-function(p) -NB.LL(p[1],p[2])

#another way of writing the same function--expanded with more detail
negNB.LL<-function(p){
  mu<-p[1]
  theta<-p[2]
  LL<-sum(log(dnbinom(aphid.data,mu=mu,size=theta)))
  -LL
}

#obtain MLEs
nlm(negNB.LL,c(4,4))->out.NB
out.NB
mean(aphid.data)
#can also use the optim function for this
optim(c(4,4),negNB.LL)->out.NB1
out.NB1

#expected negative binomial probabilities
c(dnbinom(0:8,mu=out.NB$estimate[1],size=out.NB$estimate[2]),1-pnbinom(8,mu=out.NB$estimate[1],size=out.NB$estimate[2]))->NB.p
#expected NB counts
NB.p*50->exp.NB
#add predicted counts to the bar plot graph
points(out.bar,exp.NB,col=2,pch=15,cex=.7,type='o')
#add a legend
legend('topright',c('Poisson','negative binomial'),col=1:2,pch=c(16,15),cex=.9,bty='n')
#for chi-squared distribution to hold, < 20% of expected counts should be smaller than 5
#for Poisson 50% are less than 5
abline(h=5,lty=2,col=4)

#Poisson probabilities
c(dpois(0:8,lambda=out.pois$estimate),1-ppois(8,out.pois$estimate))->pois.p

#Pearson chi=squared test for Poisson model
chisq.test(num.stems,p=pois.p)
#regroup categories so expected counts are sufficiently large
pois.p.new<-c(pois.p[1:5],sum(pois.p[6:7]),sum(pois.p[8:10]))
pois.p.new*50
#try again
pois.p.new<-c(sum(pois.p[1:2]),pois.p[3:5],sum(pois.p[6:7]),sum(pois.p[8:10]))
pois.p.new*50
#repeat the grouping with the observed data
pois.obs<-c(sum(num.stems[1:2]),num.stems[3:5],sum(num.stems[6:7]),sum(num.stems[8:10]))
pois.obs
#obtain Pearson statistic
chisq.test(pois.obs,p=pois.p.new)->out.chi
out.chi
names(out.chi)
out.chi$statistic
#calculate p-value with correct degrees of freedom
1-pchisq(out.chi$statistic,df=length(pois.p.new)-1-1)
#simulation-based p-value using all the categories
chisq.test(num.stems,p=pois.p,simulate.p.value=T)
chisq.test(num.stems,p=pois.p,simulate.p.value=T,B=9999)
0.0004998*2001

#parametric goodness of fit test for negative binomial
NB.p
#combine categories to obtain minimal cell sizes
NB.p.new<-c(NB.p[1:5],sum(NB.p[6:7]),sum(NB.p[8:10]))
NB.p.new*50
#repeat with observed values
NB.obs<-c(num.stems[1:5],sum(num.stems[6:7]),sum(num.stems[8:10]))
NB.obs
#obtain Pearson statistic
chisq.test(NB.obs,p=NB.p.new)->chi.NB
chi.NB
names(chi.NB)
#calculate p-value with the correct degrees of freedom
1-pchisq(chi.NB$statistic,length(NB.p.new)-1-2)
#simulation-based p-value using all the categories
chisq.test(num.stems,p=NB.p,simulate.p.value=T,B=9999)
