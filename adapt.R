##### desc
#generate raw data from tabulated values
num.deaths <- c(43184,529,954,283,105,41,25,11,4,3,1)
mort.data<-mort$deaths
#graph tabulated data
barplot(table(mort$deaths))
#Poisson log-likelihood 
poisson.LL<-function(lambda) sum(log(dpois(mort.data,lambda)))
#must use sapply to get correct values when given a vector
#plot log-likelihood
plot(seq(0,.3, .001),sapply(seq(0,.3,.001),poisson.LL),type='l',xlab=expression(lambda),ylab='log-likelihood')
#plot likelihood
poisson.L<-function(lambda) prod(dpois(mort.data,lambda))
plot(seq(0,.3,.001),sapply(seq(0,.3,.001),poisson.L),type='l',xlab=expression(lambda),ylab='likelihood')
#construct negative log-likelihood for use with mlm
poisson.negloglik<-function(lambda) -poisson.LL(lambda)
#obtain minimum negative log-likelihood
nlm(poisson.negloglik,2)->out.pois
out.pois
#compare to graph of negative log-likelihood
plot(seq(0,.3,.001),sapply(seq(0,.3,.001),function(x) -poisson.LL(x)),type='l',xlab=expression(lambda),ylab='likelihood')
#can get Hessian matrix returned also
nlm(poisson.negloglik,1,hessian=T)->out.pois
#find x-coordinates of bars in bar plot
barplot(table(mort$deaths))->out.bar
out.bar
#we need to include tail probability or things don't sum to 1
dpois(0:10,lambda=out.pois$estimate)
sum(dpois(0:10,lambda=out.pois$estimate))
#probabilities of X=0 to 10 and P(X>8)
c(dpois(0:9,lambda=out.pois$estimate),1-ppois(9,out.pois$estimate))
#now things sum to 1
sum(c(dpois(0:9,lambda=out.pois$estimate),1-ppois(9,out.pois$estimate)))
#expected counts under the Poisson model
c(dpois(0:9,lambda=out.pois$estimate),1-ppois(9,out.pois$estimate))*length(mort$deaths)->exp.pois
exp.pois
#add expected counts to bar plot
points(out.bar,exp.pois,pch=16,cex=.8,type='o')
#redraw bar plot with expansion of y-axis to accommodate expected counts
out.bar<-barplot(table(mort$deaths), names.arg=c(0:9,'10+'))
points(out.bar, exp.pois, pch=16, cex=.9, type='o')
legend('top', 'Poisson model', pch=16, col=1, lty=1, cex=.9, bty='n')
#### negative binomial model ####
#log-likelihood for NB model
NB.LL<-function(mu,theta) sum(log(dnbinom(mort.data,mu=mu,size=theta)))
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
mean(mort.data)
#can also use the optim function for this
optim(c(4,4),negNB.LL)->out.NB1
out.NB1
#expected negative binomial probabilities
c(dnbinom(0:9,mu=out.NB$estimate[1],size=out.NB$estimate[2]),1-pnbinom(9,mu=out.NB$estimate[1],size=out.NB$estimate[2]))->NB.p
#expected NB counts
NB.p*length(mort$deaths) -> exp.NB
#add predicted counts to the bar plot graph
points(out.bar,exp.NB,col=2,pch=15,cex=.7,type='o')
#add a legend
legend('top',c('Poisson','Negative Bin'),col=1:2,pch=c(16,15),cex=.9,bty='n')
#for chi-squared distribution to hold, < 20% of expected counts should be smaller than 5
#for Poisson 50% are less than 5
abline(h=5,lty=2,col=4)
#Poisson probabilities
c(dpois(0:9,lambda=out.pois$estimate),1-ppois(9,out.pois$estimate))->pois.p
#Pearson chi=squared test for Poisson model
out.chi <- chisq.test(table(mort$deaths),p=pois.p)
#obtain Pearson statistic
names(out.chi)
out.chi$statistic
#calculate p-value with correct degrees of freedom
1-pchisq(out.chi$statistic,df=length(pois.p.new)-1-1)
#simulation-based p-value using all the categories
#chisq.test(num.deaths,p=pois.p,simulate.p.value=T)
#parametric goodness of fit test for negative binomial
NB.p
NB.obs <- table(mort$deaths)
#combine categories to obtain minimal cell sizes
#obtain Pearson statistic
chisq.test(NB.obs,p=NB.p)->chi.NB
chi.NB
names(chi.NB)
#calculate p-value with the correct degrees of freedom
1-pchisq(chi.NB$statistic,length(NB.p.new)-1-2)
#simulation-based p-value using all the categories
chisq.test(table(mort$deaths),p=NB.p,simulate.p.value=T,B=9999)
NB.p.new<-c(NB.p[1:8],sum(NB.p[9:11]))
NB.merge <- length(mort$deaths)*NB.p.new
#I merge the same categories of the observed counts and carry out the test. The degrees of freedom for the chi-squared statistic is m – 1 – p where m is the number of categories (after merging) and p is the number of estimated parameters. We have two estimated parameters: μ and θ so p = 2.
Oi.merge<-c(num.deaths[1:8], sum(num.deaths[9:11]))
Pearson<-sum((Oi.merge-NB.merge)^2/NB.merge)
Pearson
Pearson.p<-1-pchisq(Pearson,length(Oi.merge)-1-2)
Pearson.p
chisq.NB <- chisq.test(Oi.merge, p=NB.p.new)
names(chisq.NB)
1-pchisq(chisq.NB$statistic, df=chisq.NB$parameter-2)

### NEXT
morttable<-data.frame(table(mort$deaths,mort$gen))
#convert the category labels to character data and then to numbers
as.numeric(as.character(morttable$Var1))->morttable$Var1
#Poisson neg LL functions from last time
negpois2.LL<-function(p){
  z<-as.numeric(mort$gen)-1
  mu<-p[1]+p[2]*z
  LL<-sum(log(dpois(mort$deaths,lambda=mu)))
  -LL
}
negpois1.LL<-function(p){
  z<-as.numeric(mort$gen)-1
  mu<-p[1]
  LL<-sum(log(dpois(mort$deaths,lambda=mu)))
  -LL
}
#Estimate parameters for each model
nlm(negpois1.LL,2)->out.pois1
nlm(negpois2.LL,c(2,1))->out.pois2
morttable$pred0<-dpois(morttable$Var1,lambda=out.pois2$estimate[1]+
                         out.pois2$estimate[2]*(morttable$Var2=='Male'))+ (1-ppois(10,lambda=out.pois2$estimate[1]+
                                                                                        out.pois2$estimate[2]* (morttable$Var2=='Male')))* (morttable$Var1==10)
#calculate predicted counts under Poisson model
morttable$pred0.count <- morttable$pred0*table(mort$gen)[as.numeric(morttable$Var2)]
#add predicted counts to the bar plot of the observed counts


#neg LL functions for three NB models
negNB.LL<-function(p){
  mu<-p[1]
  theta<-p[2]
  LL<-sum(log(dnbinom(mort$deaths,mu=mu,size=theta)))
  -LL
}

negNB.LL1<-function(p){
  z<-as.numeric(mort$gen)-1
  mu<-p[1]+p[2]*z
  theta<-p[3]
  LL<-sum(log(dnbinom(mort$deaths,mu=mu,size=theta)))
  -LL
}

negNB.LL2<-function(p){
  z<-as.numeric(mort$gen)-1
  mu<-p[1]
  theta<-p[2]+p[3]*z
  LL<-sum(log(dnbinom(mort$deaths,mu=mu,size=theta)))
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
morttable$pred1<-dnbinom(morttable$Var1,mu=out.NB1$estimate[1]+
                           out.NB1$estimate[2]*(morttable$Var2=='Male'),
                         size=out.NB1$estimate[3]) +  (1-pnbinom(10,mu=out.NB1$estimate[1] + out.NB1$estimate[2]*(morttable$Var2=='Male'), size=out.NB1$estimate[3]))*(morttable$Var1==10)
morttable
morttable$pred1.count <- morttable$pred1*table(mort$gen)[as.numeric(morttable$Var2)]
morttable

#compare separate means Poisson and NB model in same graph
library(lattice)
xyplot(Freq~Var1|Var2,data=morttable,xlab='Count category',
       panel=function(x,y,subscripts) {
         panel.xyplot(x,y,type='h',lineend=1,col='grey',lwd=10)
         panel.points(x,morttable$pred0.count[subscripts], pch=16, cex=.7, col=1, type='o')
         panel.points(x,morttable$pred1.count[subscripts], pch=1, cex=.7, col=2, type='o')
       })

#simulation-based Pearson test of separate means NB model
model.p1<-rbind(morttable$pred1[1:11],morttable$pred1[12:22])
n<-table(mort$gen)
replicate(9999,as.vector(sapply(1:2,function(x) rmultinom(1,n[x],model.p1[x,]))) ) -> sim.data
apply(sim.data,2,function(x) sum((x-morttable$pred1.count)^2/morttable$pred1.count)) -> pearson
sum((morttable$Freq-morttable$pred1.count)^2/morttable$pred1.count)->actual
pearson<-c(pearson,actual)
pval<-sum(pearson>=actual)/length(pearson)
pval

#fit separate dispersions NB model
nlm(negNB.LL2,c(2,1,1))->out.NB2
out.NB2

#calculate predicted probabilities from separate dispersion NB model
morttable$pred2<-dnbinom(morttable$Var1,mu=out.NB2$estimate[1], size=out.NB2$estimate[2]+
                           out.NB2$estimate[3]*(morttable$Var2=='Male'))+ (1-pnbinom(10,mu=out.NB2$estimate[1],
                                                                                        size=out.NB2$estimate[2]+ out.NB2$estimate[3]*(morttable$Var2=='Male')))* (morttable$Var1==10)
#calculate predicted counts
morttable$pred2.count <- morttable$pred2*table(mort$gen)[as.numeric(morttable$Var2)]
morttable

#generate panel graph with two negative binomials and observed data,
xyplot(Freq~Var1|Var2,data=morttable,xlab='Count category',
       panel=function(x,y,subscripts) {
         panel.xyplot(x,y,type='h',lineend=1,col='grey',lwd=10)
         panel.points(x,morttable$pred1.count[subscripts],pch=16,cex=.7,col=1,type='o')
         panel.points(x,morttable$pred2.count[subscripts],pch=1,cex=.7,col=2,type='o')
       })


#add legend to graph
xyplot(Freq~Var1|Var2,data=morttable,xlab='Deaths',
       panel=function(x,y,subscripts) {
         panel.xyplot(x,y,type='h',lineend=1,col='grey',lwd=10)
         panel.points(x,morttable$pred1.count[subscripts],pch=16,cex=.7,col=1,type='o')
         panel.points(x,morttable$pred2.count[subscripts],pch=1,cex=.7,col=2,type='o')
       }, key=list(x=.6,y=.75,corner=c(0,0),points=list(pch=c(16,1),col=c(1,2),cex=.9),
                   text=list(c('NB:separate mean','NB:separate dispersion'),cex=.8)))

#add legend with Greek letters using paste function
xyplot(Freq~Var1|Var2,data=morttable,xlab='Deaths',
       panel=function(x,y,subscripts) {
         panel.xyplot(x,y,type='h',lineend=1,col='grey',lwd=10)
         panel.points(x,morttable$pred1.count[subscripts],pch=16,cex=.7,col=1,type='o')
         panel.points(x,morttable$pred2.count[subscripts],pch=1,cex=.7,col=2,type='o')
       }, key=list(x=.6,y=.75,corner=c(0,0),points=list(pch=c(16,1),col=c(1,2),cex=.9),
                   text=list(c(expression(paste('NB:separate ',mu)),expression(paste('NB:separate ',theta))),cex=.8)))

#do plot again this time with barchart panel function
xyplot(Freq~Var1|Var2,data=morttable,xlab='Deaths',
       panel=function(x,y,subscripts) {
         panel.barchart(x,y,horizontal=F,col='grey',origin=0)
         panel.points(x,morttable$pred1.count[subscripts],pch=16,cex=.7,col=1,type='o')
         panel.points(x,morttable$pred2.count[subscripts],pch=1,cex=.7,col=2,type='o')
       },key=list(x=.6,y=.75,corner=c(0,0),points=list(pch=c(16,1),col=c(1,2),cex=.9),
                  text=list(c(expression(paste('NB:separate ',mu)),expression(paste('NB:separate ',theta))),cex=.8)))

###refit negative binomial model to aphid data set


num.stems<-c(4318,529,954,283,105,41,25,11,4,3,1)
#generate raw data from tabulated values
aphid.data<-rep(0:10,num.stems)
NB.LL<-function(mu,theta) sum(log(dnbinom(aphid.data,mu=mu,size=theta)))

#for nlm we need negative LL and a function of a vector
negNB.LL<-function(p) -NB.LL(p[1],p[2])

#obtain MLEs
nlm(negNB.LL,c(10,10))->out.NB
out.NB

### slicing the log-likelihood surface with planes

 plot(seq(0.1,5,.1),sapply(seq(0.1,5,.1), function(x) NB.LL(out.NB$estimate[1],x)),xlab=expression(theta),ylab='log-likelihood',type='l')
lines(seq(0.1,5,.1),sapply(seq(0.1,5,.1), function(x) NB.LL(.2,x)),col=2)
lines(seq(0.1,5,.1),sapply(seq(0.1,5,.1), function(x) NB.LL(.7,x)),col=4,lty=2)

plot(seq(0.1,2,.1), sapply(seq(0.1,2,.1), function(x) NB.LL(out.NB$estimate[1],x)), xlab=expression(theta), ylab='log-likelihood', type='l')
lines(seq(0.1,2,.1), sapply(seq(0.1,2,.1), function(x) NB.LL(.5,x)), col=2)
lines(seq(0.1,2,.1), sapply(seq(0.1,2,.1), function(x) NB.LL(.8,x)), col=4, lty=2)
legend('top', c(expression(mu=='MLE (0.675)'), expression(mu==.5), expression(mu==.8)), col=c(1,2,4), lty=c(1,1,2), cex=.9, bty='n')

##set-up for 3-D graphs

mu<-seq(.1,1,.1)
theta<-seq(.1,1,.1)
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
zmat<-matrix(g$z,nrow=length(seq(.1,1,.1)))
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

#two-factor interaction model
out4 <-glm.nb(deaths~ age*gen + offset(log(1+pop)), data=mort)
#create a new response without interaction: additive model
#mort$deathsb<- log(1+mort$deaths)/log(1+mort$pop)
#fit 2-factor interaction model to new response
out6 <- glm.nb(deaths ~ age*eth + + offset(log(1+pop)), data=mort)
#create interaction plot: install effects package first
library(effects)
#compare models for response with and without interaction
plot(effect('age:eth', out6), multiline=T, row=1, col=1, nrow=1, ncol=2, more=T, ylab='y')
plot(effect('age:gen' ,out4), multiline=T, row=1, col=2, nrow=1, ncol=2, more=F)
anova(out4)
summary(out4)
###
#more
#### Figure 3 ####

#SSTOTAL graph
par(mar=c(5.1,4.1,1.1,3.5))
set.seed(10)
model<-sapply(seq(1,5,.1),function(x) rnorm(1,1+2*x))
plot(seq(1,5,.1),model,pch=16, xlab='x', ylab='y', type='n')
abline(h=mean(model), lty=2, col=1, lwd=2)
matguy<-cbind(seq(1,5,.1), mean(model), seq(1,5,.1), model)
apply(matguy,1,function(x) segments(x[1], x[2], x[3], x[4], lwd=2, col='dodgerblue'))
points(seq(1,5,.1), model, pch=16, cex=.9)
mtext(side=4,line=.5, expression(y==bar(y)), at=mean(model), cex=.9, col=1, las=2)
legend(1,12,expression(y[i]-bar(y)), lty=1, col='dodgerblue', cex=.95, lwd=2, bty='n')

#SSE graph
par(mar=c(5.1,4.1,1.1,5.1))
plot(seq(1,5,.1), model, pch=16, xlab='x', ylab='y', type='n')
lm(model~seq(1,5,.1))->out
abline(out,lty=2, col=1, lwd=2)
matguy<-cbind(seq(1,5,.1), fitted(out), seq(1,5,.1), model)
apply(matguy,1,function(x) segments(x[1], x[2], x[3], x[4], lwd=2, col='dodgerblue'))
points(seq(1,5,.1), model, pch=16, cex=.9)
mtext(side=4, line=.5, "regression\n line", at=coef(out)[1]+coef(out)[2]*5.2, cex=.8, col=1, las=2)
legend(1,12, expression(y[i]-hat(y)[i]), lty=1, col='dodgerblue', cex=.95, lwd=2, bty='n')

##### Figure 4 #####

#interaction absent
means1<-c(10,15,15,20)
x<-c(1,2,1,2)
plot(x,means1,xlim=c(0.5,2.5),ylim=c(9,21),type='n',axes=F, 
     xlab='Factor A', ylab='Response')
axis(1,at=1:2,labels=c('Low','High'))
axis(2)
box()

arrows(1.5,12.5,1.5,17.5,angle=30,length=.1,code=3)
text(1.5,16,'Main Effect\n     of B',cex=.75,pos=4)
segments(1,10,2,10,lty=2)
arrows(2,10,2,15,angle=30,length=.1,code=3)
text(2,12.5,'Main Effect\n     of A',cex=.75,pos=4)
points(x[1:2],means1[1:2],pch=16,cex=1.2,col=2)
segments(x[1],means1[1],x[2],means1[2],col=2)
points(x[3:4],means1[3:4],pch=15,cex=1.2,col=4)
segments(x[3],means1[3],x[4],means1[4],col=4)
text(1,10,'Low B',col=2,cex=.85,pos=2)
text(1,15,'High B',col=4,cex=.85,pos=2)
mtext(side=3,line=.5,'No 2-factor interaction',cex=.9)

#interaction present

means1<-c(10,15,15,11)
plot(x,means1,xlim=c(0.5,2.5),ylim=c(9,16),type='n',axes=F, 
     xlab='Factor A', ylab='Response')
axis(1,at=1:2,labels=c('Low','High'))
axis(2)
box()

points(x[1:2],means1[1:2],pch=16,cex=1.2,col=2)
segments(x[1],means1[1],x[2],means1[2],col=2)
points(x[3:4],means1[3:4],pch=15,cex=1.2,col=4)
segments(x[3],means1[3],x[4],means1[4],col=4)

text(1,10,'Low B',col=2,cex=.85,pos=2)
text(1,15,'High B',col=4,cex=.85,pos=2)
mtext(side=3,line=.5,'2-factor interaction present',cex=.9)




##### Figure 6 #####

plot(y~x5,data=small.dat,xlim=c(.8,3.2),axes=F,ylim=c(8,22))
box()
axis(1,at=1:3,labels=c('L','M','H'))
axis(2)
text(1,small.dat$y[1],expression(beta[0]),pos=1)
text(1,small.dat$y[2],expression(beta[0]+beta[1]),pos=3)
text(2,small.dat$y[3],expression(beta[0]+beta[2]),pos=1)
text(2,small.dat$y[4],expression(beta[0]+beta[2]+beta[1]+beta[4]),pos=3)
text(3,small.dat$y[5],expression(beta[0]+beta[3]),pos=1)
text(3,small.dat$y[6],expression(beta[0]+beta[3]+beta[1]+beta[5]),pos=3)
lines(1:3,small.dat$y[c(2,4,6)],lty=2)
lines(1:3,small.dat$y[c(1,3,5)],lty=1)
arrows(1,small.dat$y[1]+.2,1,small.dat$y[2]-.2,angle=45, length=.1, code=3, lwd=2, col=2)
arrows(2,small.dat$y[3]+.2,2,small.dat$y[4]-.2,angle=45, length=.1, code=3, lwd=2, col=2)
arrows(3,small.dat$y[5]+.2,3,small.dat$y[6]-.2,angle=45, length=.1, code=3, lwd=2, col=2)
text(1,(small.dat$y[1]+small.dat$y[2])/2,expression(beta[1]),pos=4,col=2)
text(2,(small.dat$y[3]+small.dat$y[4])/2,expression(beta[1]+beta[4]),pos=4,col=2)
text(3,(small.dat$y[5]+small.dat$y[6])/2,expression(beta[1]+beta[5]),pos=4,col=2)
legend('topleft',LETTERS[1:2],lty=1:2, bty='n', cex=.9, lwd=2, title=expression(x[4]))


p <- ggplot(mort, aes(log(pop), deaths, colour = age)) + geom_point()
p + facet_grid(. ~ age)
p + facet_grid(age ~ .)
p + facet_grid(year ~ .)
p + facet_grid(age ~ year, margins = T)

ggplot(mort, aes(deaths, gen)) +
  geom_point() +
  facet_grid(year ~ ., scales = "free", space = "free") +
  theme(strip.text.y = element_text(angle = 0))

ggplot(mort, aes(deaths, pop, colour = age)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "glm.nb")

ggplot(mort, aes(pop, deaths)) +
  geom_point() +
  geom_smooth(span = 0.8) +
  facet_wrap(~year)