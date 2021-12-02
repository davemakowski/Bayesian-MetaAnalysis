# David Makowski
# 2021

library(rjags)

#Data and their summaries
TAB<-read.table("Data_Zhao_Wheat.txt", sep="\t", header=T)
summary(TAB)

###Data###

Y=TAB$Sensitivity
Temp=TAB$TGS
REF=as.numeric(as.factor(as.character(TAB$Site_name)))
Q=length(Y)
K=length(unique(REF))

###Model_1###

modelstring= "
model {
	for (i in 1:Q)
	{
		Y[i]~dnorm(mu[i], tau_y[i])
		mu[i]=MU+B[REF[i]]
		tau_y[i]=1/(sigma_y[REF[i]]^2)
				}
	
	for (j in 1:K)
	{	B[j]~dnorm(0,tau_b) 
		sigma_y[j]~dunif(0,100)
				}
			
			MU~dnorm(0,0.001)
			sigma_b~dunif(0,100)
			tau_b =1/(sigma_b^2)	
						
			}
"
writeLines(modelstring, con="model1.txt")

model<-jags.model('model1.txt', data=list('Y'=Y, 'Q'=Q,'K'=K, 'REF'=REF), n.chains=3, n.adapt=100000)
samples<-coda.samples(model, variable.names=c("MU","sigma_b", "sigma_y"), n.iter=100000, thin=10)
dev.new()
gelman.plot(samples[,1:2])
dev.new()
plot(samples)
summary(samples)
dic.samples(model, 10000)


###Model_2
modelstring= "
model {
	for (i in 1:Q)
	{
		Y[i]~dnorm(mu[i], tau_y[i])
		mu[i]=MU+A*Temp[i]+B[REF[i]]
		tau_y[i]=1/(sigma_y[REF[i]]^2)
			}
	
	for (j in 1:K)
	{	B[j]~dnorm(0,tau_b) 
		sigma_y[j]~dunif(0,100)
				}
			
			MU~dnorm(0,0.001)
			A~dnorm(0,0.001)
			sigma_b~dunif(0,100)
			#sigma_b~ dt(0,1/0.5^2,1)T(0,)
			tau_b =1/(sigma_b^2)				
		
			}
"
writeLines(modelstring, con="model2.txt")

#MCMC runs
model<-jags.model('model2.txt', data=list('Y'=Y, 'Q'=Q,'K'=K, 'Temp'=Temp, 'REF'=REF), n.chains=3, n.adapt=100000)
samples<-coda.samples(model, variable.names=c("MU","A","sigma_b", "sigma_y"), n.iter=100000, thin=10)
dev.new()
gelman.plot(samples[,1:3])
dev.new()
plot(samples)
summary(samples)
dic.samples(model, 10000)

Temp_vec<-seq(5,15,by=1)
Pred<-as.matrix(samples[,1:2])%*%t(cbind(Temp_vec,rep(1,length(Temp_vec))))
Pred_m<-apply(Pred, 2, mean)
Pred_lb<-apply(Pred, 2, quantile, 0.025)
Pred_ub<-apply(Pred, 2, quantile, 0.975)

plot(seq(5, 15, by=1), Pred_m, type="l", ylim=c(-20,20),lwd=2, xlab="Average temperature (°C)", ylab="Senitivity (% of yield change for +1°C)", col="blue")
lines(seq(5, 15, by=1), Pred_lb, lty=2, lwd=2, col="blue")
lines(seq(5, 15, by=1), Pred_ub, lty=2, lwd=2, col="blue")
points(Temp,Y, pch=19, col="red")
abline(h=0, lty=3)


###Model_1bis###

modelstring= "
model {
	for (i in 1:Q)
	{
		Y[i]~dnorm(mu[i], tau_y)
		mu[i]=MU+B[REF[i]]
				}
	
	for (j in 1:K)
	{	B[j]~dnorm(0,tau_b) 
				}
			
			MU~dnorm(0,0.001)
			sigma_y~dunif(0,100)
			tau_y=1/(sigma_y^2)
			sigma_b~dunif(0,100)
			#sigma_b~ dt(0,1/0.5^2,1)T(0,)
			tau_b =1/(sigma_b^2)	
						
			}
"
writeLines(modelstring, con="model1bis.txt")

model<-jags.model('model1bis.txt', data=list('Y'=Y, 'Q'=Q,'K'=K, 'REF'=REF), n.chains=3, n.adapt=100000)
samples<-coda.samples(model, variable.names=c("MU","sigma_b","sigma_y"), n.iter=100000, thin=10)
dev.new()
gelman.plot(samples)
plot(samples)
summary(samples)
dic.samples(model, 10000)

###Model_2bis
modelstring= "
model {
	for (i in 1:Q)
	{
		Y[i]~dnorm(mu[i], tau_y)
		mu[i]=MU+A*Temp[i]+B[REF[i]]
				}
	
	for (j in 1:K)
	{	B[j]~dnorm(0,tau_b) 
				}
			
			MU~dnorm(0,0.001)
			A~dnorm(0,0.001)
			sigma_y~dunif(0,100)
			tau_y=1/(sigma_y^2)
			sigma_b~dunif(0,100)
			#sigma_b~ dt(0,1/0.5^2,1)T(0,)
			tau_b =1/(sigma_b^2)				
		
			}
"
writeLines(modelstring, con="model2bis.txt")

#MCMC runs
model<-jags.model('model2bis.txt', data=list('Y'=Y, 'Q'=Q,'K'=K, 'Temp'=Temp, 'REF'=REF), n.chains=3, n.adapt=100000)
samples<-coda.samples(model, variable.names=c("MU","A","sigma_b", "sigma_y"), n.iter=100000, thin=10)
dev.new()
gelman.plot(samples[,1:3])
dev.new()
plot(samples)
summary(samples)
dic.samples(model, 10000)

Temp_vec<-seq(5,15,by=1)
Pred<-as.matrix(samples[,1:2])%*%t(cbind(Temp_vec,rep(1,length(Temp_vec))))
Pred_m<-apply(Pred, 2, mean)
Pred_lb<-apply(Pred, 2, quantile, 0.025)
Pred_ub<-apply(Pred, 2, quantile, 0.975)
dev.new()
plot(seq(5, 15, by=1), Pred_m, type="l", ylim=c(-20,20),lwd=2, xlab="Average temperature (°C)", ylab="Senitivity (% of yield change for +1°C)", col="blue")
lines(seq(5, 15, by=1), Pred_lb, lty=2, lwd=2, col="blue")
lines(seq(5, 15, by=1), Pred_ub, lty=2, lwd=2, col="blue")
points(Temp,Y, pch=19, col="red")
abline(h=0, lty=3)



