# David Makowski
# 2021

#Data and their summaries
TAB<-read.table("Data_Zhao_Wheat.txt", sep="\t", header=T)
summary(TAB)

#Individual sensitivities by site
dev.new()
par(mfrow=c(1,2))
dotchart(TAB$Sensitivity, labels=TAB$Site_name, pch=19,pt.cex=1.2, cex=0.7, xlim=c(-25,20), xlab="% of yield difference", col="blue")
title("Individual yield sensivity values, by site")
abline(v=0,lty=2)

#Individual sensitivities vs Mean temperature
plot(TAB$TGS, TAB$Sensitivity, xlab="Average temperature (°C)", ylab="% yield difference", pch=19, ylim=c(-25,20), col="blue")
title("Individual yield sensitivity values vs. Average temperature")
abline(h=0, lty=2)

#Mean values, SDs, and number of data by site
Mean=aggregate(TAB$Sensitivity, by=list(TAB$Site_name), mean)
SD=aggregate(TAB$Sensitivity, by=list(TAB$Site_name), sd)
TGSm=aggregate(TAB$TGS, by=list(TAB$Site_name), mean)
Nobs=aggregate(TAB$Sensitivity, by=list(TAB$Site_name), length)

#Mean values by site
dev.new()
par(mfrow=c(1,1))
dotchart(Mean[,2], labels=paste(Mean[,1], " (n=", Nobs[,2], ")", sep=""), pch=19,pt.cex=1.2, xlim=c(-25,20), xlab="% of yield difference", col="blue")
for (i in 1:(dim(Mean)[1])) {
	lines(c(Mean[i,2]-1.96*SD[i,2]/sqrt(Nobs[i,2]),Mean[i,2]+1.96*SD[i,2]/sqrt(Nobs[i,2])),c(i,i))	
}
title("Yield sensivities to +1°C by site")
abline(v=0,lty=2)

dev.new()
#Mean values of sensitivities vs Mean temperature
plot(TGSm[,2], Mean[,2], xlab="Average temperature (°C)", ylab="% yield difference", pch=19, ylim=c(-25,20), col="blue")
for (i in 1:(dim(Mean)[1])) {
	lines(c(TGSm[i,2], TGSm[i,2]), c(Mean[i,2]-1.96*SD[i,2]/sqrt(Nobs[i,2]),Mean[i,2]+1.96*SD[i,2]/sqrt(Nobs[i,2])))
}
title("Yield sensitivities to +1°C vs. Average temperature")


