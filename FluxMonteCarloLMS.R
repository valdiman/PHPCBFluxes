# Code to estimate individual PCB fluxes from Portland Harbor
# using 2018 and 2019 water samples
# Air data are not used in these calculations
# Monte Carlo simulation is included
# No needs of R packages

# Flux function
final.result = function(MW.PCB, H0.mean, H0.error, 
         C.PCB.water.mean, C.PCB.water.error, nOrtho.Cl, Kow.mean, Kow.error)
{
# fixed parameters

R <- 8.3144
T <- 298.15

F.PCB.aw <- NULL
for (replication in 1:5) # number of replicates
{

# random parameters

a <- rnorm(1, 0.085, 0.007)
b <- rnorm(1, 1, 0.5)
c <- rnorm(1, 32.7, 1.6)
a2 <- rnorm(1, 0.13, 0.02)
b2 <- rnorm(1, 2.9, 1.2)
c2 <- rnorm(1, 47.8, 4.3)
e <- abs(rnorm(1, e.mean, e.error))
n <- abs(rnorm(1, n.mean, n.error))
H0 <- rnorm(1, H0.mean, H0.error)
Kow <- rnorm(1,Kow.mean, Kow.error)
P <- rnorm(1, P.mean, P.error)
#u <- abs(10^(rnorm(1, u10.mean, u10.error))) #m/s
u <- rweibull(1, shape = e, scale = n) #m/s
C.PCB.water <- abs(rnorm(1, C.PCB.water.mean, C.PCB.water.error)) #ng/m3
C.PCB.air <- abs(rnorm(1, C.PCB.air.mean, C.PCB.air.error)) #pg/m3
DOC <- abs(rnorm(1, 1.5, 0.375)) #mg/L error = 25%
T.water <- rnorm(1, twater.mean, twater.error) #C 
T.air <- rnorm(1, tair.mean, tair.error) #C

# computed values

DeltaUaw <- (a*MW.PCB-b*nOrtho.Cl+c)*1000
K <- 10^(H0)*101325/(R*T)
K.air.water <- K*exp(-(DeltaUaw/R)*(1/(T.water+273.15)-1/T))
K.final <- K.air.water*(T.water+273.15)/(T.air+273.15) # no units

DeltaUoa <- (-a2*MW.PCB+b2*nOrtho.Cl-c2)*1000
DeltaUow <- DeltaUoa + DeltaUaw

Kow.water.t <- 10^(Kow)*exp(-(DeltaUow/R)*(1/(T.water+273.15)-1/T))
Kdoc.t <- 0.06*Kow.water.t

C.PCB.water.f <- C.PCB.water/(1+Kdoc.t*DOC/1000^2)#ng/m3

D.water.air <- 10^(-3)*1013.25*((273.15+T.air)^1.75*((1/28.97)+(1/18.0152))^(0.5))/P/(20.1^(1/3)+9.5^(1/3))^2
D.PCB.air <- D.water.air*(MW.PCB/18.0152)^(-0.5)
V.water.air <- 0.2*u +0.3 #cm/s eq. 20-15
V.PCB.air <- V.water.air*(D.PCB.air/D.water.air)^(2/3) #cm/s
	
visc.water <- 10^(-4.5318-220.57/(149.39-(273.15+T.water)))
dens.water <- (999.83952+16.945176*T.water-7.9870401*10^-3*T.water^2-46.170461*10^-6*3+105.56302*10^-9*T.water^4-280.54253*10^-12*T.water^5)/(1+16.87985*10^-3*T.water)
v.water <- visc.water/dens.water*10000
diff.co2 <- 0.05019*exp(-19.51*1000/(273.15+T.water)/R)
D.PCB.water <- diff.co2*(MW.PCB/44.0094)^(-0.5)
Sc.PCB.water <- v.water/D.PCB.water
Sc.co2.water <- v.water/diff.co2

integrand <-function(u) {((e/u)*(u/n)^e)*exp(-(u/n)^e)*(0.000125*u^1.64)}# cm/s
k600 <- integrate(integrand, lower = 0, upper = Inf) #cm/s

if(u > 5){
  V.PCB.water <- k600$value*(Sc.PCB.water/Sc.co2.water)^(-0.5)  
} else {
  V.PCB.water <- k600$value*(Sc.PCB.water/Sc.co2.water)^(-2/3)
}

mtc.PCB <- ((1/V.PCB.water+1/(V.PCB.air*K.final)))^(-1) #cm/s

F.PCB.aw <- c(F.PCB.aw, mtc.PCB*(C.PCB.water.f)) #ng/m2/d

}

F.PCB.aw

}

pc <- read.csv("Variables congener1.csv") # change 1 to 6
Congener <- pc$Congener
MW.PCB <- pc$MW.PCB
H0.mean <- pc$H0
H0.error <- pc$X
C.PCB.water.mean <- pc$C.PCB.water #pg/L
C.PCB.water.error <- pc$X.2
C.PCB.air.mean <- pc$C.PCB.air #pg/m3
C.PCB.air.error <- pc$X.3
nOrtho.Cl <- pc$nOrtho.Cl
Kow.mean <- pc$Kow
Kow.error <- pc$X.1

meteor <- read.csv("Meteor.csv") # change q from 1 to 6
q <- 1
e.mean <- meteor$e[q]
e.error <- meteor$e.error[q]
n.mean <- meteor$n[q]
n.error <- meteor$n.error[q]
P.mean <- meteor$P[q]
P.error <- meteor$P.error[q]
u10.mean <- meteor$u10[q]
u10.error <- meteor$u10.error[q]
twater.mean <- meteor$twater[q]
twater.error <- meteor$twater.error[q]
tair.mean <- meteor$tair[q]
tair.error <- meteor$tair.error[q]

Num.Congener <- length(Congener)

result <- NULL
for (i in 1:Num.Congener)
{
	result <- rbind(result, final.result(MW.PCB[i], H0.mean[i], H0.error[i], 
         C.PCB.water.mean[i], C.PCB.water.error[i], C.PCB.air.mean[i], C.PCB.air.error[i],
         nOrtho.Cl[i], Kow.mean[i], Kow.error[i]))
}

final.result = data.frame(colSums(result))
#write.csv(final.result, file="Results/Sum/Volatilization/FMCLMV6.csv")

#histogram
hist(as.numeric(final.result[,1]), main = "Volatilization Flux Total PCBs (9/20/10 20:00)",
     xlab = "Volatilization Flux Total PCB (ng/m2/d)", border = "blue", col = "green",
     xlim = c(min(as.numeric(final.result[,1])), max(as.numeric(final.result[,1]))))
abline(v = median(as.numeric(final.result[,1])), col = "blue", lwd = 3)
abline(v = quantile(as.numeric(final.result[,1]), 0.025), col = "red", lwd = 3)
abline(v = quantile(as.numeric(final.result[,1]), 0.975), col = "red", lwd = 3)
abline(v = 0, col = "black", lwd = 3)
