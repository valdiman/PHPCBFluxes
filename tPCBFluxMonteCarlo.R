# Code to estimate total PCB fluxes from Portland Harbor
# using 2018 and 2019 water samples
# The code estimate the flux of each congener, and
# sum them to get total PCB
# Air data are not used in these calculations
# Monte Carlo simulation is included
# No needs of R packages

# Chemical properties -----------------------------------------------------

# Create matrix to storage CP data
cp <- data.frame(matrix(NA, nrow = 160, ncol = 7))

# Add column names
colnames(cp) <- c('Congener', 'MW.PCB', 'nOrtho.Cl', 'H0.mean', 'H0.error',
                  'Kow.mean', 'Kow.error')

# Add PCB names
cp[,1] <- as.factor(c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11',
                      '12+13', '15', '16', '17', '18+30', '19', '20+28', '21+33',
                      '22', '23', '24', '25', '26+29', '27', '31', '32', '34',
                      '35', '36', '37', '38', '39', '40+41+71', '42', '43+73',
                      '44+47+65', '45+51', '46', '48', '49+69', '50+53', '52',
                      '54', '55', '56', '57', '58', '59+62+75', '60', '61+70+74+76',
                      '63', '64', '66', '67', '68', '72', '77', '78', '79', '80',
                      '81', '82', '83+99', '84', '85+116+117', '86+87+97+109+119+125',
                      '88+91', '89', '90+101+113', '92', '93+100', '94', '95',
                      '96', '98+102', '103', '104', '105', '106', '108', '107+124',
                      '110+115', '111', '112', '114', '118', '120', '121', '122',
                      '123', '126', '127', '129+138+163', '130', '131', '132',
                      '133', '134+143', '135+151', '136', '137+164', '139+140',
                      '141', '142', '144', '145', '146', '147+149', '148',
                      '150', '152', '153+168', '154', '155', '156+157', '158',
                      '159', '160', '161', '162', '165', '167', '169', '170',
                      '171+173', '172', '174', '175', '176', '177', '178',
                      '179', '180+193', '181', '182', '183', '184', '185',
                      '186', '187', '188', '189', '190', '191', '192', '194', '195',
                      '196', '197', '198+199', '200', '201', '202', '203', '205',
                      '206', '207', '208', '209'))

# Read chemical properties
cp <- read.csv("ChemicalProperties.csv")
Congener <- cp$Congener
MW.PCB <- cp$MW.PCB
nOrtho.Cl <- cp$nOrtho.Cl
H0.mean <- cp$H0
H0.error <- cp$H0.error
Kow.mean <- cp$Kow
Kow.error <- cp$Kow.error

# Water concentrations ----------------------------------------------------

# Read water concentrations
wc.raw <- read.csv("WaterConcentration.csv")
# Different approaches to use the data
# Prepare data [pg/L] = [ng/m3]
wc.1 <- subset(wc.raw, select = -c(SampleID:Units))
wc.2 <- cbind(wc.raw$LocationID, wc.1)
colnames(wc.2)[1] <- "LocationID"
wc.ave <- sapply(wc.1, mean, na.rm = TRUE)
wc.sd <- sapply(wc.1, sd, na.rm = TRUE)
wd.3 <- data.frame(t(rbind(wc.ave, wc.sd)))
C.PCB.water.mean <- wd.3$wc.ave
C.PCB.water.error <- wd.3$wc.sd

# (1) Mean and standard deviation

# (2) Geometric mean and geometric standard deviation

# (3) Selected samples

# Meteorological data -----------------------------------------------------

# Read meteorological conditions
meteor <- read.csv("Meteorological.csv")
# Columns are the different water sampling periods
# except last columns are the passive sampler deployment
# Columns 2 to 11 definitions: sampl.aug.2018.av,sampl.aug.2018.sd,
# sampl.nov.2018.av, sampl.nov.2018.sd, sampl.jan.2019.av,
# sampl.jan.2019.sd, sampl.feb.2019.av, sampl.feb.2019.sd,
# sampl.sum.2020.av, sampl.sum.2022.sd
# m is column number
m <- 2 # from 2 to 11
tair.mean <- meteor[1, m]
tair.error <- meteor[1, m+1]
twater.mean <- meteor[2, m]
twater.error <- meteor[2, m+1]
u10.mean <- meteor[3, m]
u10.error <- meteor[3, m+1]
P.mean <- meteor[4, m]
P.error <- meteor[4, m +1]

# Flux calculations -------------------------------------------------------

# Flux function
final.result = function(MW.PCB, H0.mean, H0.error, 
         C.PCB.water.mean, C.PCB.water.error, nOrtho.Cl,
         Kow.mean, Kow.error) {
# fixed parameters

R <- 8.3144 # [Pa m3/K/mol]
T <- 298.15 # [K]

F.PCB.aw <- NULL
# number of replicates for Monte Carlo simulation
for (replication in 1:1000) {

# Random parameters
# Parameters for calculating Delta Uaw
a <- rnorm(1, 0.085, 0.007)
b <- rnorm(1, 1, 0.5)
c <- rnorm(1, 32.7, 1.6)
# Parameter for calculating Delta Uoa
a2 <- rnorm(1, 0.13, 0.02) 
b2 <- rnorm(1, 2.9, 1.2)
c2 <- rnorm(1, 47.8, 4.3)
# Henry's law constant 
H0 <- rnorm(1, H0.mean, H0.error) # [Pa m3/mol]
# Octanol-water partition coefficient
Kow <- rnorm(1, Kow.mean, Kow.error) # [Lwater/Loctanol] 
# PCB water concentration
C.PCB.water <- abs(rnorm(1, C.PCB.water.mean, C.PCB.water.error)) # [pg/L]
# DOC (Spencer et al 2012)
DOC <- abs(rnorm(1, 2, 0.3)) # [mg/L]
# Water temperature
T.water <- rnorm(1, twater.mean, twater.error) # [C]
# Air temperature
T.air <- rnorm(1, tair.mean, tair.error) # [C]
# atmospheric pressure
P <- rnorm(1, P.mean, P.error) # [mbar]
# Wind speed @10 m
u <- abs(rnorm(1, u10.mean, u10.error)) # [m/s] missing

# Computed values
# Henry's law constant (HLC) corrections
# PCB internal energy for the transfer of water to air transfer
DeltaUaw <- (a*MW.PCB-b*nOrtho.Cl+c)*1000 # [J/mol]
# Transform HLC to K
K <- 10^(H0)*101325/(R*T) # [Lwater/Lair]
# Correct K using water temperature
K.air.water <- K*exp(-(DeltaUaw/R)*(1/(T.water + 273.15) - 1/T)) # [Lwater/Lair]
# Final K (corrected by air and water temperature)
K.final <- K.air.water*(T.water + 273.15)/(T.air + 273.15) # [Lwater/Lair]

# KDOC calculation and correction
# PCB internal energy for the transfer of octanol-air
DeltaUoa <- (-a2*MW.PCB+b2*nOrtho.Cl-c2)*1000 # [J/mol]
# PCB internal energy for the transfer of octanol-water
DeltaUow <- DeltaUoa + DeltaUaw # [J/mol]
# Octanol-water partition coefficient corrected by water temperature
Kow.water.t <- 10^(Kow)*exp(-(DeltaUow/R)*(1/(T.water + 273.15)-1/T)) # [Loctanol/Lwater]
# DOC-water partition coefficient
Kdoc.t <- 0.06*Kow.water.t # [Lwater/kgdoc]

# Freely dissolved water concentration calculations
C.PCB.water.f <- C.PCB.water/(1 + Kdoc.t*DOC/1000^2) # [ng/m3]

# Air-water mass transfer calculations
# (1) Air side mass transfer calculations
# Water diffusivity in air corrected by air
# temperature and atmospheric pressure
D.water.air <- (10^(-3)*1013.25*((273.15+T.air)^1.75*((1/28.97) +
                (1/18.0152))^(0.5))/P/(20.1^(1/3) + 9.5^(1/3))^2) # [cm2/s]
# PCB diffusivity in air
D.PCB.air <- D.water.air*(MW.PCB/18.0152)^(-0.5) # [cm2/s]
# Water vapor exchange velocity in air (from eq. 20-15)
V.water.air <- 0.2*u + 0.3 # u @10 meter [cm/s]
# Air side mass transfer
V.PCB.air <- V.water.air*(D.PCB.air/D.water.air)^(2/3) # [cm/s]

# (2) Water side mass transfer calculations
# Viscosity of water at water temperature
visc.water <- 10^(-4.5318-220.57/(149.39 - (273.15 + T.water)))
# Water density corrected at water temperature
dens.water <- (999.83952+16.945176*T.water - 7.9870401*10^-3*T.water^2
               - 46.170461*10^-6*3 + 105.56302*10^-9*T.water^4 -
                 280.54253*10^-12*T.water^5)/(1 + 16.87985*10^-3*T.water)
# Kinematic viscosity of water
v.water <- visc.water/dens.water*10000 # [cm2/s]
# CO2 diffusivity in water at water temperature
diff.co2 <- 0.05019*exp(-19.51*1000/(273.15 + T.water)/R) # [cm2/s]
# PCB diffusivity in water 
D.PCB.water <- diff.co2*(MW.PCB/44.0094)^(-0.5) # [cm2/s]
# PCB Schmidt number in water
Sc.PCB.water <- v.water/D.PCB.water
# CO2 Schmidt number in water
Sc.co2.water <- v.water/diff.co2
# k600 calculations, u in [m/s], k600 originally [cm/h]
k600 <- (4.46 + 7.11*u)/60/60 #[cm/s]
# Water side mass transfer (from eq. 20-24)
if(u > 5){
  V.PCB.water <- k600*(Sc.PCB.water/Sc.co2.water)^(-0.5)  
} else {
  V.PCB.water <- k600*(Sc.PCB.water/Sc.co2.water)^(-2/3)
} # [cm/s]

# Air-water mass transfer
mtc.PCB <- ((1/V.PCB.water+1/(V.PCB.air*K.final)))^(-1) # [cm/s]
# Flux calculations
F.PCB.aw <- c(F.PCB.aw, mtc.PCB*(C.PCB.water.f)*(60*60*24)/100) # [ng/m2/d]

}

F.PCB.aw

}

# Final calculations ------------------------------------------------------

Num.Congener <- length(Congener)

result <- NULL
for (i in 1:Num.Congener) {
	result <- rbind(result, final.result(MW.PCB[i], H0.mean[i], H0.error[i], 
         C.PCB.water.mean[i], C.PCB.water.error[i], nOrtho.Cl[i],
         Kow.mean[i], Kow.error[i]))
}

# Sum of all congeners per repetition
final.result <- data.frame(colSums(result, na.rm = TRUE))

# Summary of the total PCBs
mmm <- mean(final.result$colSums.result.)
sss <- sd(final.result$colSums.result.)
q2.5 <- quantile(final.result$colSums.result., 0.025)
q97.5 <- quantile(final.result$colSums.result., 0.975)
tPCBFlux <- c(mmm, sss, q2.5, q97.5)
names(tPCBFlux) <- c("Mean (ng/m2/d)", "Std (ng/m2/d)",
                     "2.5%CL (ng/m2/d)", "97.5%CL (ng/m2/d)")

# Plots -------------------------------------------------------------------

# Histogram
hist(as.numeric(final.result[,1]), main = "Volatilization Flux Total PCBs",
     xlab = "Volatilization Flux Total PCB (ng/m2/d)", border = "blue", col = "green",
     xlim = c(min(as.numeric(final.result[,1])), max(as.numeric(final.result[,1]))))
abline(v = median(as.numeric(final.result[,1])), col = "blue", lwd = 3)
abline(v = quantile(as.numeric(final.result[,1]), 0.025), col = "red", lwd = 3)
abline(v = quantile(as.numeric(final.result[,1]), 0.975), col = "red", lwd = 3)
abline(v = 0, col = "black", lwd = 3)
