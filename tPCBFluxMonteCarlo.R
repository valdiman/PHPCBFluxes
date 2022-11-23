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

# Add MW of individual PCB congeners
pc[1:3,2] <- c(188.644)
pc[4:13,2] <- c(223.088)
pc[14:33,2] <- c(257.532)
pc[34:62,2] <- c(291.976)
pc[63:93,2] <- c(326.42)
pc[94:124,2] <- c(360.864)
pc[125:146,2] <- c(395.308)
pc[147:156,2] <- c(429.752)
pc[157:159,2] <- c(465.740544)
pc[160,2] <- c(498.64)

# Add ortho Cl of individual PCB congeners
pc[,3] <- c(1, 0, 0, 2, 1, 1, 1, 1, 1, 2, 0, 0, 0, 2,
            2, 2, 3, 1, 1, 1, 1, 2, 1, 1, 2, 1, 2, 1,
            0, 0, 0, 0, 0, 2, 2, 2, 2, 3, 3, 2, 2, 3,
            2, 4, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1,
            1, 0, 0, 0, 0, 0, 2, 2, 3, 2, 2, 3, 3, 2,
            2, 3, 3, 3, 4, 3, 3, 4, 1, 2, 1, 1, 2, 1,
            2, 1, 1, 1, 2, 1, 1, 0, 0, 2, 2, 3, 3, 2,
            3, 3, 4, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 4,
            2, 3, 4, 1, 2, 1, 2, 2, 1, 2, 1, 0, 2, 3,
            2, 3, 3, 4, 3, 3, 4, 2, 3, 3, 3, 4, 3, 4,
            3, 4, 1, 2, 2, 2, 1, 3, 3, 4, 3, 4, 4, 4,
            3, 2, 3, 4, 4, 4)

# Add log10 Ho of individual PCB congeners
pc[,4] <- c(-3.526,	-3.544,	-3.562,	-3.483,	-3.622,	-3.486,	-3.424,
            -3.518,	-3.49,	-3.373,	-3.537,	-3.595,	-3.649,	-3.600,
            -3.428,	-3.495,	-3.355,	-3.544,	-3.62,	-3.719,	-3.497,
            -3.500,	-3.500,	-3.526,	-3.393,	-3.562,	-3.407,	-3.375,
            -3.3745,	-3.473,	-3.818,	-3.634,	-3.524,	-3.503,	-3.592,
            -3.475,	-3.638,	-3.450,	-3.470,	-3.519,	-3.452,	-3.366,
            -3.496,	-3.242,	-3.739,	-3.82,	-3.568,	-3.602,	-3.517,
            -3.816,	-3.694,	-3.615,	-3.565,	-3.693,	-3.631,	-3.424,
            -3.441,	-3.989,	-3.787,	-3.705,	-3.426,	-3.844,	-3.835,
            -3.603,	-3.600,	-3.716,	-3.745,	-3.461,	-3.526,	-3.610,
            -3.585,	-3.468,	-3.407,	-3.523,	-3.387,	-3.431,	-3.298,
            -3.13,	-4.003,	-3.783,	-3.755,	-3.768,	-3.707,	-3.574,
            -3.574,	-3.845,	-3.901,	-3.61,	-3.253,	-3.901,	-3.759,
            -4.087,	-3.807,	-3.886,	-3.817,	-3.616,	-3.693,	-3.691,
            -3.639,	-3.548,	-3.492,	-3.754,	-3.483,	-3.76,	-3.502,
            -3.529,	-3.328,	-3.727,	-3.625,	-3.367,	-3.296,	-3.369,
            -3.783,	-3.418,	-3.075,	-4.053,	-3.782,	-3.808,	-3.670,
            -3.545,	-3.881,	-3.56,	-3.959,	-4.186,	-4.059,	-3.763,
            -3.924,	-3.772,	-3.651,	-3.527,	-3.787,	-3.671,	-3.560,
            -3.969,	-3.638,	-3.59,	-3.696,	-3.339,	-3.669,	-3.434,
            -3.693,	-3.353,	-3.177,	-3.95,	-3.876,	-3.718,	-4.174,
            -3.926,	-3.884,	-3.596,	-3.644,	-3.619,	-3.884,	-3.651,
            -3.853,	-4.059,	-4.059,	-3.772,	-3.777,	-3.948)

# Add Ho error
pc[,5] <- c(0.662)

# Add log10 Kow of individual PCB congeners
pc[,6] <- c(4.46,	4.69,	4.69,	4.65,	4.97,	5.06,	5.07,	5.07,	5.06,	4.84,
            5.28,	5.29,	5.3,	5.16,	5.25,	5.24,	5.02,	5.67,	5.60,	5.58,
            5.57,	5.35,	5.67,	5.66,	5.44,	5.67,	5.44,	5.66,	5.82,	5.88,
            5.83,	5.76,	5.89,	5.98,	5.76,	5.75,	5.75,	5.53,	5.53,	5.78,
            5.85,	5.62,	5.84,	5.21,	6.11,	6.11,	6.17,	6.17,	5.95,	6.11,
            6.20,	6.17,	5.95,	6.20,	6.20,	6.26,	6.26,	6.36,	6.35,	6.42,
            6.48,	6.36,	6.2,	6.39,	6.04,	6.3,	6.29,	6.13,	6.07,	6.38,
            6.35,	6.04,	6.13,	6.13,	5.71,	6.16,	6.22,	5.81,	6.65,	6.64,
            6.71,	6.73,	6.48,	6.76,	6.45,	6.65,	6.74,	6.79,	6.64,	6.64,
            6.74,	6.89,	6.95,	6.83,	6.8,	6.58,	6.58,	6.86,	6.55,	6.64,
            6.22,	7.02,	6.67,	6.82,	6.51,	6.67,	6.25,	6.89,	6.67,	6.73,
            6.32,	6.22,	6.92,	6.76,	6.41,	7.18,	7.02,	7.24,	6.93,	7.08,
            7.24,	7.05,	7.27,	7.42,	7.27,	7.11,	7.33,	7.11,	7.17,	6.76,
            7.08,	7.14,	6.73,	7.36,	7.11,	7.20,	7.20,	6.85,	7.11,	6.69,
            7.17,	6.82,	7.71,	7.46,	7.55,	7.52,	7.80,	7.56,	7.65,	7.30,
            7.20,	7.27,	7.62,	7.24,	7.65,	8.00,	8.09,	7.74,	7.71,	8.18)

# Add Kow error
pc[,7] <- c(0.32)

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
