# Read and retrieve data from USGS

# Install package
install.packages("dataRetrieval")

# Load libraries
library(dataRetrieval) # read data from USGS

# Read data from USGS station
USGSPH <- "14211720" # Station @ WILLAMETTE RIVER AT PORTLAND, OR
# Water temperature in C
Watertemp082018 <- readNWISdv(USGSPH, "00010",
                          "2018-08-20", "2018-09-01")
Watertemp112018 <- readNWISdv(USGSPH, "00010",
                              "2018-11-26", "2018-12-02")
Watertemp012019 <- readNWISdv(USGSPH, "00010",
                              "2019-01-25", "2019-02-20")
Watertemp22022 <- readNWISdv(USGSPH, "00010",
                     "2022-06-01", "2022-09-01")

# Dissolved organic matter fluorescence (fDOM),
# water, in situ, concentration estimated from reference material,
# micrograms per liter as quinine sulfate equivalents (QSE), ug/l QSE
fDOM082018 <- readNWISdv(USGSPH, "32295",
                              "2018-08-20", "2018-09-01")
fDOM112018 <- readNWISdv(USGSPH, "32295",
                              "2018-11-26", "2018-12-02")
fDOM012019 <- readNWISdv(USGSPH, "32295",
                              "2019-01-25", "2019-02-20")
fDOM2022 <- readNWISdv(USGSPH, "32295",
                   "2022-06-01", "2022-09-01")
# Specific conductance, water, unfiltered, microsiemens per centimeter
# at 25 degrees Celsius
WaterConduct082018 <- readNWISdv(USGSPH, "00095",
                         "2018-08-20", "2018-09-01")
WaterConduct112018 <- readNWISdv(USGSPH, "00095",
                         "2018-11-26", "2018-12-02")
WaterConduct012019 <- readNWISdv(USGSPH, "00095",
                         "2019-01-25", "2019-02-20")
WaterConduct2022 <- readNWISdv(USGSPH, "00095",
                   "2022-06-01", "2022-09-01")

# Mean water velocity for discharge computation, feet per second,
# Morrison Bridge
# Not working! (11/16/2022)
WaterVeloc082018 <- readNWISdv(USGSPH, "72255",
                                 "2018-08-20", "2018-09-01")
WaterVeloc112018 <- readNWISdv(USGSPH, "72255",
                                 "2018-11-26", "2018-12-02")
WaterVeloc012019 <- readNWISdv(USGSPH, "72255",
                                 "2019-01-25", "2019-02-20")
WaterVeloc2022 <- readNWISdv(USGSPH, "72255",
                           "2022-06-01", "2022-09-01")


