# Read and retrieve data from USGS

# Install package
install.packages("dataRetrieval")

# Load libraries
library(dataRetrieval) # read data from USGS

# Read data from USGS station
USGSPH <- "14211720" # Station @ WILLAMETTE RIVER AT PORTLAND, OR

Watertemp <- readNWISdv(USGSPH, "00010",
                     "2022-06-01", "2022-09-01") # water temperature, C

# Dissolved organic matter fluorescence (fDOM),
# water, in situ, concentration estimated from reference material,
# micrograms per liter as quinine sulfate equivalents (QSE), ug/l QSE
fDOM <- readNWISdv(USGSPH, "32295",
                   "2022-06-01", "2022-09-01")
# Specific conductance, water, unfiltered, microsiemens per centimeter
# at 25 degrees Celsius
WaterConduct <- readNWISdv(USGSPH, "00095",
                   "2022-06-01", "2022-09-01")

# Mean water velocity for discharge computation, feet per second,
# Morrison Bridge
# Not working! (11/16)
WaterVeloc <- readNWISdv(USGSPH, "72255",
                           "2022-06-01", "2022-09-01")


