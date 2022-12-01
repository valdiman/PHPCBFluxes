# Evaluate Portland water PCB data

# Packages and libraries needed -------------------------------------------------------------------
# Install packages
install.packages("ggplot2")
install.packages("ggmap")
install.packages("ggrepel")

# Libraries
library(ggplot2) # make_bbox
library(ggmap) # make_bbox
library(ggrepel) #geom_label_repel

# Read data ---------------------------------------------------------------
# Read water concentrations
wc.raw <- read.csv("WaterConcentration.csv")

# Maps --------------------------------------------------------------------
# Map with ggmap
# Create a square map around samples
PO.box <- make_bbox(lon = wc.raw$Longitude, lat = wc.raw$Latitude,
                    f = 0.8)
PO.map <- get_stamenmap(bbox = PO.box, zoom = 10)
# Plot map with sites
# Prepare data
# Get tPCB and coordinates
tPCB.PO <- data.frame(cbind(wc.raw$LocationID, wc.raw$Latitude,
                            wc.raw$Longitude,
                            rowSums(wc.raw[, c(8:166)],
                                    na.rm = TRUE)))
# Name the columns
colnames(tPCB.PO) <- c("Site", "Latitude", "Longitude", "tPCB")
# Change no numeric to numeric
tPCB.PO$Latitude <- as.numeric(tPCB.PO$Latitude)
tPCB.PO$Longitude <- as.numeric(tPCB.PO$Longitude)
tPCB.PO$tPCB <- as.numeric(tPCB.PO$tPCB)
# Average tPCB per site
tPCB.mean <- aggregate(tPCB ~ Site + Latitude + Longitude,
                       data = tPCB.PO, FUN = mean)
tPCB.sd <- aggregate(tPCB ~ Site + Latitude + Longitude,
                     data = tPCB.PO, FUN = sd)
tPCB.mean$sd <- tPCB.sd$tPCB
# Name the columns
colnames(tPCB.mean) <- c("Site", "Latitude", "Longitude", "tPCB.ave",
                       'tPCB.sd')

# (1) Plot map + locations
ggmap(PO.map) +
  geom_point(data = tPCB.mean, aes(x = Longitude, y = Latitude),
             shape = 21, color = "red",
             fill = "white", size = 1.75, stroke = 0.75) +
  geom_label_repel(aes(x = Longitude, y = Latitude, label = Site),
                   data = tPCB.mean, family = 'Times', size = 4, 
                   box.padding = 0.2, point.padding = 0.3,
                   segment.color = 'grey50')

# (2) Plot map + tPCB
ggmap(PO.map) +
  geom_point(data = tPCB.mean, aes(x = Longitude, y = Latitude,
                                   size = tPCB.ave), alpha = 1,
             color  = "red") +
  scale_size_area(breaks = c(100, 125, 150, 175, 200),
                  name = "Ave. PCBs \n2018-2019 (pg/L)") +
  xlab("Longitude") +
  ylab("Latitude")

# Bar plot of tPCB
ggplot(tPCB.mean, aes(y = tPCB.ave, x = Site)) + 
  geom_bar(stat = 'identity', width = 0.8, fill = "black") +
  geom_errorbar(aes(ymin=0, ymax = tPCB.ave + tPCB.sd), width=.2,
                position=position_dodge(.9)) +
  theme_bw() +
  theme(aspect.ratio = 5/10) +
  ylab(expression(Sigma*"PCB (pg/L)")) +
  xlab(expression("")) +
  theme(axis.text.y = element_text(face = "bold", size = 7),
        axis.title.y = element_text(face = "bold", size = 7)) +
  theme(axis.text.x = element_text(face = "bold", size = 7,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 7))

# Read water concentrations
wc.raw <- read.csv("WaterConcentration.csv")
# Different approaches to use the data
# Prepare data [pg/L] = [ng/m3]
wc.1 <- subset(wc.raw, select = -c(SampleID:Units))
wc.2 <- cbind(wc.raw$LocationID, wc.1)
colnames(wc.2)[1] <- "LocationID"
# (1) All samples
# (1) Mean and standard deviation
wc.ave <- sapply(wc.1, mean, na.rm = TRUE)
wc.sd <- sapply(wc.1, sd, na.rm = TRUE)
wc.3 <- data.frame(t(rbind(wc.ave, wc.sd)))
C.PCB.water.mean <- wc.3$wc.ave
C.PCB.water.sd <- wc.3$wc.sd
# (II) Geometric mean and geometric standard deviation
# Log 10 individual PCBs
wc.log <- log10(wc.1)
wc.gm <- exp(sapply(wc.log, mean, na.rm = TRUE))
wc.gsd <- exp(sapply(wc.log, sd, na.rm = TRUE))
wc.4 <- data.frame(t(rbind(wc.gm, wc.gsd)))
C.PCB.water.gm <- wc.4$wc.gm
C.PCB.water.gsd <- wc.4$wc.gsd
# (2) Especific site
# Selected site
wc.POH001 <- wc.2[wc.2$LocationID == 'WCPCB_OR-POH001', ]
wc.POH002 <- wc.2[wc.2$LocationID == 'WCPCB_OR-POH002', ]
wc.POH003 <- wc.2[wc.2$LocationID == 'WCPCB_OR-POH003', ]
wc.POH004 <- wc.2[wc.2$LocationID == 'WCPCB_OR-POH004', ]
wc.POH005 <- wc.2[wc.2$LocationID == 'WCPCB_OR-POH005', ]
wc.POH006 <- wc.2[wc.2$LocationID == 'WCPCB_OR-POH006', ]
wc.POH007 <- wc.2[wc.2$LocationID == 'WCPCB_OR-POH007', ]
# Calculate mean and sd for each site
# WCPCB_OR-POH001
wc.POH001.ave <- sapply(wc.POH001[, 2:160], mean, na.rm = TRUE)
wc.POH001.sd <- sapply(wc.POH001[, 2:160], sd, na.rm = TRUE)
wc.POH001.2 <- data.frame(t(rbind(wc.POH001.ave, wc.POH001.sd)))
C.PCB.water.POH001.mean <- wc.POH001.2$wc.POH001.ave
C.PCB.water.POH001.sd <- wc.POH001.2$wc.POH001.sd
# WCPCB_OR-POH002
wc.POH002.ave <- sapply(wc.POH002[, 2:160], mean, na.rm = TRUE)
wc.POH002.sd <- sapply(wc.POH002[, 2:160], sd, na.rm = TRUE)
wc.POH002.2 <- data.frame(t(rbind(wc.POH002.ave, wc.POH002.sd)))
C.PCB.water.POH002.mean <- wc.POH002.2$wc.POH002.ave
C.PCB.water.POH002.sd <- wc.POH002.2$wc.POH002.sd
# WCPCB_OR-POH003
wc.POH003.ave <- sapply(wc.POH003[, 2:160], mean, na.rm = TRUE)
wc.POH003.sd <- sapply(wc.POH003[, 2:160], sd, na.rm = TRUE)
wc.POH003.2 <- data.frame(t(rbind(wc.POH003.ave, wc.POH003.sd)))
C.PCB.water.POH003.mean <- wc.POH003.2$wc.POH003.ave
C.PCB.water.POH003.sd <- wc.POH003.2$wc.POH003.sd
# WCPCB_OR-POH004
wc.POH004.ave <- sapply(wc.POH004[, 2:160], mean, na.rm = TRUE)
wc.POH004.sd <- sapply(wc.POH004[, 2:160], sd, na.rm = TRUE)
wc.POH004.2 <- data.frame(t(rbind(wc.POH004.ave, wc.POH004.sd)))
C.PCB.water.POH004.mean <- wc.POH004.2$wc.POH004.ave
C.PCB.water.POH004.sd <- wc.POH004.2$wc.POH004.sd
# WCPCB_OR-POH005
wc.POH005.ave <- sapply(wc.POH005[, 2:160], mean, na.rm = TRUE)
wc.POH005.sd <- sapply(wc.POH005[, 2:160], sd, na.rm = TRUE)
wc.POH005.2 <- data.frame(t(rbind(wc.POH005.ave, wc.POH005.sd)))
C.PCB.water.POH005.mean <- wc.POH005.2$wc.POH005.ave
C.PCB.water.POH005.sd <- wc.POH005.2$wc.POH005.sd
# WCPCB_OR-POH006
wc.POH006.ave <- sapply(wc.POH006[, 2:160], mean, na.rm = TRUE)
wc.POH006.sd <- sapply(wc.POH006[, 2:160], sd, na.rm = TRUE)
wc.POH006.2 <- data.frame(t(rbind(wc.POH006.ave, wc.POH006.sd)))
C.PCB.water.POH006.mean <- wc.POH006.2$wc.POH006.ave
C.PCB.water.POH006.sd <- wc.POH006.2$wc.POH006.sd
# WCPCB_OR-POH007
wc.POH007.ave <- sapply(wc.POH007[, 2:160], mean, na.rm = TRUE)
wc.POH007.sd <- sapply(wc.POH007[, 2:160], sd, na.rm = TRUE)
wc.POH007.2 <- data.frame(t(rbind(wc.POH007.ave, wc.POH007.sd)))
C.PCB.water.POH007.mean <- wc.POH007.2$wc.POH007.ave
C.PCB.water.POH007.sd <- wc.POH007.2$wc.POH007.sd
