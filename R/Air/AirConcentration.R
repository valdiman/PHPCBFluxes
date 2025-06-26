# Packages and libraries needed --------------------------------------------
# Install packages
install.packages("ggplot2")

# Libraries
library(ggplot2)

# Read data ---------------------------------------------------------------
# Read water concentrations
air <- read.csv("Data/AirConc.csv")

tpcb <- as.data.frame(rowSums(air[, 2:174]))
# Change units from ng to pg
tpcb <- tpcb * 1000
# Add sites
tpcb <- data.frame(air$sid, tpcb)
# Change column names
colnames(tpcb) <- c("sid", "tPCB")

# Boxplot
box.plot <- ggplot(data = tpcb, aes(x = "", y = tPCB)) +
  geom_point(shape = 21, size = 3.5, stroke = 1.3) +
  geom_boxplot(alpha = 0, width = 0.7, lwd = 0.9, outlier.shape = NA) +
  labs(x = " ") +
  labs(y = expression(bold("Air Concentration " * Sigma * "PCB (pg/m"^3*")"))) +
  theme_bw() +
  theme(
    aspect.ratio = 6/1,
    axis.text.y = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.ticks.length = unit(0.3, "cm"),              # Increase tick length
    axis.ticks = element_line(linewidth = 1.2)
  )

box.plot

# Save plot in folder
ggsave("Output/Figures/Plots/TOC.jpg", plot = box.plot,
       width = 3, height = 9, dpi = 500)
