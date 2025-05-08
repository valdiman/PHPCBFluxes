
# Install packages
install.packages("ggplot2")

# Load libraries
library(ggplot2)

# Read data
ph <- read.csv("Data/DistPCB.csv")

# logarithmic regression model
# Model is tpcb = a*ln(dis)+b
fit.tpcb <- lm(tpcb ~ log(dis), data = ph) # need to change tpcb
summary.fit.tpcb <- summary(fit.tpcb)
summary.fit.tpcb$coefficients

r_squared <- summary.fit.tpcb$r.squared
r2_label <- paste0("R² = ", round(r_squared, 2))
p_value_a <- summary.fit.tpcb$coefficients["log(dis)", "Pr(>|t|)"]
p_label <- paste0("p-value = ", signif(p_value_a, 2))

# New distance values to make the prediction looks smoother in plot
dis_smooth <- seq(min(ph$dis), max(ph$dis), length.out = 500)

# Prediction
tpcb_fit_smooth <- predict(fit.tpcb, newdata = data.frame(dis = dis_smooth))

# Create a new data for smoothed predictions
smoothed_data <- data.frame(dis = dis_smooth, tpcb_fit = tpcb_fit_smooth)

# Plot
plotTpcb <- ggplot(ph, aes(x = dis, y = tpcb)) + # need to change tpcb
  geom_point(size = 3, shape=21) + # Plot the actual data points
  geom_line(data = smoothed_data,
            aes(x = dis, y = tpcb_fit), color = "blue", size = 1) + # Smoother predicted line
  geom_text(aes(label = id), vjust = 2, size = 3) +
  theme_bw() +
  xlim(0,2501)+
  annotate("text", x = Inf, y = Inf,
           label = paste0("R² = ", round(r_squared, 2)),
           hjust = 1.1, vjust = 3, size = 5) +
  xlab(expression(bold("Distance to water (m)"))) +
  ylab(expression(bold(tpcb~(pg/m^3)))) + # need to change the name
  theme(aspect.ratio = 1) +
  theme(axis.text.y = element_text(face = "bold", size = 14),
        axis.title.y = element_text(face = "bold", size = 14)) +
  theme(axis.text.x = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold", size = 14))

plotTpcb

ggsave("Rcode/plots/plotTPCB.png", plot = plotTpcb, width = 5,
       height = 5, dpi = 500)
