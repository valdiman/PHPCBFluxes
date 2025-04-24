
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
ggplot(ph, aes(x = dis, y = tpcb)) + # need to change tpcb
  geom_point(size = 3) +  # Plot the actual data points
  geom_line(data = smoothed_data, aes(x = dis, y = tpcb_fit), color = "blue", size = 1) +  # Smoother predicted line
  theme_bw() +
  annotate("text", x = Inf, y = 950,
           label = paste0("R² = ", round(r_squared, 2)),
           hjust = 1.1, vjust = 2.5, size = 5) +
  annotate("text", x = Inf, y = 900, label = p_label,
           hjust = 1.1, vjust = 1.5, size = 5) +
  xlab(expression(bold("Distance to water (m)"))) +
  ylab(expression(bold(Sigma~PCB~(pg/m^3)))) + # need to change the name
  theme(axis.text.y = element_text(face = "bold", size = 14),
        axis.title.y = element_text(face = "bold", size = 14)) +
  theme(axis.text.x = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold", size = 14))
