
# Install packages
install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")

# Load libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Read and format data ----------------------------------------------------
ph <- read.csv("Data/ph.csv")

# Change data format to long format
ph_long <- ph %>%
  pivot_longer(
    cols = starts_with("pcb"),
    names_to = "congener",
    values_to = "value"
  )

# logarithmic regression model
# Model is tpcb = a*ln(dis)+b
# tPCB --------------------------------------------------------------------
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

# Add legend labels explicitly
ph$legend_observed <- factor("Observed",
                             levels = c("Observed", "AERMOD Prediction", "log-distance model"))
ph$legend_aermod   <- factor("AERMOD Prediction",
                             levels = c("Observed", "AERMOD Prediction", "log-distance model"))
smoothed_data$legend_model <- factor("log-distance model",
                                     levels = c("Observed", "AERMOD Prediction", "log-distance model"))

# Plot
plotTpcb <- ggplot() +
  geom_point(data = ph, aes(x = dis, y = tpcb, color = legend_observed), size = 2) +
  geom_point(data = ph, aes(x = dis, y = model.tpcb2022, color = legend_aermod),
             shape = 20, size = 3) +
  geom_line(data = smoothed_data, aes(x = dis, y = tpcb_fit, color = legend_model),
            linewidth = 1) +
  scale_color_manual(
    values = c("Observed" = "black",
               "AERMOD Prediction" = "red",
               "log-distance model" = "blue")
  ) +
  xlab(expression(bold("Distance to water (m)"))) +
  ylab(expression(bold(Sigma*"PCB (pg/m"^3*")"))) +
  annotate("text", x = Inf, y = Inf, label = r2_label, hjust = 1.1, vjust = 2, size = 4) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(face = "bold", size = 10),
    axis.title = element_text(face = "bold", size = 12),
    aspect.ratio = 1)

plotTpcb

# You need to change this: "Output/Figures/Plots/". This is for my code.
ggsave("Output/Figures/Plots/plotTPCB.png", plot = plotTpcb, width = 5,
       height = 5, dpi = 500)

# PCB4 --------------------------------------------------------------------
fit.pcb <- lm(pcb4 ~ log(dis), data = ph) # need to change tpcb
summary.fit.pcb <- summary(fit.pcb)
summary.fit.pcb$coefficients

r_squared <- summary.fit.pcb$r.squared
r2_label <- paste0("R² = ", signif(r_squared, 2))
p_value_a <- summary.fit.pcb$coefficients["log(dis)", "Pr(>|t|)"]
p_label <- paste0("p-value = ", signif(p_value_a, 2))

# New distance values to make the prediction looks smoother in plot
dis_smooth <- seq(min(ph$dis), max(ph$dis), length.out = 500)

# Prediction
pcb_fit_smooth <- predict(fit.pcb, newdata = data.frame(dis = dis_smooth))

# Create a new data for smoothed predictions
smoothed_data <- data.frame(dis = dis_smooth, pcb_fit = pcb_fit_smooth)

# Add legend labels explicitly
ph$legend_observed <- factor("Observed",
                             levels = c("Observed", "AERMOD Prediction", "log-distance model"))
ph$legend_aermod   <- factor("AERMOD Prediction",
                             levels = c("Observed", "AERMOD Prediction", "log-distance model"))
smoothed_data$legend_model <- factor("log-distance model",
                                     levels = c("Observed", "AERMOD Prediction", "log-distance model"))

# Plot
plotpcb4 <- ggplot() +
  geom_point(data = ph, aes(x = dis, y = pcb4, color = legend_observed), size = 2) +
  geom_point(data = ph, aes(x = dis, y = model.pcb4, color = legend_aermod),
             shape = 20, size = 3) +
  geom_line(data = smoothed_data, aes(x = dis, y = pcb_fit, color = legend_model),
            linewidth = 1) +
  scale_color_manual(
    values = c("Observed" = "black",
               "AERMOD Prediction" = "red",
               "log-distance model" = "blue")
  ) +
  xlab(expression(bold("Distance to water (m)"))) +
  ylab(expression(bold("PCB 4 (pg/m"^3*")"))) +
  annotate("text", x = Inf, y = Inf, label = r2_label, hjust = 1.1, vjust = 2, size = 4) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(face = "bold", size = 10),
    axis.title = element_text(face = "bold", size = 12),
    aspect.ratio = 1)

plotpcb4

ggsave("Output/Figures/Plots/plotPCB4.png", plot = plotpcb4, width = 5,
       height = 5, dpi = 500)

# PCB11 -------------------------------------------------------------------
fit.pcb <- lm(pcb11 ~ log(dis), data = ph) # need to change tpcb
summary.fit.pcb <- summary(fit.pcb)
summary.fit.pcb$coefficients

r_squared <- summary.fit.pcb$r.squared
r2_label <- paste0("R² = ", signif(r_squared, 2))
p_value_a <- summary.fit.pcb$coefficients["log(dis)", "Pr(>|t|)"]
p_label <- paste0("p-value = ", signif(p_value_a, 2))

# New distance values to make the prediction looks smoother in plot
dis_smooth <- seq(min(ph$dis), max(ph$dis), length.out = 500)

# Prediction
pcb_fit_smooth <- predict(fit.pcb, newdata = data.frame(dis = dis_smooth))

# Create a new data for smoothed predictions
smoothed_data <- data.frame(dis = dis_smooth, pcb_fit = pcb_fit_smooth)

# Add legend labels explicitly
ph$legend_observed <- factor("Observed",
                             levels = c("Observed", "AERMOD Prediction", "log-distance model"))
ph$legend_aermod   <- factor("AERMOD Prediction",
                             levels = c("Observed", "AERMOD Prediction", "log-distance model"))
smoothed_data$legend_model <- factor("log-distance model",
                                     levels = c("Observed", "AERMOD Prediction", "log-distance model"))

# Plot
plotpcb11 <- ggplot() +
  geom_point(data = ph, aes(x = dis, y = pcb11, color = legend_observed), size = 2) +
  geom_point(data = ph, aes(x = dis, y = model.pcb11, color = legend_aermod),
             shape = 20, size = 3) +
  geom_line(data = smoothed_data, aes(x = dis, y = pcb_fit, color = legend_model),
            linewidth = 1) +
  scale_color_manual(
    values = c("Observed" = "black",
               "AERMOD Prediction" = "red",
               "log-distance model" = "blue")
  ) +
  xlab(expression(bold("Distance to water (m)"))) +
  ylab(expression(bold("PCB 11 (pg/m"^3*")"))) +
  annotate("text", x = Inf, y = Inf, label = r2_label, hjust = 1.1, vjust = 2, size = 4) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(face = "bold", size = 10),
    axis.title = element_text(face = "bold", size = 12),
    aspect.ratio = 1)

plotpcb11

ggsave("Output/Figures/Plots/plotPCB11.png", plot = plotpcb11, width = 5,
       height = 5, dpi = 500)

# PCB45+51 ----------------------------------------------------------------
fit.pcb <- lm(pcb45.51 ~ log(dis), data = ph) # need to change tpcb
summary.fit.pcb <- summary(fit.pcb)
summary.fit.pcb$coefficients

r_squared <- summary.fit.pcb$r.squared
r2_label <- paste0("R² = ", signif(r_squared, 2))
p_value_a <- summary.fit.pcb$coefficients["log(dis)", "Pr(>|t|)"]
p_label <- paste0("p-value = ", signif(p_value_a, 2))

# New distance values to make the prediction looks smoother in plot
dis_smooth <- seq(min(ph$dis), max(ph$dis), length.out = 500)

# Prediction
pcb_fit_smooth <- predict(fit.pcb, newdata = data.frame(dis = dis_smooth))

# Create a new data for smoothed predictions
smoothed_data <- data.frame(dis = dis_smooth, pcb_fit = pcb_fit_smooth)

# Add legend labels explicitly
ph$legend_observed <- factor("Observed",
                             levels = c("Observed", "AERMOD Prediction", "log-distance model"))
ph$legend_aermod   <- factor("AERMOD Prediction",
                             levels = c("Observed", "AERMOD Prediction", "log-distance model"))
smoothed_data$legend_model <- factor("log-distance model",
                                     levels = c("Observed", "AERMOD Prediction", "log-distance model"))

# Plot
plotpcb <- ggplot() +
  geom_point(data = ph, aes(x = dis, y = pcb45.51, color = legend_observed), size = 2) +
  geom_point(data = ph, aes(x = dis, y = model.pcb45.51, color = legend_aermod),
             shape = 20, size = 3) +
  geom_line(data = smoothed_data, aes(x = dis, y = pcb_fit, color = legend_model),
            linewidth = 1) +
  scale_color_manual(
    values = c("Observed" = "black",
               "AERMOD Prediction" = "red",
               "log-distance model" = "blue")
  ) +
  xlab(expression(bold("Distance to water (m)"))) +
  ylab(expression(bold("PCB 45+51 (pg/m"^3*")"))) +
  annotate("text", x = Inf, y = Inf, label = r2_label, hjust = 1.1, vjust = 2, size = 4) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(face = "bold", size = 10),
    axis.title = element_text(face = "bold", size = 12),
    aspect.ratio = 1)

plotpcb

ggsave("Output/Figures/Plots/plotPCB45+51.png", plot = plotpcb, width = 5,
       height = 5, dpi = 500)

# PCB44+47+65 -------------------------------------------------------------
fit.pcb <- lm(pcb44.47.65 ~ log(dis), data = ph) # need to change tpcb
summary.fit.pcb <- summary(fit.pcb)
summary.fit.pcb$coefficients

r_squared <- summary.fit.pcb$r.squared
r2_label <- paste0("R² = ", signif(r_squared, 2))
p_value_a <- summary.fit.pcb$coefficients["log(dis)", "Pr(>|t|)"]
p_label <- paste0("p-value = ", signif(p_value_a, 2))

# New distance values to make the prediction looks smoother in plot
dis_smooth <- seq(min(ph$dis), max(ph$dis), length.out = 500)

# Prediction
pcb_fit_smooth <- predict(fit.pcb, newdata = data.frame(dis = dis_smooth))

# Create a new data for smoothed predictions
smoothed_data <- data.frame(dis = dis_smooth, pcb_fit = pcb_fit_smooth)

# Add legend labels explicitly
ph$legend_observed <- factor("Observed",
                             levels = c("Observed", "AERMOD Prediction", "log-distance model"))
ph$legend_aermod   <- factor("AERMOD Prediction",
                             levels = c("Observed", "AERMOD Prediction", "log-distance model"))
smoothed_data$legend_model <- factor("log-distance model",
                                     levels = c("Observed", "AERMOD Prediction", "log-distance model"))

# Plot
plotpcb <- ggplot() +
  geom_point(data = ph, aes(x = dis, y = pcb44.47.65, color = legend_observed), size = 2) +
  geom_point(data = ph, aes(x = dis, y = model.pcb44.47.65, color = legend_aermod),
             shape = 20, size = 3) +
  geom_line(data = smoothed_data, aes(x = dis, y = pcb_fit, color = legend_model),
            linewidth = 1) +
  scale_color_manual(
    values = c("Observed" = "black",
               "AERMOD Prediction" = "red",
               "log-distance model" = "blue")
  ) +
  xlab(expression(bold("Distance to water (m)"))) +
  ylab(expression(bold("PCB 44+47+65 (pg/m"^3*")"))) +
  annotate("text", x = Inf, y = Inf, label = r2_label, hjust = 1.1, vjust = 2, size = 4) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(face = "bold", size = 10),
    axis.title = element_text(face = "bold", size = 12),
    aspect.ratio = 1)

plotpcb

ggsave("Output/Figures/Plots/plotPCB44+47+65.png", plot = plotpcb, width = 5,
       height = 5, dpi = 500)

# PCB68 -------------------------------------------------------------------
fit.pcb <- lm(pcb68 ~ log(dis), data = ph) # need to change tpcb
summary.fit.pcb <- summary(fit.pcb)
summary.fit.pcb$coefficients

r_squared <- summary.fit.pcb$r.squared
r2_label <- paste0("R² = ", signif(r_squared, 2))
p_value_a <- summary.fit.pcb$coefficients["log(dis)", "Pr(>|t|)"]
p_label <- paste0("p-value = ", signif(p_value_a, 2))

# New distance values to make the prediction looks smoother in plot
dis_smooth <- seq(min(ph$dis), max(ph$dis), length.out = 500)

# Prediction
pcb_fit_smooth <- predict(fit.pcb, newdata = data.frame(dis = dis_smooth))

# Create a new data for smoothed predictions
smoothed_data <- data.frame(dis = dis_smooth, pcb_fit = pcb_fit_smooth)

# Add legend labels explicitly
ph$legend_observed <- factor("Observed",
                             levels = c("Observed", "AERMOD Prediction", "log-distance model"))
ph$legend_aermod   <- factor("AERMOD Prediction",
                             levels = c("Observed", "AERMOD Prediction", "log-distance model"))
smoothed_data$legend_model <- factor("log-distance model",
                                     levels = c("Observed", "AERMOD Prediction", "log-distance model"))

# Plot
plotpcb <- ggplot() +
  geom_point(data = ph, aes(x = dis, y = pcb68, color = legend_observed), size = 2) +
  geom_point(data = ph, aes(x = dis, y = model.pcb68, color = legend_aermod),
             shape = 20, size = 3) +
  geom_line(data = smoothed_data, aes(x = dis, y = pcb_fit, color = legend_model),
            linewidth = 1) +
  scale_color_manual(
    values = c("Observed" = "black",
               "AERMOD Prediction" = "red",
               "log-distance model" = "blue")
  ) +
  xlab(expression(bold("Distance to water (m)"))) +
  ylab(expression(bold("PCB 68 (pg/m"^3*")"))) +
  annotate("text", x = Inf, y = Inf, label = r2_label, hjust = 1.1, vjust = 2, size = 4) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(face = "bold", size = 10),
    axis.title = element_text(face = "bold", size = 12),
    aspect.ratio = 1)

plotpcb

ggsave("Output/Figures/Plots/plotPCB68.png", plot = plotpcb, width = 5,
       height = 5, dpi = 500)
