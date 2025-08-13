# Load necessary libraries
library(ggplot2)
library(ggridges)
library(dplyr)
library(patchwork)

# Define the data
il6.breast.implant.patients <- c(231, 308287, 33291, 124550, 17075, 
                                 22955, 95102, 5649, 840585, 58924)

il6.control.patients <- c(35324, 12457, 8276, 44, 278, 840)

# Create a data frame
df <- data.frame(
  group = rep(c("Implant", "Control"), c(length(il6.breast.implant.patients), 
                                         length(il6.control.patients))),
  value = c(il6.breast.implant.patients, il6.control.patients)
)

# Boxplot
boxplot_plot <- ggplot(df, aes(x = group, y = value, fill = group)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, alpha = 0.7) +
  scale_y_log10() +  # Apply log scale to handle wide data range
  labs(title = "Boxplot of IL-6 Levels", y = "IL-6 (log scale)", x = "Group") +
  theme_minimal()

# Violin Plot
violin_plot <- ggplot(df, aes(x = group, y = value, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_jitter(shape = 16, position = position_jitter(0.2), color = "black") +
  scale_y_log10() + 
  labs(title = "Violin Plot of IL-6 Levels", y = "IL-6 (log scale)", x = "Group") +
  theme_minimal()

# Ridgeline Plot
ridge_plot <- ggplot(df, aes(x = value, y = group, fill = group)) +
  geom_density_ridges(alpha = 0.7, scale = 1.2) +
  scale_x_log10() + 
  labs(title = "Ridgeline Plot of IL-6 Levels", x = "IL-6 (log scale)", y = "Group") +
  theme_minimal()

# Arrange plots in a grid
boxplot_plot + violin_plot + ridge_plot + plot_layout(ncol = 3)
