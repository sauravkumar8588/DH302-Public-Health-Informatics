# Load necessary libraries
library(ggplot2)
library(patchwork)

# Define the data
il6.breast.implant.patients <- c(231, 308287, 33291, 124550, 17075, 
                                 22955, 95102, 5649, 840585, 58924)

il6.control.patients <- c(35324, 12457, 8276, 44, 278, 840)

# Create data frames
df.breast <- data.frame(value = il6.breast.implant.patients)
df.control <- data.frame(value = il6.control.patients)

# Generate Q-Q plots
implant.qqplot <- ggplot(df.breast, aes(sample = value)) +
  geom_qq() +
  geom_qq_line() +
  ggtitle("Q-Q Plot: Breast Implant Patients") +
  theme_minimal()

control.qqplot <- ggplot(df.control, aes(sample = value)) +
  geom_qq() +
  geom_qq_line() +
  ggtitle("Q-Q Plot: Control Patients") +
  theme_minimal()

# Arrange both plots side by side
implant.qqplot | control.qqplot
