library(readr)
library(dplyr)
library(ggplot2)

# Read the cleaned dataset
df <- read_tsv("Table8_amino_acid_profile_no_uncertainity.tsv", show_col_types = FALSE)

# Remove NA values and duplicates
df <- df %>% drop_na() %>% unique() %>% as.data.frame()

# Extract numeric columns for PCA (adjust indices as needed)
pca_data <- df[, 4:21]

# Perform PCA with scaling
pca <- prcomp(pca_data, scale. = TRUE)

# Create a data frame with PCA results
pca_df <- as.data.frame(pca$x)

# Derive a grouping variable from the food_code (e.g., using the first letter)
pca_df$food_category <- substr(df$food_code, 1, 1)

# Plot PC1 vs PC2 colored by food category
ggplot(pca_df, aes(x = PC1, y = PC2, color = food_category)) +
  geom_point(size = 2) +
  labs(title = "PCA of Amino Acid Profiles", x = "PC1", y = "PC2") +
  theme_minimal()

# Get PCA loadings (rotation matrix)
loadings <- pca$rotation

# Identify top 2 features for PC1 based on absolute loadings
top_PC1_indices <- order(abs(loadings[, "PC1"]), decreasing = TRUE)[1:2]
top_PC1_features <- colnames(pca_data)[top_PC1_indices]

# Identify top 2 features for PC2 based on absolute loadings
top_PC2_indices <- order(abs(loadings[, "PC2"]), decreasing = TRUE)[1:2]
top_PC2_features <- colnames(pca_data)[top_PC2_indices]

top_PC1_features
top_PC2_features




# Define two food categories (adjust as needed based on data)
group1_code <- substr(df$food_code, 1, 1) == "A"  
group2_code <- substr(df$food_code, 1, 1) == "B"  

# Extract values of top PC1 feature for the two groups
group1 <- df[group1_code, "Methionine"]
group2 <- df[group2_code, "Methionine"]

# Perform a t-test
t_test_result <- t.test(group1, group2, var.equal = TRUE)

# Print results
print(t_test_result)