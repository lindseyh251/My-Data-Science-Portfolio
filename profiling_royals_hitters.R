# profiling_royals_hitters.R
# by Lindsey Hornberger 
# Profiling Marlins Hitters: Power vs Patience vs Speed (2024 season)

# import libraries
library(baseballr)
library(dplyr)
library(factoextra)
library(plotly)

# get data 
batters_royals <- bref_daily_batter("2024-03-01", "2024-11-30") %>% filter(Team=="Kansas City")
pitchers_marlins <- bref_daily_batter("2024-03-01", "2024-11-30") %>% filter(Team=="Kansas City")

# select necessary variables and create rate-based stats
hitting_classifier_data <- batters_royals %>% 
  select(Name, Age, BB, SO, HR, SB, CS, X2B, X3B, PA, AB, HBP, GDP) %>% 
  filter(AB>25) %>% # only include hitters with >25 ABs
  mutate(BB_rate = BB / PA,
         SO_rate = SO / PA,
         HR_rate = HR / PA,
         XBH = X2B + X3B + HR,
         XBH_rate = XBH / PA,
         SB_success = SB / (SB + CS))

# Select variables for clustering
cluster_vars <- hitting_classifier_data %>%
  select(BB_rate, SO_rate, HR_rate, XBH_rate, SB_success) %>%
  scale()  # Standardize

# fix NaN values in SB_success rate to zero
cluster_vars[!is.finite(cluster_vars)] <- 0

# K-means clustering
set.seed(123)
km <- kmeans(cluster_vars, centers = 3)

# Add cluster back to original data
hitting_classifier_data $cluster <- as.factor(km$cluster)

# View results
print(hitting_classifier_data)

# Visualize clusters using PCA
fviz_cluster(km, data = cluster_vars, labelsize = 8, main = "Hitter Types")+
  geom_text(aes(label = hitting_classifier_data$Name), size = 3, vjust = 1.5, color = "black") +
  labs(
    title = "Royals Hitters Clustered by Offensive Style",
    subtitle = "K-means clustering on BB%, SO%, HR%, XBH%, and SB success",
    caption = "Data source: Statcast",
    x = "PC 1",
    y = "PC 2"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )

# analyze pca result and scree plot 
pca_result <- prcomp(cluster_vars, center = TRUE, scale. = TRUE)
pca_result$rotation # the largest abs values are the key contributors 
summary(pca_result)

# 3D visualization with 3 PCs
pca_df <- as.data.frame(pca_result$x)
pca_df$cluster <- as.factor(km$cluster)
pca_df$Name <- hitting_classifier_data$Name

plot_ly(pca_df, x = ~PC1, y = ~PC2, z = ~PC3, color = ~cluster, text = ~Name,
        type = 'scatter3d', mode = 'markers')

# variable constributions to PCs
fviz_pca_var(pca_result, col.var = "contrib") +
  labs(title = "Variable Contributions to Principal Components")

'''
SUMMARY

Cluster 1 (Red) – High-walk / Low-AB players
Nick Loftin, Yuli Gurriel. Small-sample or bench hitters with higher BB%, 
low power and low SB activity.

Cluster 2 (Green) – Balanced / Contact–Power mix
Bobby Witt, Vinnie Pasquantino, Michael Massey, Salvador Perez, Maikel Garcia, 
etc. Moderate BB%, decent power (HR%, XBH%), some speed — overall more “regular 
starter” profiles.

Cluster 3 (Blue) – Swing-heavy / Power-Speed mix
MJ Melendez, Hunter Renfroe, Kyle Isbel, Dairon Blanco, Nelson Velázquez, 
Garrett Hampson. Higher strikeout rates, more extra-base hits or SB attempts, 
lower walk rates — aggressive hitters with either speed or power emphasis.

'''