# profiling_marlins_hitters.R
# by Lindsey Hornberger 
# Profiling Marlins Hitters: Power vs Patience vs Speed (2025 season so far)

# import libraries
library(baseballr)
library(dplyr)
library(factoextra)
library(plotly)

# get data 
batters_marlins <- bref_daily_batter("2025-03-01", "2025-10-03") %>% filter(Team=="Miami")
pitchers_marlins <- bref_daily_batter("2025-03-01", "2025-10-03") %>% filter(Team=="Miami")

# Project 1 -- Profiling Marlins Hitters: Power vs Patience vs Speed

# select necessary variables and create rate-based stats
hitting_classifier_data <- batters_marlins %>% 
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
    title = "Marlins Hitters Clustered by Offensive Style",
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
Glossary: 
 Stat	What it Measures	Definition
BB_rate--Plate discipline (Patience at the plate)--The percentage of plate 
    appearances that result in a walk. Higher = more selective and patient 
    hitters.
SO_rate--Contact ability (Lower is better)--The percentage of plate appearances 
    that end in a strikeout. Lower = better contact skills.
HR_rate--Power--The percentage of plate appearances that result in a home run. 
    Higher = more home run power.
XBH_rate--Total extra-base power--The percentage of plate appearances that 
    result in doubles, triples, or home runs. Shows gap power and slugging 
    ability beyond just home runs.
SB_success--Base-running skill--Stolen base success rate: successful steals 
    divided by total steal attempts (SB / (SB + CS)). Measures both speed and 
    smart base running.
    
    
SUMMARY:

Cluster 1 (Red, circles)
  Contains players like Derek Hill, Dane Myers, Eric Wagaman.
  Tend to be moderate on patience (BB%), moderate contact (SO%), and lower power (HR%).
  More likely contact hitters or those with balanced approaches but less power.

Cluster 2 (Green, triangles)
  Includes Kyle Stowers, Matt Mervis, Maximo Acosta.
  These players tend to have higher power and strikeout rates, so more "power hitters" but possibly with patience varying.
  Probably more aggressive or long-ball focused hitters.

Cluster 3 (Blue, squares)
  Griffin Conine, Victor Mesa Jr., Jakob Marsee.
  These hitters have lower strikeouts and moderate power.
  Likely balanced with good contact and some power.
  
Interpretation of the axes (PC1 and PC2):
  PC1 (x-axis) probably separates based on power and strikeout tendencies.
  PC2 (y-axis) might reflect patience or contact ability differences.
'''
