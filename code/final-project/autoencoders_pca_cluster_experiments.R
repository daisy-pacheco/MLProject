library(tidyverse)
library(h2o)

set.seed(123)
h2o.init()

# filtering for full time
full_time_data <- prepped_data %>% 
  filter(hours_worked_week >= 35) %>% 
  select(-full_time)

features <- full_time_data %>% 
  select(-job_satisfaction, -id, -industry)

features <- as.h2o(features)

# autoencoder
ae1 <- h2o.deeplearning(
  x = seq_along(features),
  training_frame = features,
  autoencoder = TRUE,
  hidden = 5,
  activation = 'Tanh'
)

ae1_codings <- h2o.deepfeatures(ae1, features, layer = 1)
ae1_codings

# PCA
my_pca <- h2o.prcomp(
  training_frame = features,
  pca_method = "GramSVD",
  k = ncol(features), 
  transform = "STANDARDIZE", 
  impute_missing = TRUE,
  max_runtime_secs = 1000
)

my_pca

my_pca@model$eigenvectors %>% 
  as.data.frame() %>% 
  mutate(feature = row.names(.)) %>%
  ggplot(aes(pc1, reorder(feature, pc1))) +
  geom_point()

my_pca@model$eigenvectors %>% 
  as.data.frame() %>% 
  mutate(feature = row.names(.)) %>%
  ggplot(aes(pc1, pc2, label = feature)) +
  geom_text()


# clustering

## hierarchical
library(cluster)     
library(factoextra)

full_time_data_sample <- sample_n(full_time_data, 1000)

data_scale <- full_time_data_sample %>%
  select_if(is.numeric) %>%  
  select(-job_satisfaction, -id) %>%  
  mutate_all(as.double) %>%  
  scale() 

data_scale <- as.h2o(data_scale)

hc2 <- agnes(data_scale, method = "complete")

dend_plot <- fviz_dend(hc2)
dend_data <- attr(dend_plot, "dendrogram")
dend_cuts <- cut(dend_data, h = 8)
cluster_plot <- fviz_dend(dend_cuts$lower[[2]])


## k-means
full_time_data_sample <- full_time_data_sample %>% 
  select_if(is.numeric)

kmeans_clustering <- kmeans(full_time_data_sample, centers = 4, nstart = 10)

str(kmeans_clustering)

fviz_nbclust(
  full_time_data_sample, 
  kmeans, 
  k.max = 25,
  method = "wss",
  diss = get_dist(full_time_data_sample, method = "spearman")
)

x <- tapply(full_time_data_sample$avg_age_per_job_centered, kmeans_clustering$cluster, mean)
y <- tapply(full_time_data_sample$hourly_pay_centered, kmeans_clustering$cluster, mean)

kcenters <- data.frame(x, y)

plot <- ggplot(full_time_data_sample, aes(avg_age_per_job_centered, hourly_pay_centered)) +
  geom_point(col = kmeans_clustering$cluster) + 
  geom_point(data = kcenters, aes(x, y), pch = 8, size = 10)

plot
