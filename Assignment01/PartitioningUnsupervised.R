# Assignment 01 Part 1 Partitioning

# Read in cleaned data
jaxRunPlayByPlay = read.csv("../data/jaxRunData.csv", stringsAsFactors = FALSE)

jaxRunPlayByPlay = subset(jaxRunPlayByPlay, offensive_play == 1, select = -c(X, offensive_play))
jaxRunPlayByPlay = jaxRunPlayByPlay[with(jaxRunPlayByPlay, order(-yards_gained, -td_team)), ]

kmeans_jax_1 = kmeans(
  x = jaxRunPlayByPlay,
  centers = 4
)

# basic view
kmeans_jax_1

#Doesn't really tell us much... lets center and scale
scale_jax = scale(jaxRunPlayByPlay)

kmeans_jax_2 = kmeans(
  x = scale_jax,
  centers = 4
)

kmeans_jax_2

# Now for the centers
# These decisions are based on what I believe could lead to the most success, based on my experience.
# 0 was used if I believed it was neutral, 3 if the higher number seems better, and -3 if the lower number seems better.
v = c(0, -3, -3, -3, -3, 3, 3, 3, 3, -3, 3, -3, 3, 0, -3, 0, -3, 0, 0, 3, 3, 3, -3, 3, -3)

initial_centers = matrix(
  data = c(v, -v, abs(v), -abs(v)),
  nrow = 4,
  byrow = TRUE
)

rownames(initial_centers) = c(
  "Best cases",
  "Worse Cases",
  "All High",
  "All Low"
)

kmeans_jax_3 = kmeans(
  x = scale_jax,
  centers = initial_centers
)

kmeans_jax_3

# Visualize what we have so far
prcomp_jax = data.frame(
  prcomp(
    x = scale_jax, 
    center = FALSE,
    scale. = FALSE
  )$x[, 1:2],
  Name = rownames(jaxRunPlayByPlay),
  Rank = jaxRunPlayByPlay$ydsnet,
  Points = jaxRunPlayByPlay$yards_gained,
  Cluster = as.character(kmeans_jax_3$cluster),
  stringsAsFactors = FALSE
)

require(ggplot2)

require(ggforce)

ggplot(prcomp_jax) + 
  aes(x = PC1, y = PC2, size = Points, color = Cluster, fill = Cluster, label = Name, group = Cluster) + geom_point() + 
  ggrepel::geom_text_repel(color = "black",size = 3) + 
  ggtitle("Scatter plot of Jacksonville Running plays principal components","Color corresponds to k-means cluster") + 
  theme_bw() + 
  theme(legend.position = "top")
