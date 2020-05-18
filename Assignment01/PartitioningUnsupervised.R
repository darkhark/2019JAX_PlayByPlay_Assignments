# Assignment 01 Part 1 Partitioning

# Read in cleaned data
jaxRunPlayByPlay = read.csv("../data/jaxRunData.csv", stringsAsFactors = FALSE)

jaxRunPlayByPlay = subset(jaxRunPlayByPlay, offensive_play == 1, select = -c(X, offensive_play))
jaxRunPlayByPlay = jaxRunPlayByPlay[with(jaxRunPlayByPlay, order(-yards_gained, -td_team)), ]

# K = 6
kmeans_jax_1 = kmeans(
  x = jaxRunPlayByPlay,
  centers = 6
)

# basic view
kmeans_jax_1

#Doesn't really tell us much... lets center and scale
scale_jax = scale(jaxRunPlayByPlay)

kmeans_jax_2 = kmeans(
  x = scale_jax,
  centers = 6
)

kmeans_jax_2

# Now for the centers
# These decisions are based on what I believe could lead to the most success, based on my experience.
# 0 was used if I believed it was neutral, 3 if the higher number seems better, and -3 if the lower number seems better.
v = c(0, -3, -3, -3, -3, 3, 3, 3, 3, -3, 3, -3, 3, 0, -3, 0, -3, 0, 0, 3, 3, 3, -3, 3, -3)
v2 = c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3)

initial_centers = matrix(
  data = c(v, -v, abs(v), -abs(v), v2, -v2),
  nrow = 6,
  byrow = TRUE
)

rownames(initial_centers) = c(
  "Best cases",
  "Worse Cases",
  "Most High",
  "Most Low",
  "First Half High",
  "First Half Low"
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

# different plot
useful::plot.kmeans(
  x = kmeans_jax_3,
  data = scale_jax
)

# The Sum of squares is far too low (26.1% on last run)
cluster::clusplot(
  scale_jax,
  kmeans_jax_3$cluster,
  color=TRUE,
  shade=TRUE,
  labels=2,
  lines=0
)

fpc::plotcluster(
  x=scale_jax,
  clvecd = kmeans_jax_3$cluster
)

factoextra::fviz_cluster(
  object=kmeans_jax_3,
  data=scale_jax
)

# Many of the plots indicate we have some heavy overlapping in our clusters. There's likely a more
# optimal k we can use. We can check this using the elbow method.

set.seed(32)
factoextra::fviz_nbclust(
  x = scale_jax,
  FUNcluster = kmeans,
  method = "wss"
)

factoextra::fviz_nbclust(
  x = scale_jax,
  FUNcluster = kmeans,
  method = "silhouette"
)

# Looks like 4 was actually pretty close to the ideal number of clusers, but 3 appears to be the optimal number. Let's gather
# some statistics.
# seed already set 
clusGap_kmeans <- cluster::clusGap(
  x = scale_jax,
  FUNcluster = kmeans,
  K.max = 40
)
clusGap_kmeans

# Plot those results
factoextra::fviz_gap_stat(
  gap_stat = clusGap_kmeans
)

# This time 6 was marked as the optimal number of clusters because it was the first peak before the clusters fell again. 
# 15 looks like another good possibility.

#Let's look at another plot.
factoextra::fviz_nbclust(
  x = scale_jax,
  FUNcluster = kmeans,
  method = "gap_stat"
)

# Once again, 6 was found to be the best.

#Let's look at some other stats
fpc::cluster.stats(
  d = dist(scale_jax),
  clustering = kmeans_jax_3$cluster,
  G2 = TRUE,
  G3 = TRUE
)

# Although our data is not that large, we try clara anyway. We will use k = 6 since most visualizations and stats seemed to
# defend that number. Also, when we ran 4, some of the clusters heavily overlapped each other.
#Clustering Large Application
clara_jax <- cluster::clara(
  x = scale_jax,
  k = 6
)
plot(clara_jax)
print(clara_jax)

# Still a lot of heavy overlapping :/ Maybe it's even worse now.

# Fuzzy analysis clustering
fanny_jax <- cluster::fanny(
  x = scale_jax,
  k = 6
)

## Warning in cluster::fanny(x = scale_M, k = 6): the memberships are all very
## close to 1/k. Maybe decrease 'memb.exp' ?
plot(fanny_jax)
print(fanny_jax)
# Useless

# Partitioning Around Medoids
pam_jax <- cluster::pam(scale_jax,k = 6)
plot(pam_jax)
print(pam_jax)

# Looks slightly better than clara, but still a fair amount of overlap

# --------------------------------------K = 4--------------------------------------------------------------
kmeans_jax_4 = kmeans(
  x = jaxRunPlayByPlay,
  centers = 4
)

# basic view
kmeans_jax_4

kmeans_jax_5 = kmeans(
  x = scale_jax,
  centers = 4
)

kmeans_jax_5

# Now for the centers
# These decisions are based on what I believe could lead to the most success, based on my experience.
# 0 was used if I believed it was neutral, 3 if the higher number seems better, and -3 if the lower number seems better.
v = c(0, -3, -3, -3, -3, 3, 3, 3, 3, -3, 3, -3, 3, 0, -3, 0, -3, 0, 0, 3, 3, 3, -3, 3, -3)

initial_centers_2 = matrix(
  data = c(v, -v, abs(v), -abs(v)),
  nrow = 4,
  byrow = TRUE
)

rownames(initial_centers_2) = c(
  "Best cases",
  "Worse Cases",
  "Most High",
  "Most Low"
)

kmeans_jax_6 = kmeans(
  x = scale_jax,
  centers = initial_centers_2
)

kmeans_jax_6

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
  Cluster = as.character(kmeans_jax_6$cluster),
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

# different plot
useful::plot.kmeans(
  x = kmeans_jax_6,
  data = scale_jax
)

# The Sum of squares is far too low (26.1% on last run)
cluster::clusplot(
  scale_jax,
  kmeans_jax_6$cluster,
  color=TRUE,
  shade=TRUE,
  labels=2,
  lines=0
)

fpc::plotcluster(
  x=scale_jax,
  clvecd = kmeans_jax_6$cluster
)

factoextra::fviz_cluster(
  object=kmeans_jax_6,
  data=scale_jax
)

#Let's look at some other stats
fpc::cluster.stats(
  d = dist(scale_jax),
  clustering = kmeans_jax_6$cluster,
  G2 = TRUE,
  G3 = TRUE
)

# Although our data is not that large, we try clara anyway. We will use k = 6 since most visualizations and stats seemed to
# defend that number. Also, when we ran 4, some of the clusters heavily overlapped each other.
#Clustering Large Application
clara_jax <- cluster::clara(
  x = scale_jax,
  k = 4
)
plot(clara_jax)
print(clara_jax)

# Still a lot of heavy overlapping

# Partitioning Around Medoids
pam_jax <- cluster::pam(scale_jax,k = 4)
plot(pam_jax)
print(pam_jax)

# Looks slightly better than clara, but still a fair amount of overlap
