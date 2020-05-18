# Assignment 01 Part 2 Hierarchical

# Read in cleaned data
jaxRunPlayByPlay = read.csv("../data/jaxRunData.csv", stringsAsFactors = FALSE)

jaxRunPlayByPlay = subset(jaxRunPlayByPlay, offensive_play == 1, select = -c(X, offensive_play))
jaxRunPlayByPlay = jaxRunPlayByPlay[with(jaxRunPlayByPlay, order(-yards_gained, -td_team)), ]

scale_jax = scale(jaxRunPlayByPlay)

dist(scale_jax[1:5,])

# Distance matrix 
dist_jax <- dist(scale_jax)
heatmap(
  x = as.matrix(dist_jax),
  col = viridis::viridis(256)
)

factoextra::fviz_dist(dist_jax)

qgraph::qgraph(
  input = 1/dist_jax,
  layout="spring",
  minimum = 0.3
)

# Now for real clustering
hclust_jax <- hclust(dist_jax)
plot(hclust_jax)

cutree_jax_4 <- cutree(
  tree = hclust_jax,
  k = 4
)

prcomp_jax_4 <- data.frame(
  prcomp(
    x = scale_jax,
    center = FALSE,
    scale. = FALSE
  )$x[,1:2],
  Name = rownames(jaxRunPlayByPlay),
  Rank = jaxRunPlayByPlay$ydsnet,
  Points = jaxRunPlayByPlay$yards_gained,
  Cluster = as.character(cutree_jax_4),
  stringsAsFactors = FALSE
)

require(ggplot2)

ggplot(prcomp_jax_4) + 
  aes(x = PC1,y = PC2,size = Points,color = Cluster,fill = Cluster,label = Name,group = Cluster) + 
  geom_point() + 
  ggrepel::geom_text_repel(color = "black",size = 3) + 
  ggtitle("Scatter plot of Jacksonville running plays principal components K = 4","Color corresponds to k-means cluster") + 
  theme_bw() + 
  theme(legend.position = "none")

silhouette_jax_4 <- cluster::silhouette(
  x = cutree_jax_4,
  dist = dist_jax
)
plot(silhouette_jax_4)

# -------------------- k = 6 -----------------------------------
cutree_jax_6 <- cutree(
  tree = hclust_jax,
  k = 6
)

prcomp_jax_6 <- data.frame(
  prcomp(
    x = scale_jax,
    center = FALSE,
    scale. = FALSE
  )$x[,1:2],
  Name = rownames(jaxRunPlayByPlay),
  Rank = jaxRunPlayByPlay$ydsnet,
  Points = jaxRunPlayByPlay$yards_gained,
  Cluster = as.character(cutree_jax_6),
  stringsAsFactors = FALSE
)

require(ggplot2)

ggplot(prcomp_jax_6) + 
  aes(x = PC1,y = PC2,size = Points,color = Cluster,fill = Cluster,label = Name,group = Cluster) + 
  geom_point() + 
  ggrepel::geom_text_repel(color = "black",size = 3) + 
  ggtitle("Scatter plot of Jacksonville running plays principal components K = 6","Color corresponds to k-means cluster") + 
  theme_bw() + 
  theme(legend.position = "none")

silhouette_jax_6 <- cluster::silhouette(
  x = cutree_jax_6,
  dist = dist_jax
)
plot(silhouette_jax_6)

# -----------------Stick with k = 4------------------------------------
# ditances
v_dist <- c(
  "canberra","manhattan","euclidean","maximum","minkowski"
  #,"binary"
)
list_dist <- lapply(
  X = v_dist,
  FUN = function(distance_method) dist(
    x = scale_jax,
    method = distance_method
  )
)
names(list_dist) <- v_dist
v_hclust <- c(
  "ward.D","ward.D2","complete","mcquitty","average","single"
  #,"median","centroid"
)

# clustering methods
list_hclust <- list()
for(j in v_dist) for(k in v_hclust) list_hclust[[j]][[k]] <- hclust(
  d = list_dist[[j]],
  method = k
)
par(
  mfrow = c(length(v_dist),length(v_hclust)),
  mar = c(0,0,0,0),
  mai = c(0,0,0,0),
  oma = c(0,0,0,0)
)
for(j in v_dist) for(k in v_hclust) plot(
  x = list_hclust[[j]][[k]],
  labels = FALSE,
  axes = FALSE,
  main = paste("\n",j,"\n",k)
)

# Converting height
for(j in v_dist) for(k in v_hclust) list_hclust[[j]][[k]]$height <- rank(list_hclust[[j]][[k]]$height)
par(
  mfrow = c(length(v_dist),length(v_hclust)),
  mar = c(0,0,0,0),
  mai = c(0,0,0,0),
  oma = c(0,0,0,0)
)
for(j in v_dist) for(k in v_hclust) plot(
  x = list_hclust[[j]][[k]],
  labels = FALSE,
  axes = FALSE,
  main = paste("\n",j,"\n",k)
)

# choose best plot
M_coef <- matrix(
  data = NA,
  nrow = length(v_dist),
  ncol = length(v_hclust)
)
rownames(M_coef) <- v_dist
colnames(M_coef) <- v_hclust
for(j in v_dist) for(k in v_hclust) try({
  M_coef[j,k] <- cluster::coef.hclust(
    object = list_hclust[[j]][[k]]
  )
})
dev.off()
## null device 
##           1

M_coef

# ward.D and canberra have largest value
plot(
  x = list_hclust[["canberra"]][["ward.D"]],
  main = "Canberrra Ward's D",
  sub = ""
)

#Smallest
plot(
  x = list_hclust[["minkowski"]][["single"]],
  main = "Maximum Single",
  sub = ""
)

# Agglomerative Nesting (Hierarchical Clustering)
v <- prcomp(scale_jax)$x[,1]
agnes_jax <- cluster::agnes(scale_jax)
plot(agnes_jax)

as.hclust(
  x = agnes_jax
)

print(agnes_jax)

cutree(
  tree = agnes_jax,
  k = 4
)

# DIvisive ANAlysis Clustering
diana_jax <- cluster::diana(scale_jax)
plot(diana_jax)

as.hclust(
  x = diana_jax
)


as.dendrogram(
  object = diana_jax
)

cutree(
  tree = diana_jax,
  k = 4
)


