# Assignemnt 2 - Dimension Reduction Set Assignment 02 as wd
set.seed(32)
jaxPassDefense_ALL = read.csv("../data/jaxPassDefense.csv", stringsAsFactors = FALSE)

# Order by quality as we want to see where the defense is strong and weak
jaxPassDefense = jaxPassDefense_ALL[order(jaxPassDefense_ALL$Quality, decreasing = TRUE),]

# Remove added column and target variable
y = jaxPassDefense$Quality
jaxPassDefense = subset(jaxPassDefense, select = -c(X, Quality))

# Scale the variables we have. We don't want seconds becoming a significant factor 
# just because it has large values
jaxPassDefense_scaled = scale(jaxPassDefense)

# Apparently the QB will always drop back for a pass. I thought maybe a quick snap and throw
# would not be considered a dropback but I was wrong. Also, I forgot to remove qb scramble.
# When the qb scrambles, it is then a run play. We will remove these two columns since 
# they do not change.
# Also, no safeties scored for the defense so we'll remove that.
jaxPassDefense_scaled = subset(jaxPassDefense, select = -c(qb_dropback, qb_scramble, safety))
jaxPassDefense_PC = prcomp(jaxPassDefense_scaled)

# Looking at PC1, we can see a lot of negative values when it comes to the late game. It also
# made me realize many of the variables are tracking the same thing (drive, seconds left,
# half seconds left, qtr, etc.). I'm going to remove a few of these in hopes that it can
# give us more insight into other possible factors.
jaxPassDefense = subset(jaxPassDefense, select = -c(half_seconds_remaining, 
                                                    game_seconds_remaining,
                                                    drive_Late, drive_Mid, 
                                                    qb_dropback, qb_scramble, safety,
                                                    game_half_Half2, incomplete_pass))

normalizeColumn = function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

normalizeData = function() {
  for(col in 1:ncol(jaxPassDefense)) {
    jaxPassDefense[col] = normalizeColumn(jaxPassDefense[col])
  }
  return(jaxPassDefense)
}

jaxPassDefense_scaled = normalizeData()

write.csv(jaxPassDefense_scaled, "../data/jaxPassDefenseScaled.csv")

plot(
  x = jaxPassDefense$yards_gained
)

plot(
  x = jaxPassDefense_scaled$yards_gained
)

jaxPassDefense_PC = prcomp(jaxPassDefense_scaled)
jaxPassDefense_PC
summary(jaxPassDefense_PC)
str(jaxPassDefense_PC)
head(jaxPassDefense_PC$x)

plot(jaxPassDefense_PC)
biplot(jaxPassDefense_PC)

require("ggfortify")
autoplot(
  object = jaxPassDefense_PC,
  data = jaxPassDefense_ALL,
  colour = "Quality"
)

factoextra::fviz_eig(jaxPassDefense_PC)
factoextra::fviz_pca_ind(
  jaxPassDefense_PC,
  col.ind = "cos2",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
)

factoextra::fviz_pca_var(
  X = jaxPassDefense_PC,
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
)

factoextra::get_eigenvalue(jaxPassDefense_PC)
# Using this, we can see that the only variance above double digits is PC1. To get to 90%
# cumulative variance we would need the first 14 dimensions, a reduction of 16 variables, or 53%

jaxPassDefense_get_vars = factoextra::get_pca_var(jaxPassDefense_PC)
jaxPassDefense_get_vars$coord
jaxPassDefense_get_vars$cos2
jaxPassDefense_get_vars$contrib

jaxPassDefense_get_ind = factoextra::get_pca_ind(jaxPassDefense_PC)
head(jaxPassDefense_get_ind$coord)
head(jaxPassDefense_get_ind$cos2)
head(jaxPassDefense_get_ind$contrib)
 
################ t-SNE -> t- distributed stochastic Neighbor Embedding ############
require("Rtsne")
#seed set above
jaxPassDefense_tsne = Rtsne::Rtsne(jaxPassDefense_scaled)

plot(
  jaxPassDefense_tsne$Y,
  col = y,
  pch = as.character(y),
  main = "Scatter Plot of Jax Pass Defense T-SNE Two Dimensions"
)

jaxPassDefense_prcomp = prcomp(
  x = jaxPassDefense_scaled,
  scale. = TRUE,
  center = TRUE,
  rank = 2
)

plot(
  jaxPassDefense_prcomp$x[,1:2],
  col = y,
  pch = as.character(y),
  main = "Scatter Plot of Jax Pass Defense PCA Two Dimensions"
)

jaxPassDefense_tsne2 = Rtsne::Rtsne(
  X = jaxPassDefense_scaled,
  dims = 2, 
  PCA = FALSE,
  max_iter = 2000,
  perplexity = 60
)

plot(jaxPassDefense_tsne2$costs)

plot(
  jaxPassDefense_tsne2$Y,
  col = y,
  pch = as.character(y),
  main = "Scatter Plot of Jax Pass Defense T-SNE Two Dimensions"
)

############ Non-negative matrix ##################
options(scipen = 1, digits = 2)
jaxPassDefense_nmf = NMF::nmf(
  x = jaxPassDefense_scaled,
  rank = 2,
  seed = 32
)

jaxPassDefense_nmf

jaxPassDefense_basis = NMF::basis(jaxPassDefense_nmf)
jaxPassDefense_coef = NMF::coef(jaxPassDefense_nmf)

dim(jaxPassDefense_scaled)
dim(jaxPassDefense_basis)
dim(jaxPassDefense_coef)

round(head(jaxPassDefense_basis), 3)

round(head(jaxPassDefense_coef), 3)

plot(
  x = jaxPassDefense_basis,
  col = y,
  pch = as.character(y)
)
