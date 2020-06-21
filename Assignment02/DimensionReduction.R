# Assignemnt 2 - Dimension Reduction Set Assignment 02 as wd

jaxPassDefense = read.csv("../data/jaxPassDefense.csv", stringsAsFactors = FALSE)

# Remove added column
jaxPassDefense = subset(jaxPassDefense, select = -c(X))

# Order by yards gained as we want to see where the defense is strong and weak and
# this will likely be the best metric
jaxPassDefense = jaxPassDefense[order(jaxPassDefense$yards_gained, decreasing = TRUE),]

# Remove target variable
# jaxPassDefense = subset(jaxPassDefense, select = -c(yards_gained))

# The above was removed from execution because the yards gained is not the target variable
# like I considered. Instead, the target variable is more unknown. The idea of the target
# is to witness where weak, intermediate, and strong points in the defense are. We do not 
# know the number of weak points in pass defense, so we do not know how many possible clusters
# there are. Weaknesses could range from the the time of the game to the location of the 
# pass and everywhere in-between. We do not know when players way give less effort or is there
# is a consistence player giving up passes in certain scenarios. 

# Scale the variables we have. We don't want seconds becoming a significant factor 
# just because it has large values
jaxPassDefense_scaled = scale(jaxPassDefense)

# Apparently the QB will always drop back for a pass by. I thought maybe a quick snap and throw
# would not be considered a dropback but I was wrong. Also, I forgot to remove qb scramble.
# When the qb scrambles, it is then a run play. We will remove these two columns since 
# they do not change.
jaxPassDefense_scaled = subset(jaxPassDefense_scaled, select = -c(qb_dropback, qb_scramble))
jaxPassDefense_PC = prcomp(jaxPassDefense_scaled)

# Looking at PC1, we can see a lot of negative values when it comes to the late game. It also
# made me realize many of the variables are tracking the same thing (drive, seconds left,
# half seconds left, qtr, etc.). I'm going to remove a few of these in hopes that it can
# give us more insight into other possible factors.
jaxPassDefense_scaled = subset(jaxPassDefense_scaled, select = -c(half_seconds_remaining,
                                                                  game_seconds_remaining,
                                                                  drive_Late, drive_Mid))

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
  data = jaxPassDefense,
  colour = "yards_gained"
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

# Using this, we can see that the only variance above double digits is PC1. To get to 90%
# cumulative variance we would need the first 18 dimensions, which is a reduction of 
# 11 dimensions(~40% of the total dimensions).
factoextra::get_eigenvalue(jaxPassDefense_PC)

jaxPassDefense_get_vars = factoextra::get_pca_var(jaxPassDefense_PC)
jaxPassDefense_get_vars$coord
jaxPassDefense_get_vars$cos2
jaxPassDefense_get_vars$contrib

jaxPassDefense_get_ind = factoextra::get_pca_ind(jaxPassDefense_PC)
head(jaxPassDefense_get_ind$coord)
head(jaxPassDefense_get_ind$cos2)
head(jaxPassDefense_get_ind$contrib)

################ t-DSNE -> t- distributed stochastic Neighbor Embedding############