# Assignment 01 Part 1 Partitioning

# Read in cleaned data
jaxRunPlayByPlay = read.csv("../data/jaxRunData.csv", stringsAsFactors = FALSE)

jaxRunPlayByPlay = subset(jaxRunPlayByPlay, select = -c(X))

kmeans_jax_1 = kmeans(
  x = jaxRunPlayByPlay,
  centers = 5
)

# basic view
kmeans_jax_1

#Doesn't really tell us much... lets center and scale

