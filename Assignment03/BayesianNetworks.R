# Assignment 03 Bayesian netwroks

require(bnlearn)
# scaled with normalization
jaxPassDefense_scaled = read.csv("../data/jaxPassDefenseScaled.csv")
# Remove id column and timeouts because they do not appear important.
jaxPassDefense_scaled = subset(jaxPassDefense_scaled, select = -c(X, jax_timeouts_remaining, opp_timeouts_remaining))

# Set columns as factors or numerical data
factorColumns = c("goal_to_go", "shotgun", "no_huddle", "first_down_pass", "interception", 
                  "side_of_field_MID", "side_of_field_OPP", "qtr_2", "qtr_3", "qtr_4", "down_2", 
                  "down_3", "down_4", "pass_length_short", "pass_location_middle", "pass_location_right",
                  "td_team_1", "td_team_2")
numericColumns = c("yardline_100", "quarter_seconds_remaining", "ydstogo", "ydsnet", "yards_gained", 
                   "air_yards", "yards_after_catch", "score_differential", "jax_score", "opp_score")

jaxPassDefense_scaled[, factorColumns] = lapply(jaxPassDefense_scaled[, factorColumns], as.factor)
jaxPassDefense_scaled[, numericColumns] = lapply(jaxPassDefense_scaled[, numericColumns], as.numeric)

# make values discrete
summary(jaxPassDefense_scaled)

jaxPassDefense_scaled_generalDiscrete = discretize(jaxPassDefense_scaled)
# discretize the data using three methods
# Quantile and hartemink caused errors no matter the number of breaks.
jaxPassDefense_scaled_discretized = lapply(
  X = c("interval"),
  FUN = function(method) discretize(
    data = jaxPassDefense_scaled,
    method = method,
    breaks = 4,
    ordered = TRUE
  )
)

names(jaxPassDefense_scaled_discretized) = c("interval")
lapply(X = jaxPassDefense_scaled_discretized, FUN = summary)

require("Rgraphviz")
############ Practice with earlier bayesian model video ##########
jaxPassDefense_scored = hc(jaxPassDefense_scaled_generalDiscrete)

graphviz.plot(jaxPassDefense_scored)

jaxPassDefense_arcStrength = arc.strength(
  x = jaxPassDefense_scored,
  data = jaxPassDefense_scaled_generalDiscrete
)
strength.plot(
  x = jaxPassDefense_scored,
  strength = jaxPassDefense_arcStrength
)

# Model fitting
v_models <- c(
  "pc.stable","gs","iamb","fast.iamb","inter.iamb","iamb.fdr",
  "hc", "tabu",
  "mmhc","rsmax2","h2pc",
  "mmpc","si.hiton.pc","hpc", 
  "chow.liu","aracne"
)

list_crossValidation <- list()
for(j in v_models) try({ 
  list_crossValidation[[j]] <- bn.cv(
    data = jaxPassDefense_scaled_generalDiscrete,
    bn = j,
    k = 2,
    runs = 2
  )
},silent = TRUE)
list_crossValidation

list_mean <- list()
for(j in names(list_crossValidation)){
  for(k in 1:length(list_crossValidation[[j]])){
    list_mean[[j]][[k]] <- rep(NA,length(list_crossValidation[[j]][[k]]))
    for(l in 1:length(list_crossValidation[[j]][[k]])){
      list_mean[[j]][[k]][l] <- list_crossValidation[[j]][[k]][[l]]$loss
    }
  }
  list_mean[[j]] <- unlist(list_mean[[j]])
}
sort(base::sapply(X = list_mean,FUN = mean))

########### Score based #########################
# Calculated above, here we'll just call on the model
jaxPassDefense_scored
bnlearn::score(jaxPassDefense_scored, jaxPassDefense_scaled_generalDiscrete)
########### Constraint based ####################
jaxPassDefense_constraint = iamb.fdr(jaxPassDefense_scaled_generalDiscrete)
jaxPassDefense_constraint
jaxPassDefense_arcStrength_constraint = arc.strength(
  x = jaxPassDefense_constraint,
  data = jaxPassDefense_scaled_generalDiscrete
)
strength.plot(
  x = jaxPassDefense_constraint,
  strength = jaxPassDefense_arcStrength_constraint
)
bnlearn::score(jaxPassDefense_constraint, jaxPassDefense_scaled_generalDiscrete)
########### Hybrid based ########################
jaxPassDefense_hybrid = h2pc(jaxPassDefense_scaled_generalDiscrete)
jaxPassDefense_hybrid
jaxPassDefense_arcStrength_hybrid = arc.strength(
  x = jaxPassDefense_hybrid,
  data = jaxPassDefense_scaled_generalDiscrete
)
strength.plot(
  x = jaxPassDefense_hybrid,
  strength = jaxPassDefense_arcStrength_hybrid
)
bnlearn::score(jaxPassDefense_hybrid, jaxPassDefense_scaled_generalDiscrete)
########### Local Discovery based ###############
jaxPassDefense_local = aracne(jaxPassDefense_scaled_generalDiscrete)
jaxPassDefense_local
jaxPassDefense_arcStrength_local = arc.strength(
  x = jaxPassDefense_local,
  data = jaxPassDefense_scaled_generalDiscrete
)
strength.plot(
  x = jaxPassDefense_local,
  strength = jaxPassDefense_arcStrength
)
bnlearn::score(jaxPassDefense_local, jaxPassDefense_scaled_generalDiscrete)

#### Try a different approach for undirected scores too
all4Algorithms = c("hc", "iamb.fdr", "h2pc", "aracne")
bnlearnList = list()

for(j in all4Algorithms) for(k in names(jaxPassDefense_scaled_discretized)) try({
  bnlearnList[[j]][[k]] <- do.call(
    what = j,
    args = list(x = jaxPassDefense_scaled_discretized[[k]])
  )
  M_arcs <- arcs(bnlearnList[[j]][[k]])
  for(l in 1:nrow(M_arcs)){
    bnlearnList[[j]][[k]] <- set.arc(
      x = bnlearnList[[j]][[k]],
      from = M_arcs[l,1],
      to = M_arcs[l,2],
      check.cycles = FALSE,
      check.illegal = FALSE
    )
    bnlearnList[[j]][[k]] <- choose.direction(
      x = bnlearnList[[j]][[k]],
      arc = M_arcs[l,],
      data = jaxPassDefense_scaled_discretized[[k]]
    )
  }
},silent = TRUE)

networkScores <- matrix(
  data = NA,
  nrow = length(all4Algorithms),
  ncol = length(jaxPassDefense_scaled_discretized),
)
rownames(networkScores) <- all4Algorithms
colnames(networkScores) <- names(jaxPassDefense_scaled_discretized)

for(j in all4Algorithms) for(k in names(jaxPassDefense_scaled_discretized)) try({
  networkScores[j,k] <- bnlearn::score(
    x = bnlearnList[[j]][[k]],
    data = jaxPassDefense_scaled_discretized[[k]],
    type = "aic"
  )
})

networkScores

graphviz.plot(
  bnlearnList[[1]][[1]]
)
