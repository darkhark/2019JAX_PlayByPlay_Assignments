data2019 = read.csv("reg_pbp_2019.csv", stringsAsFactors = FALSE)

jaxData = subset(data2019, away_team == "JAX" | home_team == "JAX")

jaxRunOrPass = subset(jaxData, play_type == "run" | play_type == "pass")

# Is entire column NA?
jaxRunOrPass = jaxRunOrPass[, colSums(is.na(jaxRunOrPass)) != nrow(jaxRunOrPass)]

# After column 55 is all probablilities. Don't care about those
jaxRunOrPass = jaxRunOrPass[c(0:55)]

# Other columns that are not immediately useful for this data
jaxRunOrPass = subset(jaxRunOrPass, select = -c(desc, play_id, game_id, two_point_conv_result, timeout_team,
                                                quarter_end, sp, qb_spike, qb_kneel, timeout, posteam_timeouts_remaining,
                                                defteam_timeouts_remaining, posteam_score, defteam_score, posteam_score_post,
                                                defteam_score_post, score_differential_post, yrdln, game_date, time))


# Commented out because it doesn't appear to add real value
# Make defensive yards given up negative
# alterYardageForDefense = function(df) {
#   for(row in 1:nrow(df)) {
#     if (df[row, "posteam"] != "JAX") {
#       df[row, "yards_gained"] = -df[row, "yards_gained"]
#       df[row, "air_yards"] = -df[row, "air_yards"]
#       df[row, "yards_after_catch"] = -df[row, "yards_after_catch"]
#     }
#   }
#   return(df)
# }

# Change columns to always be in the perspective of JAX.
changeColumnsToRepresentJAXOrOPP = function(df) {
  for(row in 1:nrow(df)) {
    if (df[row, "home_team"] == "JAX") {
      df[row, "jax_score"] = df[row, "total_home_score"]
      df[row, "opp_score"] = df[row, "total_away_score"]
      df[row, "jax_timeouts_remaining"] = df[row, "home_timeouts_remaining"]
      df[row, "opp_timeouts_remaining"] = df[row, "away_timeouts_remaining"]
    } else if(df[row, "away_team"] == "JAX") {
      df[row, "opp_score"] = df[row, "total_home_score"]
      df[row, "jax_score"] = df[row, "total_away_score"]
      df[row, "jax_timeouts_remaining"] = df[row, "away_timeouts_remaining"]
      df[row, "opp_timeouts_remaining"] = df[row, "home_timeouts_remaining"]
    }
    
    if (df[row, "side_of_field"] != "JAX" & df[row, "side_of_field"] != "MID") {
      levels(df$side_of_field) = c("JAX", "OPP")
      df[row, "side_of_field"] = "OPP"
    }
    
    if (!is.na(df[row, "td_team"]) & df[row, "td_team"] != "JAX") {
      levels(df$td_team) = c("JAX", "OPP")
      df[row, "td_team"] = "OPP"
    }
    
    # No team scored = 0, JAX scored = 1, OPP scored = 2
    if (is.na(df[row, "td_team"])) {
      df[row, "td_team"] = 0
    } else if (df[row, "td_team"] == "JAX") {
      df[row, "td_team"] = 1
    } else {
      df[row, "td_team"] = 2
    }
    
    # Offense(1) or defense(0) for Jax
    if (df[row, "posteam"] == "JAX") {
      df[row, "offensive_play"] = 1
    } else {
      df[row, "offensive_play"] = 0
    }
  
    # A little outside of the realm of this method, but while we're iterating through
    # each row, might as well fix it here. 
    # Categorize the drives as an early, mid, or late game drive
    if (as.numeric(df[row, "drive"]) < 7) {
      df[row, "drive"] = "Early"
    } else if (as.numeric(df[row, "drive"]) >= 7 && as.numeric(df[row, "drive"]) < 15) {
      df[row, "drive"] = "Mid"
    } else {
      df[row, "drive"] = "Late"
    }
  }
  
  df = subset(df, select = -c(total_away_score, total_home_score, defteam, posteam, posteam_type, home_timeouts_remaining,
                              away_timeouts_remaining, home_team, away_team))
  return(df)
}

#jaxRunOrPass = alterYardageForDefense(jaxRunOrPass)
jaxRunOrPass = changeColumnsToRepresentJAXOrOPP(jaxRunOrPass)

write.csv(jaxRunOrPass, "jaxRunOrPassData.csv")

jaxRun = subset(jaxRunOrPass, play_type == "run" & !is.na(run_location), select = -c(pass_length, pass_location, air_yards, yards_after_catch, play_type))
jaxPass = subset(jaxRunOrPass, play_type == "pass" & !is.na(pass_length), select = -c(run_location, run_gap, play_type))
addCenterAsGap = function(df) {
  for (row in 1:nrow(df)) {
    if (df[row, "run_location"] == "middle" & is.na(df[row, "run_gap"])) {
      df[row, "run_gap"] = "center"
    }
  }
  return(df)
}

# Makes yards after catch -1 if the pass was not caught
makeMissedPassesNegative = function(df) {
  for (row in 1:nrow(df)) {
    if (is.na(df[row, "yards_after_catch"])) {
      df[row, "yards_after_catch"] = -1
    }
  }
  return(df)
}

jaxRun = addCenterAsGap(jaxRun)
jaxPass = makeMissedPassesNegative(jaxPass)

jaxRunDefense = subset(jaxRun, offensive_play == 0, select = -c(offensive_play))
jaxPassDefense = subset(jaxPass, offensive_play == 0, select = -c(offensive_play))

jaxPassDefense = fastDummies::dummy_cols(jaxPassDefense, remove_first_dummy = TRUE, select_columns=c("side_of_field",
                                         "game_half", "drive", "qtr", "down", "pass_length",
                                         "pass_location", "td_team"))

# Remove columns that the dummy variables represent
jaxPassDefense = subset(jaxPassDefense, select = -c(side_of_field, game_half, drive, qtr, down, pass_length, 
                                                    pass_location, td_team))

write.csv(jaxPassDefense, "jaxPassDefense.csv")
