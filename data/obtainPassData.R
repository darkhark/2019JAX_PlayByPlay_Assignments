data2019 = read.csv("reg_pbp_2019.csv", stringsAsFactors = FALSE)

jaxData = subset(data2019, away_team == "JAX" | home_team == "JAX")

write.csv(jaxData, "jaxPlayByPlay.csv")

jaxPass = subset(jaxData, play_type == "pass")

# Is entire column NA?
jaxPass = jaxPass[, colSums(is.na(jaxPass)) != nrow(jaxPass)]

write.csv(jaxPass, "partial.csv")

# Pick variables that may be of use (ex. probabilities won't be)
jaxPass = jaxPass %>% dplyr::select(0:53, first_down_pass, incomplete_pass, interception, safety)

# Other columns that are not immediately useful for this data
jaxPass = subset(jaxPass, select = -c(desc, play_id, game_id, two_point_conv_result, timeout_team,
                                                quarter_end, sp, qb_spike, qb_kneel, timeout, posteam_timeouts_remaining,
                                                defteam_timeouts_remaining, posteam_score, defteam_score, posteam_score_post,
                                                defteam_score_post, score_differential_post, yrdln, game_date, time, play_type))



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

jaxPass = changeColumnsToRepresentJAXOrOPP(jaxPass)
jaxPass = subset(jaxPass, !is.na(pass_length))

# Makes yards after catch 0 if the pass was not caught
makeMissedPassesZero = function(df) {
  for (row in 1:nrow(df)) {
    if (is.na(df[row, "yards_after_catch"])) {
      df[row, "yards_after_catch"] = 0
    }
  }
  return(df)
}

jaxPass = makeMissedPassesZero(jaxPass)
write.csv(jaxPass, "jaxPassData.csv")

jaxPassDefense = subset(jaxPass, offensive_play == 0, select = -c(offensive_play))

# Y has to be created here to account for first down on play
passOver10Yards = function(row) {
  if(jaxPassDefense[row, "td_team"] == 2 || jaxPassDefense[row, "yards_gained"] >= 20) {
    jaxPassDefense[row, "Quality"] = 0
  } else {
    jaxPassDefense[row, "Quality"] = 1
  }
  return(jaxPassDefense[row, "Quality"])
}

passUnder10YardsNotGoalLine = function(row) {
  if (jaxPassDefense[row, "first_down_pass"] == 1) {
    if (jaxPassDefense[row, "yards_gained"] < 3) {
      jaxPassDefense[row, "Quality"] = 3
    } else if (jaxPassDefense[row, "yards_gained"] < 7) {
      jaxPassDefense[row, "Quality"] = 2
    } else {
      jaxPassDefense[row, "Quality"] = 1
    }
  } else {
    if (jaxPassDefense[row, "yards_gained"] < 1) {
      jaxPassDefense[row, "Quality"] = 4
    } else if (jaxPassDefense[row, "yards_gained"] < 4) {
      jaxPassDefense[row, "Quality"] = 3
    } else {
      jaxPassDefense[row, "Quality"] = 2
    }
  }
  return(jaxPassDefense[row, "Quality"])
}

passUnder10YardsGoalLine = function(row) {
  if (jaxPassDefense[row, "td_team"] == 2) {
    jaxPassDefense[row, "Quality"] = 1
  } else if (jaxPassDefense[row, "yards_gained"] < 1) {
    jaxPassDefense[row, "Quality"] = 4
  } else if (jaxPassDefense[row, "yards_gained"] < 3) {
    jaxPassDefense[row, "Quality"] = 3
  } else if (jaxPassDefense[row, "yards_gained"] < 6){
    jaxPassDefense[row, "Quality"] = 2
  } else {
    jaxPassDefense[row, "Quality"] = 1
  }
  return(jaxPassDefense[row, "Quality"])
}

# Create a y variable with 5 categories - Very Poor, Poor, Fair, Good, Excellent
# Represented as 0, 1, 2, 3, 4 respectfully
createYVariablePassDefense = function() {
  for(row in 1:nrow(jaxPassDefense)) {
    turnover = jaxPassDefense[row, "td_team"] == 1 | 
      jaxPassDefense[row, "interception"] == 1 |
      jaxPassDefense[row, "safety"] == 1
    if (turnover) {
      jaxPassDefense[row, "Quality"] = 4
    } else if (jaxPassDefense[row, "yards_gained"] >= 10) {
      jaxPassDefense[row, "Quality"] = passOver10Yards(row)
    } else {
      if (jaxPassDefense[row, "goal_to_go"] == "0") {
        jaxPassDefense[row, "Quality"] = passUnder10YardsNotGoalLine(row)
      } else {
        jaxPassDefense[row, "Quality"] = passUnder10YardsGoalLine(row)
      }
    }
  }
  return(jaxPassDefense)
}

jaxPassDefense = createYVariablePassDefense()

jaxPassDefense = fastDummies::dummy_cols(jaxPassDefense, remove_first_dummy = TRUE, select_columns=c("side_of_field",
                                         "game_half", "drive", "qtr", "down", "pass_length",
                                         "pass_location", "td_team"))

# Remove columns that the dummy variables represent
jaxPassDefense = subset(jaxPassDefense, select = -c(side_of_field, game_half, drive, qtr, down, pass_length, 
                                                    pass_location, td_team))

jaxPassDefense = jaxPassDefense %>% select(-Quality, Quality)
write.csv(jaxPassDefense, "jaxPassDefense.csv")
