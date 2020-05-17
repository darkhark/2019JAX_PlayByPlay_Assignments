data2019 = read.csv("reg_pbp_2019.csv", stringsAsFactors = FALSE)

jaxData = subset(data2019, away_team == "JAX" | home_team == "JAX")

jaxRunOrPass = subset(jaxData, play_type == "run" | play_type == "pass")

# Is entire column NA?
jaxRunOrPass = jaxRunOrPass[, colSums(is.na(jaxRunOrPass)) != nrow(jaxRunOrPass)]

# After row 55 is all probablilities. Don't care about those
jaxRunOrPass = jaxRunOrPass[c(0:55)]

# Other columns that are not immediately useful for this data
jaxRunOrPass = subset(jaxRunOrPass, select = -c(desc, play_id, game_id, two_point_conv_result, timeout_team,
                                                quarter_end, sp, qb_spike, qb_kneel, timeout, posteam_timeouts_remaining,
                                                defteam_timeouts_remaining, posteam_score, defteam_score, posteam_score_post,
                                                defteam_score_post, score_differential_post, yrdln, game_date))

# Make defensive yards given up negative
alterYardageForDefense = function(df) {
  for(row in 1:nrow(df)) {
    if (df[row, "posteam"] != "JAX") {
      df[row, "yards_gained"] = -df[row, "yards_gained"]
      df[row, "air_yards"] = -df[row, "air_yards"]
      df[row, "yards_after_catch"] = -df[row, "yards_after_catch"]
    }
  }
  return(df)
}

# Change columns to always be in the perspective of JAX.
changeColumnsToRepresentJAX = function(df) {
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
  }
  df = subset(df, select = -c(total_away_score, total_home_score, defteam, posteam, posteam_type, home_timeouts_remaining,
                              away_timeouts_remaining))
  return(df)
}

jaxRunOrPass = alterYardageForDefense(jaxRunOrPass)
jaxRunOrPass = changeColumnsToRepresentJAX(jaxRunOrPass)

# Replace NA with blank for ease of reading
jaxRunOrPass[is.na(jaxRunOrPass)] = ""

write.csv(jaxRunOrPass, "jaxRunOrPassData.csv")

