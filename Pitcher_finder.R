library(tidyverse)
library(baseballr)

# 1. Check if the raw data is still in memory
if (exists("full_season_raw")) {
  print("Found raw data! creating Pitcher Analysis file...")
  
  # Load a map of Player IDs to Names so we can name the Pitchers
  print("Loading Player Name Database...")
  player_map <- baseballr::chadwick_player_lu() |> 
    select(key_mlbam, name_last, name_first) |> 
    mutate(full_name = paste(name_last, name_first, sep = ", "))
  
  # Process the data specifically for PITCHERS this time
  pitcher_data <- full_season_raw |> 
    group_by(game_pk, at_bat_number) |> 
    mutate(final_event = last(events, order_by = pitch_number)) |> 
    ungroup() |> 
    filter(pitch_number == 1) |> 
    
    # Join with the name map to convert 'pitcher' ID to a Name
    left_join(player_map, by = c("pitcher" = "key_mlbam")) |> 
    rename(pitcher_name = full_name) |> 
    
    mutate(
      first_pitch_result = case_when(
        type == "S" ~ "Strike",
        type == "X" ~ "Strike",
        type == "B" ~ "Ball",
        TRUE ~ "Other"
      ),
      is_hit = final_event %in% c("single", "double", "triple", "home_run"),
      is_walk = final_event == "walk",
      is_homerun = final_event == "home_run",
      is_strikeout = final_event %in% c("strikeout", "strikeout_double_play"),
      is_out = !is_hit & !is_walk & final_event != "hit_by_pitch"
    ) |> 
    
    # SELECT correct columns (Including game_pk this time!)
    select(
      game_pk, game_date, 
      player_name = pitcher_name, # We rename this so your other scripts work
      first_pitch_result, final_event, 
      is_hit, is_walk, is_homerun, is_strikeout, is_out
    )
  
  write_csv(pitcher_data, "fps_analysis_2024.csv")
  print("SUCCESS! File updated with PITCHER names. You can now run the graph script.")

} else {
  print("❌ Raw data not found. You must re-download the season.")
  print("Please copy and run the 'Re-Download' script below.")
}
