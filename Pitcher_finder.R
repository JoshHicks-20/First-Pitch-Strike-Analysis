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

library(tidyverse)

# 1. Load your data
data <- read_csv("fps_analysis_2024.csv")

print("Data loaded. Processing...")

# 2. Identify Pitchers & Calculate FPS%
pitcher_stats <- data |>
  group_by(player_name) |>
  summarize(
    # FIX: Since the file only has 1 row per at-bat, we can just count rows!
    total_batters = n(), 
    
    # FPS Stats
    fps_count = sum(first_pitch_result == "Strike"),
    fps_pct = fps_count / total_batters,
    
    # --- OUTCOME COUNTS ---
    walks = sum(is_walk, na.rm = TRUE),
    homeruns = sum(is_homerun, na.rm = TRUE),
    base_hits = sum(is_hit, na.rm = TRUE) - homeruns,
    strikeouts = sum(is_strikeout, na.rm = TRUE),
    field_outs = sum(is_out, na.rm = TRUE) - strikeouts
  ) |>
  
  # Filter for Qualified Starters (400+ Batters Faced)
  filter(total_batters >= 400) |> 
  arrange(desc(fps_pct))

# 3. Pick the 3 Specific Pitchers
best_pitcher   <- pitcher_stats |> slice(1)
worst_pitcher  <- pitcher_stats |> slice(n())
median_pitcher <- pitcher_stats |> slice(ceiling(n() / 2))

# Combine them
target_pitchers <- bind_rows(best_pitcher, median_pitcher, worst_pitcher) |> 
  mutate(
    Category = case_when(
      player_name == best_pitcher$player_name ~ "Highest FPS%",
      player_name == median_pitcher$player_name ~ "Average FPS%",
      player_name == worst_pitcher$player_name ~ "Lowest FPS%"
    )
  )

print(target_pitchers[, c("player_name", "Category", "fps_pct")])

# 4. Reshape Data for the Pie Chart
comparison_data <- target_pitchers |> 
  pivot_longer(
    cols = c(homeruns, base_hits, walks, strikeouts, field_outs), 
    names_to = "Outcome_Type", 
    values_to = "Count"
  ) |> 
  group_by(player_name) |> 
  mutate(
    Percentage = Count / sum(Count),
    Label = scales::percent(Percentage, accuracy = 1)
  ) |> 
  select(player_name, Category, fps_pct, Outcome_Type, Count, Percentage, Label)

# 5. Save the file
write_csv(comparison_data, "pitcher_comparison.csv")

print("SUCCESS! 'pitcher_comparison.csv' has been created.")
