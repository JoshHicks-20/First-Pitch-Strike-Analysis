library(baseballr)
library(tidyverse)

print("Scraping data... (This scrapes 1 week of June 2024)")
raw_data <- scrape_statcast_savant(
  start_date = "2024-06-01", 
  end_date = "2024-06-07", 
  player_type = "batter"
)

print("Fixing the 'Missing Events' bug...")

# --- THE FIX IS HERE ---
clean_data <- raw_data |> 
  # 1. Group by specific Game and At-Bat so we keep pitches together
  group_by(game_pk, at_bat_number) |> 
  
  # 2. Create a new column that grabs the outcome from the LAST pitch
  mutate(final_event = last(events, order_by = pitch_number)) |> 
  
  # 3. Now we can ungroup and filter
  ungroup() |> 
  filter(pitch_number == 1) |> 
  
  # 4. Create your analysis flags using the NEW 'final_event' column
  mutate(
    first_pitch_result = case_when(
      type == "S" ~ "Strike",
      type == "X" ~ "Strike",
      type == "B" ~ "Ball",
      TRUE ~ "Other"
    ),
    
    # Use 'final_event' instead of 'events'
    is_hit = final_event %in% c("single", "double", "triple", "home_run"),
    is_walk = final_event == "walk",
    is_homerun = final_event == "home_run",
    is_strikeout = final_event %in% c("strikeout", "strikeout_double_play")
  ) |> 
  select(
    game_date, player_name, 
    first_pitch_result, final_event, 
    is_hit, is_walk, is_homerun, is_strikeout
  )

# Save the corrected file
write_csv(clean_data, "fps_analysis_2024.csv")

print("Fixed! 'fps_analysis_2024.csv' now includes outcomes for First Pitch Balls.")