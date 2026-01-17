library(baseballr)
library(tidyverse)

# 1. Define the 2024 Regular Season Schedule (Week by Week)
# We break it into 7-day chunks to prevent timeouts
start_date <- as.Date("2024-03-28") # Opening Day
end_date   <- as.Date("2024-09-29") # End of Regular Season

# Create a list of weeks to loop through
weeks <- seq(start_date, end_date, by = "week")

# Initialize an empty list to store the data
all_data <- list()

print("--- STARTING FULL SEASON DOWNLOAD (This may take 15-20 mins) ---")

# 2. The Loop: Go through every week and download data
for (i in 1:(length(weeks) - 1)) {
  
  week_start <- weeks[i]
  week_end   <- weeks[i+1] - 1 # Subtract 1 day so we don't double count
  
  print(paste("Downloading Week:", week_start, "to", week_end, "..."))
  
  tryCatch({
    # Scrape that specific week
    weekly_data <- scrape_statcast_savant(
      start_date = week_start, 
      end_date = week_end, 
      player_type = "batter"
    )
    
    # Add it to our list
    all_data[[i]] <- weekly_data
    
    # Sleep for 2 seconds to be polite to the MLB server (prevents banning)
    Sys.sleep(2)
    
  }, error = function(e) {
    print(paste("Error downloading week starting", week_start, ":", e$message))
  })
}

print("--- DOWNLOAD COMPLETE. PROCESSING DATA... ---")

# 3. Combine all weeks into one big table
full_season_raw <- bind_rows(all_data)

# 4. Clean and Process (Apply the "Final Outcome" logic)
clean_data <- full_season_raw |> 
  # Group by specific Game and At-Bat to link First Pitch -> Final Outcome
  group_by(game_pk, at_bat_number) |> 
  
  # Grab the outcome from the LAST pitch of the at-bat
  mutate(final_event = last(events, order_by = pitch_number)) |> 
  
  ungroup() |> 
  filter(pitch_number == 1) |> # Keep only the first pitch
  
  # Create the Analysis Columns
  mutate(
    first_pitch_result = case_when(
      type == "S" ~ "Strike",
      type == "X" ~ "Strike",
      type == "B" ~ "Ball",
      TRUE ~ "Other"
    ),
    
    # Use 'final_event' for the outcomes
    is_hit = final_event %in% c("single", "double", "triple", "home_run"),
    is_walk = final_event == "walk",
    is_homerun = final_event == "home_run",
    is_strikeout = final_event %in% c("strikeout", "strikeout_double_play")
  ) |> 
  select(
    game_date, player_name, 
    first_pitch_result, final_event, 
    is_hit, is_walk, is_homerun, is_strikeout,
    release_speed
  )

# 5. Save the massive file
write_csv(clean_data, "fps_analysis_2024.csv")

print("SUCCESS! Full 2024 Season data saved to 'fps_analysis_2024.csv'.")
