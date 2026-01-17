# 1. Load your massive full-season file
print("Loading data...")
data <- read_csv("fps_analysis_2024.csv")

# 2. Add the 'is_out' column
# Logic: If it's not a Hit, not a Walk, and not a Hit-By-Pitch, it's an Out.
data_with_outs <- data |> 
  mutate(
    is_out = !is_hit & !is_walk & final_event != "hit_by_pitch"
  )

# 3. Save it back to the file
write_csv(data_with_outs, "fps_analysis_2024.csv")

print("SUCCESS! Your data now has an 'is_out' column.")
