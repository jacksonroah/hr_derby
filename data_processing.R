# data_processing.R - Functions for processing home run data
library(dplyr)
library(tidyr)
library(readr)
library(httr)
library(jsonlite)

# Source configuration
source("config.R")

# Helper function to normalize player names (remove accents and special characters)
normalize_name <- function(name) {
  if (is.null(name) || is.na(name) || name == "") return("")
  
  # Convert to lowercase
  name <- tolower(name)
  
  # Replace common Spanish characters
  name <- gsub("á", "a", name)
  name <- gsub("é", "e", name)
  name <- gsub("í", "i", name)
  name <- gsub("ó", "o", name)
  name <- gsub("ú", "u", name)
  name <- gsub("ñ", "n", name)
  name <- gsub("ü", "u", name)
  
  # Replace common accented characters from other languages
  name <- gsub("à", "a", name)
  name <- gsub("â", "a", name)
  name <- gsub("ä", "a", name)
  name <- gsub("è", "e", name)
  name <- gsub("ê", "e", name)
  name <- gsub("ë", "e", name)
  name <- gsub("ì", "i", name)
  name <- gsub("î", "i", name)
  name <- gsub("ï", "i", name)
  name <- gsub("ò", "o", name)
  name <- gsub("ô", "o", name)
  name <- gsub("ö", "o", name)
  name <- gsub("ù", "u", name)
  name <- gsub("û", "u", name)
  name <- gsub("ç", "c", name)
  
  # Remove periods, hyphens, apostrophes
  name <- gsub("[\\.-]", "", name)
  name <- gsub("'", "", name)
  
  # Remove Jr, Sr suffixes
  name <- gsub("\\s+jr\\.?$", "", name)
  name <- gsub("\\s+sr\\.?$", "", name)
  
  # Remove "II", "III", "IV" suffixes
  name <- gsub("\\s+i{2,3}$", "", name)
  name <- gsub("\\s+iv$", "", name)
  
  # Remove spaces
  name <- gsub("\\s+", "", name)
  
  return(name)
}

# Load roster
load_roster <- function() {
  tryCatch({
    # Check if file exists
    if (!file.exists("roster.csv")) {
      warning("roster.csv file not found. Using default roster.")
      return(data.frame(
        player_name = character(),
        team_name = character(),
        stringsAsFactors = FALSE
      ))
    }
    
    # Load the roster CSV
    roster <- readr::read_csv("roster.csv", col_types = cols(
      player_name = col_character(),
      team_name = col_character()
    ))
    
    # Validate the roster data
    if (!all(c("player_name", "team_name") %in% colnames(roster))) {
      warning("Invalid roster.csv format. Must contain 'player_name' and 'team_name' columns.")
      return(data.frame(
        player_name = character(),
        team_name = character(),
        stringsAsFactors = FALSE
      ))
    }
    
    # Validate team names against configuration
    valid_teams <- get_team_names()
    invalid_teams <- roster$team_name[!roster$team_name %in% valid_teams]
    if (length(invalid_teams) > 0) {
      warning(paste("Invalid team names in roster:", paste(unique(invalid_teams), collapse = ", "),
                   "Valid teams are:", paste(valid_teams, collapse = ", ")))
    }
    
    return(roster)
  }, error = function(e) {
    warning(paste("Error loading roster:", e$message))
    return(data.frame(
      player_name = character(),
      team_name = character(),
      stringsAsFactors = FALSE
    ))
  })
}

# Global variable for the roster
drafted_players <- load_roster()

# Process raw data from API - Updated for your specific API format
# Process raw data from API - with improved name matching
process_data <- function(raw_data, roster) {
  if (is.null(raw_data) || nrow(raw_data) == 0) {
    warning("No data received from API")
    return(NULL)
  }
  
  # Check that we have the expected columns in the API response
  expected_columns <- c("batter_name", "date")
  missing_columns <- expected_columns[!expected_columns %in% colnames(raw_data)]
  
  if (length(missing_columns) > 0) {
    warning(paste("Missing expected columns in API response:", paste(missing_columns, collapse=", ")))
    return(NULL)
  }
  
  # Process the data for your specific API
  processed_data <- tryCatch({
    # Extract the player names and dates
    data <- data.frame(
      player_name = raw_data$batter_name,  # Use batter_name from your API
      date = as.Date(raw_data$date),       # Use date from your API
      stringsAsFactors = FALSE
    )
    
    # Add additional useful data if you want to use it later
    if ("hit_distance" %in% colnames(raw_data)) {
      data$hit_distance <- raw_data$hit_distance
    }
    if ("hit_speed" %in% colnames(raw_data)) {
      data$hit_speed <- raw_data$hit_speed
    }
    if ("venue" %in% colnames(raw_data)) {
      data$venue <- raw_data$venue
    }
    
    # Create normalized versions of player names for matching
    data$normalized_name <- sapply(data$player_name, normalize_name)
    roster$normalized_name <- sapply(roster$player_name, normalize_name)
    
    # Join with the roster using normalized names
    joined_data <- data %>%
      inner_join(roster, by = "normalized_name") %>%
      # Keep original player name from roster but use API data for everything else
      select(-player_name.x) %>%
      rename(player_name = player_name.y) %>%
      # Remove the normalized_name column since we don't need it anymore
      select(-normalized_name)
    
    return(joined_data)
  }, error = function(e) {
    warning(paste("Error processing data:", e$message))
    return(NULL)
  })
  
  return(processed_data)
}

# Calculate total home runs per player - including zero HR players
calculate_total_hr_per_player <- function(data) {
  # Get roster
  roster <- drafted_players
  
  if (is.null(data)) {
    # If no data is available, still show roster with zeros
    player_totals <- roster %>% 
      mutate(total_home_runs = 0) %>%
      arrange(team_name, desc(total_home_runs))
    
    return(player_totals)
  }
  
  # Calculate home runs per player for those with at least one HR
  players_with_hrs <- data %>%
    group_by(team_name, player_name) %>%
    summarise(total_home_runs = n(), .groups = "drop")
  
  # Create a base frame with all players from roster (including those with 0 HRs)
  all_players <- roster %>%
    select(team_name, player_name) %>%
    # Left join to include players with 0 HRs
    left_join(players_with_hrs, by = c("team_name", "player_name")) %>%
    # Replace NA values with 0 for players with no HRs
    mutate(total_home_runs = ifelse(is.na(total_home_runs), 0, total_home_runs)) %>%
    # Sort by team and HR count
    arrange(team_name, desc(total_home_runs))
  
  return(all_players)
}

# # LEADERBOARD FOR top five, team total, and total distance
# create_leaderboard <- function(player_totals, hr_data = NULL) {
#   if (is.null(player_totals) || nrow(player_totals) == 0) {
#     return(NULL)
#   }
#   
#   # Get number of counting players from config
#   counting_players <- get_counting_players()
#   
#   # Calculate team totals based on top N players
#   team_totals <- player_totals %>%
#     group_by(team_name) %>%
#     mutate(rank = row_number()) %>%
#     summarise(
#       top_n_total = sum(total_home_runs[rank <= counting_players]),
#       all_players_total = sum(total_home_runs),
#       .groups = "drop"
#     )
#   
#   # Add distance data if available
#   if (!is.null(hr_data) && "hit_distance" %in% colnames(hr_data)) {
#     # Make sure hit_distance is numeric
#     hr_data$hit_distance <- as.numeric(as.character(hr_data$hit_distance))
#     
#     # Calculate total distance by team
#     distance_data <- hr_data %>%
#       filter(!is.na(hit_distance)) %>%
#       group_by(team_name) %>%
#       summarise(
#         total_distance = sum(hit_distance, na.rm = TRUE),
#         .groups = "drop"
#       )
#     
#     # Join with team totals
#     team_totals <- left_join(team_totals, distance_data, by = "team_name") %>%
#       mutate(
#         total_distance = ifelse(is.na(total_distance), 0, total_distance)
#       )
#   }
#   
#   return(list(
#     leaderboard = team_totals,
#     counting_players = counting_players
#   ))
# }

# Create the leaderboard with top N players counting
create_leaderboard <- function(player_totals, hr_data = NULL) {
  if (is.null(player_totals) || nrow(player_totals) == 0) {
    return(NULL)
  }
  
  # Get number of counting players from config
  counting_players <- get_counting_players()
  
  # Calculate team totals based on top N players
  team_totals <- player_totals %>%
    group_by(team_name) %>%
    mutate(rank = row_number()) %>%
    summarise(
      top_n_total = sum(total_home_runs[rank <= counting_players]),
      all_players_total = sum(total_home_runs),
      .groups = "drop"
    )
  
  # Add distance data if available
  if (!is.null(hr_data) && "hit_distance" %in% colnames(hr_data)) {
    # Make sure hit_distance is numeric
    hr_data$hit_distance <- as.numeric(as.character(hr_data$hit_distance))
    
    # Calculate distance stats by team
    distance_data <- hr_data %>%
      filter(!is.na(hit_distance)) %>%
      group_by(team_name) %>%
      summarise(
        total_distance = sum(hit_distance, na.rm = TRUE),
        hr_count = n(),
        .groups = "drop"
      )
    
    # Join with team totals
    team_totals <- left_join(team_totals, distance_data, by = "team_name") %>%
      mutate(
        total_distance = ifelse(is.na(total_distance), 0, total_distance),
        hr_count = ifelse(is.na(hr_count), 0, hr_count)
      )
  }
  
  return(list(
    leaderboard = team_totals,
    counting_players = counting_players
  ))
}

# Prepare data for cumulative graph
prepare_cumulative_data <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }
  
  # Get number of counting players from config
  counting_players <- get_counting_players()
  opening_day <- get_opening_day()
  
  # Calculate cumulative home runs over time for top N players
  cumulative_data <- data %>%
    mutate(date = as.Date(date)) %>%
    # Filter for dates on or after opening day
    filter(date >= opening_day) %>%
    # Get total HRs by player to determine top N
    group_by(team_name, player_name) %>%
    mutate(player_total_hr = n()) %>%
    ungroup() %>%
    # Rank players within teams
    group_by(team_name) %>%
    mutate(player_rank = dense_rank(desc(player_total_hr))) %>%
    # Keep only top N players
    filter(player_rank <= counting_players) %>%
    # Get daily and cumulative counts
    group_by(team_name, date) %>%
    summarise(daily_hr = n(), .groups = "drop") %>%
    arrange(team_name, date) %>%
    group_by(team_name) %>%
    mutate(cumulative_hr = cumsum(daily_hr)) %>%
    ungroup()
  
  return(cumulative_data)
}

# Generate sample data for testing
generate_sample_hr_data <- function() {
  # Load roster
  roster <- load_roster()
  if (is.null(roster) || nrow(roster) == 0) {
    roster <- data.frame(
      player_name = c(
        "Aaron Judge", "Shohei Ohtani", "Juan Soto", "Pete Alonso",
        "Ronald Acuna Jr.", "Freddie Freeman", "Mike Trout", 
        "Giancarlo Stanton", "Jose Ramirez", "Fernando Tatis Jr."
      ),
      team_name = c(
        "Derek", "Derek", "Derek", "Derek",
        "Jackson", "Jackson", "Jackson",
        "Tyler", "Tyler", "Tyler"
      ),
      stringsAsFactors = FALSE
    )
  }
  
  # Generate sample data spanning March to present
  end_date <- Sys.Date()
  start_date <- as.Date("2025-03-27")  # Opening day
  date_range <- seq(start_date, end_date, by = "day")
  
  # Container for our sample data
  sample_data <- data.frame(
    player_name = character(),
    date = as.Date(character()),
    team_name = character(),
    stringsAsFactors = FALSE
  )
  
  # For each player, generate random home runs
  for (i in 1:nrow(roster)) {
    player <- roster$player_name[i]
    team <- roster$team_name[i]
    
    # Number of home runs depends on "perceived power" (just for sample data variety)
    power_hitters <- c("Aaron Judge", "Shohei Ohtani", "Pete Alonso", "Giancarlo Stanton", "Mike Trout")
    medium_hitters <- c("Juan Soto", "Ronald Acuna Jr.", "Fernando Tatis Jr.", "Kyle Tucker", "Matt Olson")
    
    if (player %in% power_hitters) {
      hr_count <- sample(15:25, 1)
    } else if (player %in% medium_hitters) {
      hr_count <- sample(10:18, 1)
    } else {
      hr_count <- sample(5:15, 1)
    }
    
    # Select random dates for the home runs
    hr_dates <- sample(date_range, hr_count, replace = TRUE)
    
    # Create entries for this player's home runs
    if (length(hr_dates) > 0) {
      player_data <- data.frame(
        player_name = player,
        date = hr_dates,
        team_name = team,
        stringsAsFactors = FALSE
      )
      
      sample_data <- rbind(sample_data, player_data)
    }
  }
  
  # Sort by date
  sample_data <- sample_data[order(sample_data$date), ]
  
  return(sample_data)
}