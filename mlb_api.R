# mlb_api.R - Functions for fetching and processing MLB API data
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

# MLB API constants
MLB_API_BASE <- "https://statsapi.mlb.com/api/v1"

# Helper function to safely retrieve nested values from a list
safe_extract <- function(list_data, ..., default = NA) {
  result <- tryCatch({
    for(item in list(...)) {
      if(is.null(list_data[[item]])) return(default)
      list_data <- list_data[[item]]
    }
    return(list_data)
  }, error = function(e) {
    return(default)
  })
  return(result)
}

# Function to fetch home runs for the current season
fetch_home_runs <- function(season = format(Sys.Date(), "%Y"), use_sample = FALSE) {
  if (use_sample) {
    return(generate_sample_hr_data())
  }
  
  # Log the fetch attempt
  message(paste("Fetching home runs for season:", season))
  
  # URL for the stats API - game hitting stats
  stats_url <- paste0(
    MLB_API_BASE, 
    "/stats?stats=gameLog&group=hitting&gameType=R&season=", 
    season,
    "&sportId=1"
  )
  
  # Make the API request with timeout and retry
  response <- tryCatch({
    GET(stats_url, timeout(20))
  }, error = function(e) {
    message(paste("Error fetching data:", e$message))
    return(NULL)
  })
  
  # Check if request was successful
  if (is.null(response) || http_status(response)$category != "Success") {
    warning_message <- "Failed to fetch data from MLB API"
    if (!is.null(response)) {
      warning_message <- paste(warning_message, "Status code:", response$status_code)
    }
    warning(warning_message)
    return(NULL)
  }
  
  # Parse the JSON response
  hr_data <- tryCatch({
    content <- content(response, "text", encoding = "UTF-8")
    fromJSON(content)
  }, error = function(e) {
    warning(paste("Error parsing MLB API response:", e$message))
    return(NULL)
  })
  
  # Process the response to extract home run data
  processed_data <- process_mlb_api_response(hr_data)
  return(processed_data)
}

# Process MLB API response to extract home run data
process_mlb_api_response <- function(api_data) {
  if (is.null(api_data) || !("stats" %in% names(api_data))) {
    warning("Invalid or empty API data received")
    return(NULL)
  }
  
  # Attempt to extract the splits data which contains the hitting stats
  splits <- tryCatch({
    api_data$stats[[1]]$splits
  }, error = function(e) {
    warning(paste("Error extracting splits from API data:", e$message))
    return(NULL)
  })
  
  if (is.null(splits) || length(splits) == 0) {
    warning("No stats data found in API response")
    return(NULL)
  }
  
  # Extract home run data from each game
  home_runs <- lapply(1:length(splits), function(i) {
    split <- splits[[i]]
    
    # Extract player info
    player_id <- safe_extract(split, "player", "id")
    player_name <- safe_extract(split, "player", "fullName")
    
    # Extract game info
    game_id <- safe_extract(split, "game", "gamePk")
    game_date <- safe_extract(split, "date")
    
    # Extract stats - we only care about home runs
    hr <- safe_extract(split, "stat", "homeRuns", default = 0)
    
    # Only create records for games with home runs
    if (hr > 0) {
      # Create multiple rows if player hit multiple HRs in a game
      player_data <- data.frame(
        player_id = rep(player_id, hr),
        player_name = rep(player_name, hr),
        game_id = rep(game_id, hr),
        date = rep(as.Date(game_date), hr),
        stringsAsFactors = FALSE
      )
      return(player_data)
    } else {
      return(NULL)
    }
  })
  
  # Combine all home run records and remove NULL entries
  home_runs <- do.call(rbind, home_runs[!sapply(home_runs, is.null)])
  
  # If no home runs were found, return empty data frame
  if (is.null(home_runs) || nrow(home_runs) == 0) {
    warning("No home run data found in API response")
    return(data.frame(
      player_id = character(),
      player_name = character(),
      game_id = character(),
      date = as.Date(character()),
      stringsAsFactors = FALSE
    ))
  }
  
  return(home_runs)
}

# Function to join home run data with roster data
match_home_runs_to_roster <- function(home_runs, roster) {
  if (is.null(home_runs) || nrow(home_runs) == 0) {
    warning("No home run data available to match with roster")
    return(NULL)
  }
  
  if (is.null(roster) || nrow(roster) == 0) {
    warning("No roster data available")
    return(NULL)
  }
  
  # Check if roster has the required columns
  if (!all(c("player_name", "team_name") %in% colnames(roster))) {
    warning("Roster data missing required columns 'player_name' and/or 'team_name'")
    print("Roster columns:", colnames(roster))
    return(NULL)
  }
  
  # Standardize player names for matching
  home_runs$player_name_std <- tolower(gsub("[^a-zA-Z]", "", home_runs$player_name))
  roster$player_name_std <- tolower(gsub("[^a-zA-Z]", "", roster$player_name))
  
  # Debug output
  print(paste("Home runs dataframe columns:", paste(colnames(home_runs), collapse=", ")))
  print(paste("Roster dataframe columns:", paste(colnames(roster), collapse=", ")))
  
  # Join home run data with roster by standardized player name
  matched_data <- tryCatch({
    joined_data <- inner_join(
      home_runs, 
      roster, 
      by = "player_name_std"
    )
    
    # Debug the joined data
    print(paste("Joined dataframe columns:", paste(colnames(joined_data), collapse=", ")))
    
    # Now select the columns we need
    result <- joined_data %>%
      select(
        player_name = player_name.x,  # Keep the original MLB name
        date,
        team_name
      )
    
    return(result)
  }, error = function(e) {
    warning(paste("Error joining data:", e$message))
    print(e)
    # Create a fallback version with more explicit column selection
    if ("player_name.x" %in% colnames(joined_data) && 
        "date" %in% colnames(joined_data) && 
        "team_name" %in% colnames(joined_data)) {
      return(data.frame(
        player_name = joined_data$player_name.x,
        date = joined_data$date,
        team_name = joined_data$team_name,
        stringsAsFactors = FALSE
      ))
    } else {
      return(NULL)
    }
  })
  
  # If we got a result, report match success
  if (!is.null(matched_data) && nrow(matched_data) > 0) {
    match_percentage <- nrow(matched_data) / nrow(home_runs) * 100
    message(sprintf(
      "Matched %d of %d home runs (%.1f%%) to roster players",
      nrow(matched_data), nrow(home_runs), match_percentage
    ))
  } else {
    warning("No matches found between home runs and roster players")
  }
  
  return(matched_data)
}
# Generate sample data for testing when API is unavailable
generate_sample_hr_data <- function() {
  # Ensure we have the roster
  roster <- tryCatch({
    if (file.exists("roster.csv")) {
      message("Reading roster.csv file")
      read.csv("roster.csv", stringsAsFactors = FALSE)
    } else {
      # Create a basic roster if file doesn't exist
      message("roster.csv not found, using built-in sample roster")
      data.frame(
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
  }, error = function(e) {
    message(paste("Error loading roster:", e$message))
    # Fallback roster
    data.frame(
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
  })
  
  # Print roster for debugging
  message(paste("Loaded roster with", nrow(roster), "players"))
  print(head(roster))
  
  # Generate sample data spanning the past 3 months
  end_date <- Sys.Date()
  start_date <- end_date - lubridate::days(90)
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
    power_hitters <- c("Aaron Judge", "Shohei Ohtani", "Pete Alonso", "Giancarlo Stanton")
    medium_hitters <- c("Juan Soto", "Ronald Acuna Jr.", "Fernando Tatis Jr.")
    
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
  
  # Print summary of generated data
  message(paste("Generated", nrow(sample_data), "sample home run records"))
  
  return(sample_data)
}