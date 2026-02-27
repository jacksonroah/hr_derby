# config.R - Central configuration file for the Home Run Derby app

# API Configuration
CONFIG <- list(
  # API Settings
  api = list(
    use_mlb_api = FALSE,          # Set to FALSE to use your custom API
    use_sample_data = FALSE,      # Set to FALSE to use real data
    url = "https://zuriteapi.com/homers/api/homeruns/?format=json&year=2025",
    season = "2025",              # Current season
    poll_interval_ms = 300000      # Poll every minute
  ),
  
  # Team Settings
  teams = list(
    # Team names and colors
    team_info = list(
      "Derek" = list(
        display_name = "Derek",
        squad_name = '',
        primary_color = "#CFB87C",    # University of Colorado Boulder Gold
        highlight_color = "#E6C085",  # Brighter version for graphs
        text_color = "black"
      ),
      "Matt" = list(
        display_name = "Matt",
        squad_name = '',
        primary_color = "#005A9C",    # Dodger Blue
        highlight_color = "#005A9C",  # Brighter version for graphs
        text_color = "white"        # Light color for contrast
      ),
      "Tyler" = list(
        display_name = "Tyler",
        squad_name = '',
        primary_color = "#4b2e83",    # University of Washington Purple
        highlight_color = "#5A3E9B",  # Brighter version for graphs
        text_color = "#ffc700"        # Light color for contrast
      ),
      "Jason" = list(
        display_name = "Jason",
        squad_name = '',
        primary_color = "#73000A",    # University of South Carolina Red
        highlight_color = "#8B0000",  # Brighter version for graphs
        text_color = "white"
      ),
      "Jared" = list(
        display_name = "Jared",
        squad_name = '',
        primary_color = "#007030", 
        highlight_color = "#154733",
        text_color = "#FEE123"
      ),
      "Brusick" = list(
        display_name = "Brusick",
        squad_name = '',
        primary_color = "#ffce30", 
        highlight_color = "#ffce30",
        text_color = "black"
      ),
      "Maddox" = list(
        display_name = "Maddox",
        squad_name = '',
        primary_color = "black",
        highlight_color = "#f5182f",
        text_color = "#f5182f"
      )
    ) 
  ),
  
  # Game Settings
  game = list(
    opening_day = "2025-03-27",    # Opening Day of the 2025 season
    counting_players = 5,          # Number of top players whose HRs count toward team total
    bottom_display_count = 2        # Number of bottom players to highlight differently
  )
)

# Helper functions to access configuration
get_team_names <- function() {
  return(names(CONFIG$teams$team_info))
}

get_team_colors <- function(for_graph = FALSE) {
  team_names <- get_team_names()
  if (for_graph) {
    # For graphs, return a named vector of highlight colors
    colors <- sapply(team_names, function(team) CONFIG$teams$team_info[[team]]$highlight_color)
    return(colors)
  } else {
    # For tables, return a list of primary colors
    colors <- sapply(team_names, function(team) CONFIG$teams$team_info[[team]]$primary_color)
    return(as.list(colors))
  }
}

# NEW: Get display names for the main leaderboard
get_team_display_names <- function() {
  team_names <- get_team_names()
  display_names <- sapply(team_names, function(team) {
    display_name <- CONFIG$teams$team_info[[team]]$display_name
    if (is.null(display_name)) {
      return(team)  # Fallback to original name
    }
    return(display_name)
  })
  return(as.list(display_names))
}

# NEW: Get squad names for individual standings
get_team_squad_names <- function() {
  team_names <- get_team_names()
  squad_names <- sapply(team_names, function(team) {
    squad_name <- CONFIG$teams$team_info[[team]]$squad_name
    if (is.null(squad_name)) {
      return(team)  # Fallback to original name
    }
    return(squad_name)
  })
  return(as.list(squad_names))
}

# Calculate HR distances by team
calculate_hr_distances <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }
  
  # Check if hit_distance is available in the data
  if (!"hit_distance" %in% colnames(data)) {
    warning("hit_distance not available in data")
    return(NULL)
  }
  
  # Convert hit_distance to numeric, handling NA values
  data$hit_distance <- as.numeric(data$hit_distance)
  
  # Calculate total distance and count of HRs with distance data by team
  team_distances <- data %>%
    filter(!is.na(hit_distance)) %>%
    group_by(team_name) %>%
    summarise(
      total_distance = sum(hit_distance, na.rm = TRUE),
      distance_count = n(),
      .groups = "drop"
    )
  
  return(team_distances)
}

get_team_text_colors <- function() {
  team_names <- get_team_names()
  colors <- sapply(team_names, function(team) CONFIG$teams$team_info[[team]]$text_color)
  return(as.list(colors))
}

get_counting_players <- function() {
  return(CONFIG$game$counting_players)
}

get_bottom_display_count <- function() {
  return(CONFIG$game$bottom_display_count)
}

get_opening_day <- function() {
  return(as.Date(CONFIG$game$opening_day))
}

get_api_url <- function() {
  return(CONFIG$api$url)
}

get_poll_interval <- function() {
  return(CONFIG$api$poll_interval_ms)
}