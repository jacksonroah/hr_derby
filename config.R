# config.R - Central configuration file for the 2026 Home Run Derby app

CONFIG <- list(
  # API Settings
  api = list(
    use_mlb_api = FALSE,
    use_sample_data = FALSE,
    url = "https://zuriteapi.com/homers/api/homeruns/?format=json&year=2025",
    season = "2026",
    poll_interval_ms = 300000
  ),

  # Team Settings
  teams = list(
    team_info = list(
      "Derek" = list(
        display_name  = "Derek",
        abbr          = "DRK",
        squad_name    = "Team AI",
        primary_color = "#CFB87C",
        highlight_color = "#E6C085",
        text_color    = "black",
        pastel_color  = "rgba(207, 184, 124, 0.18)"
      ),
      "Jackson" = list(
        display_name  = "Jackson",
        abbr          = "JXN",
        squad_name    = "Dum Cha Cha",
        primary_color = "#c41919",
        highlight_color = "#c43c3c",
        text_color    = "#000000",
        pastel_color  = "rgba(0, 90, 156, 0.12)"
      ),
      "Tyler" = list(
        display_name  = "Tyler",
        abbr          = "TYL",
        squad_name    = "House Hunters",
        primary_color = "#005A9C",
        highlight_color = "#567be0",
        text_color    = "#fdfdfd",
        pastel_color  = "rgba(75, 46, 131, 0.12)"
      ),
      "Jason" = list(
        display_name  = "Jason",
        abbr          = "JSN",
        squad_name    = "Camel",
        primary_color = "#64010a",
        highlight_color = "#8B0000",
        text_color    = "white",
        pastel_color  = "rgba(115, 0, 10, 0.10)"
      ),
      "Jared" = list(
        display_name  = "Jared",
        abbr          = "JRD",
        squad_name    = "Diablo",
        primary_color = "#007030",
        highlight_color = "#154733",
        text_color    = "#ffe228",
        pastel_color  = "rgba(0, 112, 48, 0.12)"
      ),
      "Brusick" = list(
        display_name  = "Brusick",
        abbr          = "BRU",
        squad_name    = "Trophyless",
        primary_color = "#eebd1c",
        highlight_color = "#f5db66",
        text_color    = "black",
        pastel_color  = "rgba(255, 206, 48, 0.18)"
      ),
      "Maddox" = list(
        display_name  = "Maddox",
        abbr          = "MDX",
        squad_name    = "Offseason",
        primary_color = "#1a1a1a",
        highlight_color = "#f5182f",
        text_color    = "#f5182f",
        pastel_color  = "rgba(26, 26, 26, 0.08)"
      ),
      "Alex" = list(
        display_name  = "Alex",
        abbr          = "ALX",
        squad_name    = "Alleged Good Guy",
        primary_color = "#d429be",
        highlight_color = "#df8dd1",
        text_color    = "white",
        pastel_color  = "rgba(158, 79, 148, 0.12)"
      )
    )
  ),

  # Game Settings
  game = list(
    opening_day    = "2026-03-26",
    position_slots = c("OF", "OF", "OF", "1B", "2B", "3B", "SS", "C", "UTL"),
    # Standings color layout: "A" = color left / white right, "B" = white left / color right
    standings_variant = "A"
  ),

  # Position badge color map
  positions = list(
    "OF"  = list(text_color = "#3B82F6", bg_color = "#EFF6FF"),
    "1B"  = list(text_color = "#D97706", bg_color = "#FFFBEB"),
    "2B"  = list(text_color = "#7C3AED", bg_color = "#F5F3FF"),
    "3B"  = list(text_color = "#DC2626", bg_color = "#FEF2F2"),
    "SS"  = list(text_color = "#16A34A", bg_color = "#F0FDF4"),
    "C"   = list(text_color = "#DB2777", bg_color = "#FDF2F8"),
    "UTL" = list(text_color = "#6B7280", bg_color = "#F9FAFB")
  )
)

# ---------------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------------

get_team_names <- function() {
  names(CONFIG$teams$team_info)
}

get_team_colors <- function(for_graph = FALSE) {
  team_names <- get_team_names()
  if (for_graph) {
    colors <- sapply(team_names, function(t) CONFIG$teams$team_info[[t]]$highlight_color)
    return(colors)
  } else {
    colors <- sapply(team_names, function(t) CONFIG$teams$team_info[[t]]$primary_color)
    return(as.list(colors))
  }
}

get_team_display_names <- function() {
  team_names <- get_team_names()
  display_names <- sapply(team_names, function(t) {
    dn <- CONFIG$teams$team_info[[t]]$display_name
    if (is.null(dn)) t else dn
  })
  as.list(display_names)
}

get_team_squad_names <- function() {
  team_names <- get_team_names()
  squad_names <- sapply(team_names, function(t) {
    sn <- CONFIG$teams$team_info[[t]]$squad_name
    if (is.null(sn) || sn == "") t else sn
  })
  as.list(squad_names)
}

get_team_text_colors <- function() {
  team_names <- get_team_names()
  colors <- sapply(team_names, function(t) CONFIG$teams$team_info[[t]]$text_color)
  as.list(colors)
}

get_opening_day <- function() {
  as.Date(CONFIG$game$opening_day)
}

get_api_url <- function() {
  CONFIG$api$url
}

get_poll_interval <- function() {
  CONFIG$api$poll_interval_ms
}

get_position_slots <- function() {
  CONFIG$game$position_slots
}

get_position_colors <- function() {
  CONFIG$positions
}

# Calculate HR distances by team
calculate_hr_distances <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(NULL)
  if (!"hit_distance" %in% colnames(data)) {
    warning("hit_distance not available in data")
    return(NULL)
  }
  data$hit_distance <- as.numeric(data$hit_distance)
  team_distances <- data %>%
    dplyr::filter(!is.na(hit_distance)) %>%
    dplyr::group_by(team_name) %>%
    dplyr::summarise(
      total_distance = sum(hit_distance, na.rm = TRUE),
      distance_count = dplyr::n(),
      .groups = "drop"
    )
  return(team_distances)
}
