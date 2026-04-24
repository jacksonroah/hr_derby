# data_processing.R - Functions for processing home run data
library(dplyr)
library(tidyr)
library(readr)
library(httr)
library(jsonlite)

source("config.R")

# ---------------------------------------------------------------------------
# "Today" in Pacific Time with a 4am rollover cutoff.
# Shinyapps.io runs in UTC, so Sys.Date() resets at 4–5pm PST.
# This returns the calendar date a PST viewer would consider "today",
# rolling over at 4am PST (not midnight) so late-night stats persist.
# ---------------------------------------------------------------------------
get_today_pst <- function() {
  now_pst <- as.POSIXct(Sys.time(), tz = "America/Los_Angeles")
  d <- as.Date(now_pst, tz = "America/Los_Angeles")
  hour_pst <- as.integer(format(now_pst, "%H"))
  if (hour_pst < 4) d - 1L else d
}

# ---------------------------------------------------------------------------
# Name normalization
# ---------------------------------------------------------------------------
normalize_name <- function(name) {
  if (is.null(name) || is.na(name) || name == "") return("")
  name <- tolower(name)
  name <- gsub("\u00e1", "a", name); name <- gsub("\u00e9", "e", name)
  name <- gsub("\u00ed", "i", name); name <- gsub("\u00f3", "o", name)
  name <- gsub("\u00fa", "u", name); name <- gsub("\u00f1", "n", name)
  name <- gsub("\u00fc", "u", name); name <- gsub("\u00e0", "a", name)
  name <- gsub("\u00e2", "a", name); name <- gsub("\u00e4", "a", name)
  name <- gsub("\u00e8", "e", name); name <- gsub("\u00ea", "e", name)
  name <- gsub("\u00eb", "e", name); name <- gsub("\u00ec", "i", name)
  name <- gsub("\u00ee", "i", name); name <- gsub("\u00ef", "i", name)
  name <- gsub("\u00f2", "o", name); name <- gsub("\u00f4", "o", name)
  name <- gsub("\u00f6", "o", name); name <- gsub("\u00f9", "u", name)
  name <- gsub("\u00fb", "u", name); name <- gsub("\u00e7", "c", name)
  name <- gsub("[\\.-]", "", name)
  name <- gsub("'", "", name)
  name <- gsub("\\s+jr\\.?$", "", name)
  name <- gsub("\\s+sr\\.?$", "", name)
  name <- gsub("\\s+i{2,3}$", "", name)
  name <- gsub("\\s+iv$", "", name)
  name <- gsub("\\s+", "", name)
  return(name)
}

# ---------------------------------------------------------------------------
# Load roster (new 2026 schema: player_name, team_name, position)
# ---------------------------------------------------------------------------
load_roster <- function() {
  valid_positions <- c("OF", "1B", "2B", "3B", "SS", "C", "UTL", "BENCH")

  tryCatch({
    roster_file <- if (file.exists("roster_2026.csv")) "roster_2026.csv" else "roster.csv"

    if (!file.exists(roster_file)) {
      warning(paste(roster_file, "not found. Returning empty roster."))
      return(data.frame(player_name = character(), team_name = character(),
                        position = character(), stringsAsFactors = FALSE))
    }

    roster <- readr::read_csv(roster_file, col_types = cols(
      player_name = col_character(),
      team_name   = col_character(),
      position    = col_character(),
      mlb_team    = col_character()
    )) %>% dplyr::filter(!is.na(player_name))

    required_cols <- c("player_name", "team_name", "position")
    if (!all(required_cols %in% colnames(roster))) {
      if (all(c("player_name", "team_name") %in% colnames(roster))) {
        warning("roster CSV missing position column; defaulting to UTL")
        roster$position <- "UTL"
      } else {
        warning("Invalid roster format.")
        return(data.frame(player_name = character(), team_name = character(),
                          position = character(), stringsAsFactors = FALSE))
      }
    }

    # Validate teams
    valid_teams <- get_team_names()
    bad_teams <- roster$team_name[!roster$team_name %in% valid_teams]
    if (length(bad_teams) > 0) {
      warning(paste("Unknown team names in roster:", paste(unique(bad_teams), collapse = ", ")))
    }

    # Validate positions
    bad_pos <- roster$position[!roster$position %in% valid_positions]
    if (length(bad_pos) > 0) {
      warning(paste("Unknown positions in roster:", paste(unique(bad_pos), collapse = ", ")))
    }

    return(roster)
  }, error = function(e) {
    warning(paste("Error loading roster:", e$message))
    return(data.frame(player_name = character(), team_name = character(),
                      position = character(), stringsAsFactors = FALSE))
  })
}

# Global roster variable
drafted_players <- load_roster()

# ---------------------------------------------------------------------------
# Load roster transaction log (player swaps).
# File: transactions_2026.csv — columns: date, team_name, player_out, player_in
# Returns an empty data frame if the file is missing or has no rows.
# ---------------------------------------------------------------------------
load_transactions <- function() {
  tx_file <- "transactions_2026.csv"
  empty <- data.frame(
    date       = as.Date(character()),
    team_name  = character(),
    player_out = character(),
    player_in  = character(),
    stringsAsFactors = FALSE
  )
  if (!file.exists(tx_file)) return(empty)
  tryCatch({
    tx <- readr::read_csv(tx_file, col_types = readr::cols(
      date       = readr::col_date(),
      team_name  = readr::col_character(),
      player_out = readr::col_character(),
      player_in  = readr::col_character()
    ))
    tx <- tx[!is.na(tx$date) & !is.na(tx$team_name) &
               !is.na(tx$player_out) & !is.na(tx$player_in), ]
    if (nrow(tx) == 0) return(empty)
    tx[order(tx$date), ]
  }, error = function(e) {
    warning(paste("Error loading transactions:", e$message))
    empty
  })
}

# ---------------------------------------------------------------------------
# Build player active-date ranges from roster + transactions.
#
# Returns a data frame with columns:
#   player_name, team_name, start_date, end_date, is_historical
#
# is_historical = TRUE  -> player was dropped; HRs counted up to end_date
# is_historical = FALSE -> player is in current roster; start_date may be >
#                          opening_day if they were swapped in mid-season
# ---------------------------------------------------------------------------
build_player_date_ranges <- function(roster, transactions) {
  opening_day <- get_opening_day()
  far_future  <- as.Date("2099-12-31")

  # Non-bench active players start from opening_day (may be overridden below)
  ranges <- roster %>%
    dplyr::filter(position != "BENCH") %>%
    dplyr::select(player_name, team_name) %>%
    dplyr::mutate(
      start_date   = as.Date(opening_day),
      end_date     = far_future,
      is_historical = FALSE
    )

  if (is.null(transactions) || nrow(transactions) == 0) {
    return(ranges)
  }

  for (i in seq_len(nrow(transactions))) {
    tx      <- transactions[i, ]
    tx_date <- as.Date(tx$date)

    # --- Swapped-IN player: their counting window starts on the swap date ---
    in_idx <- which(ranges$player_name == tx$player_in &
                      ranges$team_name  == tx$team_name)
    if (length(in_idx) > 0) {
      ranges$start_date[in_idx] <- tx_date
    }

    # --- Swapped-OUT player: cap their window at swap_date - 1 ---
    out_idx <- which(ranges$player_name == tx$player_out &
                       ranges$team_name  == tx$team_name)
    if (length(out_idx) > 0) {
      # Already in ranges (was swapped in previously, now being swapped out)
      ranges$end_date[out_idx]     <- tx_date - 1
      ranges$is_historical[out_idx] <- TRUE
    } else {
      # Was an original roster player; add a historical row
      ranges <- dplyr::bind_rows(ranges, data.frame(
        player_name   = tx$player_out,
        team_name     = tx$team_name,
        start_date    = as.Date(opening_day),
        end_date      = tx_date - 1,
        is_historical = TRUE,
        stringsAsFactors = FALSE
      ))
    }
  }

  return(ranges)
}

# Global transaction log and date ranges (rebuilt each time the app starts)
roster_transactions  <- load_transactions()
player_date_ranges   <- build_player_date_ranges(drafted_players, roster_transactions)

# ---------------------------------------------------------------------------
# Load golden swap log.
# File: golden_swaps_2026.csv — columns: team_name, player_out, player_in, swap_date
# Each team gets one golden swap per season. Returns empty df if unused.
# ---------------------------------------------------------------------------
load_golden_swaps <- function() {
  gs_file <- "golden_swaps_2026.csv"
  empty <- data.frame(
    team_name  = character(),
    player_out = character(),
    player_in  = character(),
    swap_date  = as.Date(character()),
    stringsAsFactors = FALSE
  )
  if (!file.exists(gs_file)) return(empty)
  tryCatch({
    gs <- readr::read_csv(gs_file, col_types = readr::cols(
      team_name  = readr::col_character(),
      player_out = readr::col_character(),
      player_in  = readr::col_character(),
      swap_date  = readr::col_date()
    ))
    gs <- gs[!is.na(gs$team_name) & !is.na(gs$player_in), ]
    if (nrow(gs) == 0) return(empty)
    gs
  }, error = function(e) {
    warning(paste("Error loading golden swaps:", e$message))
    empty
  })
}

# Global golden swap record
golden_swaps <- load_golden_swaps()

# ---------------------------------------------------------------------------
# Process raw API data
# ---------------------------------------------------------------------------
process_data <- function(raw_data, roster) {
  if (is.null(raw_data) || nrow(raw_data) == 0) {
    warning("No data received from API")
    return(NULL)
  }

  if ("batter" %in% colnames(raw_data)) {
    raw_data <- raw_data %>%
      filter(!(batter_name == "Max Muncy" & batter == "691777"))
  }

  expected_columns <- c("batter_name", "date")
  missing_columns <- expected_columns[!expected_columns %in% colnames(raw_data)]
  if (length(missing_columns) > 0) {
    warning(paste("Missing columns in API response:", paste(missing_columns, collapse = ", ")))
    return(NULL)
  }

  processed_data <- tryCatch({
    data <- data.frame(
      player_name = raw_data$batter_name,
      date        = as.Date(raw_data$date),
      stringsAsFactors = FALSE
    )

    if ("hit_distance" %in% colnames(raw_data)) data$hit_distance <- raw_data$hit_distance
    if ("hit_speed"    %in% colnames(raw_data)) data$hit_speed    <- raw_data$hit_speed
    if ("venue"        %in% colnames(raw_data)) data$venue        <- raw_data$venue

    data$normalized_name   <- sapply(data$player_name, normalize_name)
    roster$normalized_name <- sapply(roster$player_name, normalize_name)

    joined_data <- data %>%
      inner_join(roster, by = "normalized_name") %>%
      select(-player_name.x) %>%
      rename(player_name = player_name.y) %>%
      select(-normalized_name) %>%
      filter(position != "BENCH")

    return(joined_data)
  }, error = function(e) {
    warning(paste("Error processing data:", e$message))
    return(NULL)
  })

  return(processed_data)
}

# ---------------------------------------------------------------------------
# Calculate total home runs per player (all players, includes position)
# ---------------------------------------------------------------------------
calculate_total_hr_per_player <- function(data) {
  roster <- drafted_players

  if (is.null(data)) {
    return(roster %>% mutate(total_home_runs = 0, avg_distance = NA_real_) %>% arrange(team_name, desc(total_home_runs)))
  }

  players_with_hrs <- data %>%
    group_by(team_name, player_name) %>%
    summarise(total_home_runs = n(), .groups = "drop")

  if ("hit_distance" %in% colnames(data)) {
    data$hit_distance <- as.numeric(as.character(data$hit_distance))
    player_distances <- data %>%
      filter(!is.na(hit_distance)) %>%
      group_by(team_name, player_name) %>%
      summarise(avg_distance = round(mean(hit_distance, na.rm = TRUE)), .groups = "drop")
    players_with_hrs <- left_join(players_with_hrs, player_distances, by = c("team_name", "player_name"))
  } else {
    players_with_hrs$avg_distance <- NA_real_
  }

  keep_cols <- intersect(c("team_name", "player_name", "position", "mlb_team"), names(roster))
  all_players <- roster %>%
    select(all_of(keep_cols)) %>%
    left_join(players_with_hrs, by = c("team_name", "player_name")) %>%
    mutate(total_home_runs = ifelse(is.na(total_home_runs), 0, total_home_runs)) %>%
    arrange(team_name, desc(total_home_runs))

  return(all_players)
}

# ---------------------------------------------------------------------------
# Create leaderboard — ALL players count (no top-N filtering)
# ---------------------------------------------------------------------------
create_leaderboard <- function(player_totals, hr_data = NULL) {
  if (is.null(player_totals) || nrow(player_totals) == 0) return(NULL)

  team_totals <- player_totals %>%
    group_by(team_name) %>%
    summarise(
      team_total = sum(total_home_runs),
      .groups = "drop"
    )

  if (!is.null(hr_data) && "hit_distance" %in% colnames(hr_data)) {
    hr_data$hit_distance <- as.numeric(as.character(hr_data$hit_distance))
    distance_data <- hr_data %>%
      filter(!is.na(hit_distance)) %>%
      group_by(team_name) %>%
      summarise(
        total_distance = sum(hit_distance, na.rm = TRUE),
        hr_count       = n(),
        .groups = "drop"
      )
    team_totals <- left_join(team_totals, distance_data, by = "team_name") %>%
      mutate(
        total_distance = ifelse(is.na(total_distance), 0, total_distance),
        hr_count       = ifelse(is.na(hr_count), 0, hr_count)
      )
  }

  return(list(leaderboard = team_totals))
}

# ---------------------------------------------------------------------------
# Recent HR stats by team — today / 7-day / 30-day
# ---------------------------------------------------------------------------
calculate_recent_hr_stats <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(NULL)

  today      <- get_today_pst()
  week_ago   <- today - 7
  month_ago  <- today - 30

  today_hrs <- data %>%
    filter(date >= today) %>%
    group_by(team_name) %>%
    summarise(today_hr = n(), .groups = "drop")

  past7_hrs <- data %>%
    filter(date >= week_ago) %>%
    group_by(team_name) %>%
    summarise(past7_hr = n(), .groups = "drop")

  past30_hrs <- data %>%
    filter(date >= month_ago) %>%
    group_by(team_name) %>%
    summarise(past30_hr = n(), .groups = "drop")

  # Distance stats
  longest_hrs  <- NULL
  avg_distance <- NULL
  if ("hit_distance" %in% colnames(data)) {
    data$hit_distance <- as.numeric(as.character(data$hit_distance))

    longest_by_team <- data %>%
      filter(!is.na(hit_distance)) %>%
      group_by(team_name) %>%
      mutate(max_dist = max(hit_distance, na.rm = TRUE)) %>%
      filter(hit_distance == max_dist) %>%
      arrange(desc(date)) %>%
      slice(1) %>%
      mutate(
        last_name  = sapply(strsplit(player_name, "\\s+"), tail, 1),
        longest_hr = hit_distance
      ) %>%
      select(team_name, longest_hr, last_name)

    avg_distance <- data %>%
      filter(!is.na(hit_distance)) %>%
      group_by(team_name) %>%
      summarise(
        avg_distance           = round(mean(hit_distance, na.rm = TRUE), 1),
        hr_count_with_distance = n(),
        .groups = "drop"
      )

    longest_hrs <- longest_by_team
  }

  all_teams <- unique(data$team_name)
  result <- data.frame(team_name = all_teams, stringsAsFactors = FALSE)

  result <- result %>%
    left_join(today_hrs,  by = "team_name") %>%
    left_join(past7_hrs,  by = "team_name") %>%
    left_join(past30_hrs, by = "team_name")

  if (!is.null(longest_hrs))  result <- left_join(result, longest_hrs,  by = "team_name")
  if (!is.null(avg_distance)) result <- left_join(result, avg_distance, by = "team_name")

  # Ensure all expected columns exist before mutating
  if (!"longest_hr"             %in% names(result)) result$longest_hr             <- 0
  if (!"last_name"              %in% names(result)) result$last_name              <- ""
  if (!"avg_distance"           %in% names(result)) result$avg_distance           <- 0
  if (!"hr_count_with_distance" %in% names(result)) result$hr_count_with_distance <- 0

  result <- result %>%
    mutate(
      today_hr               = ifelse(is.na(today_hr),  0, today_hr),
      past7_hr               = ifelse(is.na(past7_hr),  0, past7_hr),
      past30_hr              = ifelse(is.na(past30_hr), 0, past30_hr),
      longest_hr             = ifelse(is.na(longest_hr), 0, longest_hr),
      last_name              = ifelse(is.na(last_name),  "", last_name),
      avg_distance           = ifelse(is.na(avg_distance), 0, avg_distance),
      hr_count_with_distance = ifelse(is.na(hr_count_with_distance), 0, hr_count_with_distance)
    )

  return(result)
}

# ---------------------------------------------------------------------------
# Recent HR stats by player — today / 7-day / 30-day
# ---------------------------------------------------------------------------
calculate_player_recent_stats <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(NULL)

  today     <- get_today_pst()
  week_ago  <- today - 7
  month_ago <- today - 30

  today_hrs <- data %>%
    filter(date >= today) %>%
    group_by(team_name, player_name) %>%
    summarise(today_hr = n(), .groups = "drop")

  past7_hrs <- data %>%
    filter(date >= week_ago) %>%
    group_by(team_name, player_name) %>%
    summarise(past7_hr = n(), .groups = "drop")

  past30_hrs <- data %>%
    filter(date >= month_ago) %>%
    group_by(team_name, player_name) %>%
    summarise(past30_hr = n(), .groups = "drop")

  all_players <- unique(data[, c("team_name", "player_name")])

  result <- all_players %>%
    left_join(today_hrs,  by = c("team_name", "player_name")) %>%
    left_join(past7_hrs,  by = c("team_name", "player_name")) %>%
    left_join(past30_hrs, by = c("team_name", "player_name")) %>%
    mutate(
      today_hr  = ifelse(is.na(today_hr),  0, today_hr),
      past7_hr  = ifelse(is.na(past7_hr),  0, past7_hr),
      past30_hr = ifelse(is.na(past30_hr), 0, past30_hr)
    )

  return(result)
}

# ---------------------------------------------------------------------------
# Prepare cumulative data — all players, no top-N filter
# All teams anchored to 0 on the day before opening day so the graph
# always starts at zero rather than the first HR date.
# ---------------------------------------------------------------------------
prepare_cumulative_data <- function(data) {
  opening_day <- get_opening_day()
  all_teams   <- get_team_names()

  # Zero anchor: every team starts at 0 on opening_day - 1
  zero_anchor <- data.frame(
    team_name     = all_teams,
    date          = opening_day - 1,
    cumulative_hr = 0L,
    stringsAsFactors = FALSE
  )

  if (is.null(data) || nrow(data) == 0) {
    return(zero_anchor)
  }

  # ---- Graph-only roster-swap continuity ------------------------------------
  # For players who were swapped IN mid-season, remap all of their HR event
  # dates that fall BEFORE their join date to the join date itself.
  # This creates a visible "jump" on the graph at the swap date rather than
  # retroactively altering the team's historical line.
  # This remapping only affects the graph; HR counts/roster totals are unchanged.
  if (exists("player_date_ranges") &&
      !is.null(player_date_ranges) &&
      nrow(player_date_ranges) > 0) {
    late_starters <- player_date_ranges %>%
      dplyr::filter(!is_historical, start_date > as.Date(opening_day)) %>%
      dplyr::select(player_name, team_name, start_date)

    if (nrow(late_starters) > 0) {
      data <- data %>%
        dplyr::mutate(date = as.Date(date)) %>%
        dplyr::left_join(late_starters, by = c("player_name", "team_name")) %>%
        dplyr::mutate(
          date = dplyr::if_else(!is.na(start_date) & date < start_date,
                                start_date, date)
        ) %>%
        dplyr::select(-start_date)
    }
  }

  daily_counts <- data %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= opening_day) %>%
    group_by(team_name, date) %>%
    summarise(daily_hr = n(), .groups = "drop") %>%
    arrange(team_name, date) %>%
    group_by(team_name) %>%
    mutate(cumulative_hr = cumsum(daily_hr)) %>%
    ungroup() %>%
    select(team_name, date, cumulative_hr)

  raw_cum <- bind_rows(zero_anchor, daily_counts) %>%
    arrange(team_name, date)

  # Expand to every date so all teams share the same x-axis (flat line on no-HR days)
  today_pst <- get_today_pst()
  max_date  <- max(c(max(raw_cum$date), today_pst))
  all_dates <- seq(opening_day - 1, max_date, by = "day")
  full_grid <- expand.grid(
    team_name = all_teams,
    date      = all_dates,
    stringsAsFactors = FALSE
  )
  full_grid$date <- as.Date(full_grid$date)

  cumulative_data <- full_grid %>%
    left_join(raw_cum, by = c("team_name", "date")) %>%
    arrange(team_name, date) %>%
    group_by(team_name) %>%
    tidyr::fill(cumulative_hr, .direction = "down") %>%
    mutate(cumulative_hr = ifelse(is.na(cumulative_hr), 0L, as.integer(cumulative_hr))) %>%
    ungroup()

  if (nrow(cumulative_data) > 0) {
    message("Cumulative data prepared:")
    message(paste("- Date range:", min(cumulative_data$date), "to", max(cumulative_data$date)))
    message(paste("- Teams:", paste(unique(cumulative_data$team_name), collapse = ", ")))
  }

  return(cumulative_data)
}

# ---------------------------------------------------------------------------
# HR stats for ALL batters in raw API data — no roster filtering.
# Used by the Players tab to show every MLB player with HRs this season.
# ---------------------------------------------------------------------------
calculate_all_player_hr_stats <- function(raw_data) {
  if (is.null(raw_data)) return(NULL)
  if (!is.data.frame(raw_data)) {
    tryCatch({ raw_data <- as.data.frame(raw_data) }, error = function(e) return(NULL))
  }
  if (nrow(raw_data) == 0) return(NULL)
  if (!all(c("batter_name", "date") %in% names(raw_data))) return(NULL)

  # Apply the same Max Muncy dedup as process_data
  if ("batter" %in% names(raw_data)) {
    raw_data <- raw_data %>%
      filter(!(batter_name == "Max Muncy" & batter == "691777"))
  }

  raw_data$date <- as.Date(raw_data$date)

  today     <- get_today_pst()
  week_ago  <- today - 7
  month_ago <- today - 30

  raw_data %>%
    group_by(batter_name) %>%
    summarise(
      total_hr  = n(),
      past7_hr  = sum(date >= week_ago),
      past30_hr = sum(date >= month_ago),
      .groups = "drop"
    ) %>%
    rename(player_name = batter_name) %>%
    arrange(desc(total_hr))
}

