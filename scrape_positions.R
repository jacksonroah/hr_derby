#!/usr/bin/env Rscript
# scrape_positions.R
# Fetches player position eligibility from fantasysixpack.net and writes
# cache/player_positions.csv for use by the HR Derby app's Players tab.
#
# Run manually:  Rscript scrape_positions.R
# Cron (4am PT): 0 12 * * * Rscript /path/to/hr_derby/scrape_positions.R
#
# Output CSV columns:
#   player_name      — canonical name as it appears on the source page
#   mlb_team         — MLB team abbreviation (e.g. "NYY", "LAD")
#   positions        — comma-separated eligible non-DH positions (e.g. "OF,1B")
#                      "UTL" if the player is DH-only
#   primary_position — first/primary eligible position (or "UTL" for DH-only)

suppressPackageStartupMessages({
  library(httr)
  library(rvest)
  library(dplyr)
})

URL       <- "https://fantasysixpack.net/mlb-games-played-fantasy-baseball-multi-position-eligibility/"
script_dir <- tryCatch({
  args <- commandArgs(trailingOnly = FALSE)
  file_flag <- "--file="
  script_path <- sub(file_flag, "", args[grep(file_flag, args)])
  if (length(script_path) > 0) dirname(normalizePath(script_path)) else getwd()
}, error = function(e) getwd())
OUT_DIR   <- file.path(script_dir, "cache")
OUT_FILE  <- file.path(OUT_DIR, "player_positions.csv")
TMP_FILE  <- paste0(OUT_FILE, ".tmp")

# Normalise team abbreviations used on the page to standard ones
# Add entries here if the page uses non-standard abbreviations
TEAM_MAP <- c(
  "ARI" = "ARI", "ATL" = "ATL", "BAL" = "BAL", "BOS" = "BOS",
  "CHC" = "CHC", "CWS" = "CHW", "CHW" = "CHW", "CIN" = "CIN",
  "CLE" = "CLE", "COL" = "COL", "DET" = "DET", "HOU" = "HOU",
  "KC"  = "KCR", "KCR" = "KCR", "LAA" = "LAA", "LAD" = "LAD",
  "MIA" = "MIA", "MIL" = "MIL", "MIN" = "MIN", "NYM" = "NYM",
  "NYY" = "NYY", "OAK" = "OAK", "SAC" = "SAC", "PHI" = "PHI",
  "PIT" = "PIT", "SDP" = "SDP", "SD"  = "SDP", "SEA" = "SEA",
  "SFG" = "SFG", "SF"  = "SFG", "STL" = "STL", "TBR" = "TBR",
  "TB"  = "TBR", "TEX" = "TEX", "TOR" = "TOR", "WSH" = "WSH",
  "WAS" = "WSH", "ATH" = "ATH"
)

FIELD_POSITIONS <- c("C", "1B", "2B", "3B", "SS", "OF", "SP", "RP")

cat("Fetching position eligibility from fantasysixpack.net...\n")

resp <- tryCatch(
  httr::GET(URL,
    httr::user_agent(paste0(
      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
      "AppleWebKit/537.36 (KHTML, like Gecko) ",
      "Chrome/122.0.0.0 Safari/537.36"
    )),
    httr::add_headers(
      "Accept"          = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
      "Accept-Language" = "en-US,en;q=0.5",
      "Referer"         = "https://fantasysixpack.net/"
    ),
    httr::timeout(30)
  ),
  error = function(e) {
    cat("ERROR: HTTP request failed:", conditionMessage(e), "\n")
    NULL
  }
)

if (is.null(resp) || httr::http_status(resp)$category != "Success") {
  status_code <- if (!is.null(resp)) httr::status_code(resp) else "N/A"
  cat("ERROR: Failed to fetch page (HTTP", status_code, ")\n")
  cat("The site may be blocking automated requests.\n")
  cat("Options:\n")
  cat("  1. Try running the script again later\n")
  cat("  2. Check if the URL is still valid:", URL, "\n")
  cat("  3. Manually create cache/player_positions.csv\n")
  quit(status = 1)
}

html_text <- httr::content(resp, "text", encoding = "UTF-8")
cat("Page fetched successfully (", nchar(html_text), "chars)\n")

page <- tryCatch(rvest::read_html(html_text), error = function(e) {
  cat("ERROR: Failed to parse HTML:", conditionMessage(e), "\n")
  quit(status = 1)
})

# ---------------------------------------------------------------------------
# Parse strategy: try multiple common page structures in order
# ---------------------------------------------------------------------------

parse_players <- function(page) {

  # --- Strategy 1: table with position columns (C, 1B, 2B, 3B, SS, OF, DH) -
  tables <- rvest::html_elements(page, "table")
  for (tbl in tables) {
    headers <- tbl %>% rvest::html_elements("th") %>% rvest::html_text(trim = TRUE)
    has_pos_cols <- any(c("C", "1B", "2B", "OF") %in% toupper(headers))
    if (has_pos_cols) {
      cat("Detected table format with position columns\n")
      df <- tryCatch(rvest::html_table(tbl, fill = TRUE), error = function(e) NULL)
      if (!is.null(df) && nrow(df) > 0) {
        return(parse_table_format(df, headers))
      }
    }
  }

  # --- Strategy 2: <details> or accordion items with summary = player line ---
  details <- rvest::html_elements(page, "details")
  if (length(details) > 0) {
    cat("Detected <details> accordion format\n")
    return(parse_details_format(details))
  }

  # --- Strategy 3: div/li blocks where player name + team on first line ------
  # Look for a parent container that has many children, each with a name line
  # and position eligibility lines
  cat("Trying generic text-block format...\n")
  return(parse_text_blocks(page))
}

# Strategy 1: table with Player, Team, C, 1B, 2B, 3B, SS, OF, DH columns
parse_table_format <- function(df, headers) {
  headers_upper <- toupper(trimws(headers))

  # Find column indices
  find_col <- function(names) {
    idx <- which(headers_upper %in% toupper(names))
    if (length(idx) > 0) idx[1] else NA
  }

  player_col <- find_col(c("PLAYER", "NAME", "PLAYER NAME"))
  team_col   <- find_col(c("TEAM", "MLB TEAM", "MLB"))
  c_col      <- find_col("C")
  b1_col     <- find_col("1B")
  b2_col     <- find_col("2B")
  b3_col     <- find_col("3B")
  ss_col     <- find_col("SS")
  of_col     <- find_col("OF")
  dh_col     <- find_col("DH")

  if (is.na(player_col)) {
    cat("WARNING: Could not find player column in table\n")
    return(NULL)
  }

  # Map column numbers to positions
  pos_cols <- c(C = c_col, `1B` = b1_col, `2B` = b2_col, `3B` = b3_col,
                SS = ss_col, OF = of_col, DH = dh_col)
  pos_cols <- pos_cols[!is.na(pos_cols)]

  results <- lapply(seq_len(nrow(df)), function(i) {
    row <- df[i, ]
    player_name <- trimws(as.character(row[[player_col]]))
    if (nchar(player_name) == 0 || player_name == "Player" || player_name == "Name") {
      return(NULL)
    }
    mlb_team <- if (!is.na(team_col)) {
      raw_t <- trimws(as.character(row[[team_col]]))
      TEAM_MAP[raw_t] %||% raw_t
    } else NA_character_

    eligible <- names(pos_cols)[sapply(names(pos_cols), function(pos) {
      val <- trimws(toupper(as.character(row[[pos_cols[[pos]]]])))
      val %in% c("X", "YES", "1", "TRUE", "\u2713", "\u2714")
    })]

    make_position_row(player_name, mlb_team, eligible)
  })

  dplyr::bind_rows(Filter(Negate(is.null), results))
}

# Strategy 2: <details> elements where summary contains "PlayerName TEAM"
parse_details_format <- function(details) {
  results <- lapply(details, function(d) {
    summary_text <- d %>% rvest::html_element("summary") %>%
      rvest::html_text(trim = TRUE)
    if (is.na(summary_text) || nchar(summary_text) == 0) return(NULL)

    # Parse "FirstName LastName TEAM" — last token is team abbr
    tokens <- strsplit(trimws(summary_text), "\\s+")[[1]]
    if (length(tokens) < 2) return(NULL)
    mlb_team_raw <- tokens[length(tokens)]
    player_name  <- paste(tokens[-length(tokens)], collapse = " ")
    mlb_team     <- TEAM_MAP[mlb_team_raw] %||% mlb_team_raw

    # Find position lines: "C: X", "OF: X", etc.
    body_text <- d %>% rvest::html_elements("li, p, span, div") %>%
      rvest::html_text(trim = TRUE)

    eligible <- character(0)
    for (line in body_text) {
      m <- regmatches(line, regexpr("^([A-Z0-9]+):\\s*(X|x|Yes|yes)?", line))
      if (length(m) > 0) {
        parts <- strsplit(m, ":\\s*")[[1]]
        pos   <- toupper(trimws(parts[1]))
        has_x <- length(parts) > 1 && grepl("^[Xx]|[Yy]es", parts[2])
        if (pos %in% c(FIELD_POSITIONS, "DH") && has_x) eligible <- c(eligible, pos)
      }
    }
    make_position_row(player_name, mlb_team, eligible)
  })
  dplyr::bind_rows(Filter(Negate(is.null), results))
}

# Strategy 3: generic text parsing — look for "Name TEAM" patterns near position markers
parse_text_blocks <- function(page) {
  # Get all text content and look for patterns
  # This is a fallback — print a sample so the user can debug selectors
  all_text <- page %>% rvest::html_text(trim = TRUE)
  sample <- paste(strsplit(all_text, "\n")[[1]][1:50], collapse = "\n")
  cat("Could not detect page structure. First 50 lines of text:\n")
  cat(sample, "\n")
  cat("\nPlease inspect the page HTML and update parse logic in scrape_positions.R\n")
  NULL
}

# Build a single-row data.frame from player info + eligible position list
make_position_row <- function(player_name, mlb_team, eligible) {
  # Remove DH from eligible to get field positions
  field_eligible <- eligible[eligible %in% FIELD_POSITIONS]

  if (length(field_eligible) == 0) {
    # DH-only or no data
    positions        <- "UTL"
    primary_position <- "UTL"
  } else {
    # Order by standard fantasy priority
    ordered <- field_eligible[order(match(field_eligible,
                                          c("C","SS","2B","3B","1B","OF","SP","RP")))]
    positions        <- paste(ordered, collapse = ",")
    primary_position <- ordered[1]
  }

  data.frame(
    player_name      = player_name,
    mlb_team         = if (is.na(mlb_team)) "" else mlb_team,
    positions        = positions,
    primary_position = primary_position,
    stringsAsFactors = FALSE
  )
}

# Null-coalescing helper
`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && nchar(a) > 0) a else b

# ---------------------------------------------------------------------------
# Run the scraper
# ---------------------------------------------------------------------------
result <- parse_players(page)

if (is.null(result) || nrow(result) == 0) {
  cat("ERROR: No player data could be parsed from the page.\n")
  cat("The page structure may have changed. Check the URL and HTML structure.\n")
  quit(status = 1)
}

result <- result %>%
  filter(nchar(player_name) > 0) %>%
  distinct(player_name, .keep_all = TRUE)

cat("Parsed", nrow(result), "players\n")

# Write atomically
dir.create(dirname(OUT_FILE), showWarnings = FALSE, recursive = TRUE)
write.csv(result, TMP_FILE, row.names = FALSE)
file.rename(TMP_FILE, OUT_FILE)

cat("Saved to", OUT_FILE, "\n")
cat("Done.\n")
