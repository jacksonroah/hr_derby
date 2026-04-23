# server.R - 2026 HR Derby server logic
library(shiny)
library(ggplot2)
library(dplyr)
library(httr)
library(jsonlite)

source("config.R")
source("data_processing.R")
options(viewer = NULL, shiny.launch.browser = TRUE)

# Disk cache — speeds up initial load by serving last-known data immediately
CACHE_DIR      <- "cache"
CACHE_HR_FILE  <- file.path(CACHE_DIR, "hr_data.rds")
CACHE_API_FILE <- file.path(CACHE_DIR, "total_api_hrs.rds")
CACHE_RAW_FILE <- file.path(CACHE_DIR, "raw_api_data.rds")
dir.create(CACHE_DIR, showWarnings = FALSE)

# Ordinal suffix: 1 -> "1st", 2 -> "2nd", etc.
ordinal_suffix <- function(n) {
  n <- as.integer(n)
  s <- if (n %% 100 %in% 11:13) "th" else
       c("th","st","nd","rd","th","th","th","th","th","th")[n %% 10 + 1]
  paste0(n, s)
}

# Convert hex color to rgba string
hex_to_rgba <- function(hex, alpha) {
  hex <- gsub("^#", "", hex)
  r <- strtoi(substr(hex, 1, 2), 16)
  g <- strtoi(substr(hex, 3, 4), 16)
  b <- strtoi(substr(hex, 5, 6), 16)
  paste0("rgba(", r, ",", g, ",", b, ",", alpha, ")")
}

# ---------------------------------------------------------------------------
# Position sort order
# ---------------------------------------------------------------------------
POSITION_ORDER <- c("OF", "1B", "2B", "3B", "SS", "C", "UTL")

# ---------------------------------------------------------------------------
# Shared graph data builder — used by all 4 graph variants
# ---------------------------------------------------------------------------
build_graph_base <- function(cum, lb_result) {
  standings <- if (!is.null(lb_result)) {
    lb_result$leaderboard %>% arrange(desc(team_total))
  } else {
    data.frame(team_name = get_team_names())
  }
  display_map <- setNames(
    sapply(get_team_names(), function(t) CONFIG$teams$team_info[[t]]$display_name),
    get_team_names()
  )
  cum_data <- cum %>%
    mutate(
      team_name  = factor(team_name, levels = standings$team_name),
      team_label = display_map[as.character(team_name)]
    )
  list(
    cum_data    = cum_data,
    team_colors = get_team_colors(for_graph = TRUE),
    opening_day = get_opening_day()
  )
}

server <- function(input, output, session) {

  status        <- reactiveVal("Initializing...")
  # Load from disk cache so standings display immediately on reload
  hr_data       <- reactiveVal(if (file.exists(CACHE_HR_FILE))  readRDS(CACHE_HR_FILE)  else NULL)
  total_api_hrs <- reactiveVal(if (file.exists(CACHE_API_FILE)) readRDS(CACHE_API_FILE) else 0L)
  raw_api_data  <- reactiveVal(if (file.exists(CACHE_RAW_FILE)) readRDS(CACHE_RAW_FILE) else NULL)

  # ---- API polling --------------------------------------------------------
  # Single GET per cycle; on error keeps old hr_data so UI never blanks out.
  observe({
    invalidateLater(get_poll_interval(), session)
    tryCatch({
      raw_response <- httr::GET(get_api_url())

      if (httr::http_status(raw_response)$category != "Success") {
        status(paste("API Error:", httr::http_status(raw_response)$message))
        return()
      }

      raw_content <- httr::content(raw_response, "text", encoding = "UTF-8")
      data        <- jsonlite::fromJSON(raw_content)
      if (is.data.frame(data)) {
        raw_api_data(data)
        tryCatch(saveRDS(data, CACHE_RAW_FILE),
                 error = function(e) warning("Raw cache write failed: ", e$message))
      }
      processed   <- process_data(data, drafted_players)

      if (!is.null(processed) && nrow(processed) > 0) {
        hr_data(processed)
        # Track total MLB HR count from raw API response (for % calculation)
        api_count <- if (is.data.frame(data)) nrow(data) else 0L
        total_api_hrs(api_count)
        # Persist to disk cache so next app start is instant
        tryCatch({
          saveRDS(processed, CACHE_HR_FILE)
          saveRDS(api_count, CACHE_API_FILE)
        }, error = function(e) warning("Cache write failed: ", e$message))
        status(paste("Updated —", nrow(processed), "HR records"))
      } else {
        status("No matching players found in API data.")
      }
    }, error = function(e) {
      status(paste("Error fetching data:", e$message))
      # hr_data unchanged — UI stays stable with last good data
    })
  })

  output$status_message <- renderText({ status() })

  # ---- Core reactive data -------------------------------------------------
  total_hr_per_player <- reactive({
    calculate_total_hr_per_player(hr_data())
  })

  leaderboard_data <- reactive({
    req(total_hr_per_player())
    create_leaderboard(total_hr_per_player(), hr_data())
  })

  recent_team_stats <- reactive({
    calculate_recent_hr_stats(hr_data())
  })

  recent_player_stats <- reactive({
    calculate_player_recent_stats(hr_data())
  })

  cumulative_hr_data <- reactive({
    prepare_cumulative_data(hr_data())
  })

  # ---- Sort mode (HR vs position) -----------------------------------------
  sort_mode <- reactiveVal("hr")

  # ---- Position tab view mode (rank vs HR total) --------------------------
  pos_view_mode <- reactiveVal("rank")

  observeEvent(input$sort_by_hr, {
    sort_mode("hr")
    session$sendCustomMessage("updateSortButtons", list(active = "hr"))
  })

  observeEvent(input$sort_by_position, {
    sort_mode("position")
    session$sendCustomMessage("updateSortButtons", list(active = "position"))
  })

  # ---- Expanded card state ------------------------------------------------
  expanded_cards <- reactiveValues()
  # Initialize all teams to collapsed
  for (tm in get_team_names()) {
    expanded_cards[[tm]] <- FALSE
  }

  observeEvent(input$toggle_card, {
    team <- input$toggle_card$team
    if (!is.null(team) && team %in% get_team_names()) {
      expanded_cards[[team]] <- !isTRUE(expanded_cards[[team]])
    }
  })

  observeEvent(input$expand_all, {
    for (tm in get_team_names()) expanded_cards[[tm]] <- TRUE
  })

  observeEvent(input$collapse_all, {
    for (tm in get_team_names()) expanded_cards[[tm]] <- FALSE
  })

  observeEvent(input$pos_show_rank, {
    pos_view_mode("rank")
    session$sendCustomMessage("updatePosButtons", list(active = "rank"))
  })

  observeEvent(input$pos_show_hr, {
    pos_view_mode("hr")
    session$sendCustomMessage("updatePosButtons", list(active = "hr"))
  })

  # ---- Players tab state ---------------------------------------------------
  players_sort_mode <- reactiveVal("hr")
  players_show_all  <- reactiveVal(TRUE)

  observeEvent(input$players_sort_hr, {
    players_sort_mode("hr")
    session$sendCustomMessage("updatePlayersButtons",   list(active = "hr"))
    session$sendCustomMessage("updatePlayersPosFilter", list(active = "none"))
  })

  for (.pos in c("OF", "1B", "2B", "3B", "SS", "C", "UTL")) {
    local({
      p <- .pos
      observeEvent(input[[paste0("players_pos_", p)]], {
        players_sort_mode(p)
        session$sendCustomMessage("updatePlayersButtons",   list(active = "none"))
        session$sendCustomMessage("updatePlayersPosFilter", list(active = p))
      }, ignoreInit = TRUE)
    })
  }

  observeEvent(input$players_filter_available, {
    players_show_all(FALSE)
    session$sendCustomMessage("updatePlayersFilter", list(active = "available"))
  })

  observeEvent(input$players_filter_all, {
    players_show_all(TRUE)
    session$sendCustomMessage("updatePlayersFilter", list(active = "all"))
  })

  # ---- Players tab data ----------------------------------------------------
  player_positions_data <- reactive({
    path <- file.path(CACHE_DIR, "player_positions.csv")
    if (file.exists(path)) {
      tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
    } else NULL
  })

  all_player_hr_stats <- reactive({
    raw <- raw_api_data()
    if (is.null(raw)) return(NULL)
    calculate_all_player_hr_stats(raw)
  })

  players_tab_data <- reactive({
    hr_stats  <- all_player_hr_stats()
    positions <- player_positions_data()

    if (is.null(hr_stats) || nrow(hr_stats) == 0) return(NULL)

    hr_stats$norm_name <- sapply(hr_stats$player_name, normalize_name)

    # Attach position + MLB team from positions CSV
    if (!is.null(positions) && nrow(positions) > 0) {
      positions$norm_name <- sapply(positions$player_name, normalize_name)
      keep <- intersect(c("norm_name", "mlb_team", "positions", "primary_position"),
                        names(positions))
      hr_stats <- hr_stats %>%
        left_join(positions[, keep], by = "norm_name")
    } else {
      hr_stats$mlb_team        <- NA_character_
      hr_stats$positions       <- NA_character_
      hr_stats$primary_position <- NA_character_
    }

    # Attach derby team from roster (and fill mlb_team from roster if missing)
    roster_info <- drafted_players %>%
      mutate(norm_name = sapply(player_name, normalize_name)) %>%
      filter(position != "BENCH") %>%
      select(norm_name, derby_team = team_name, roster_mlb = mlb_team) %>%
      distinct()

    hr_stats <- hr_stats %>%
      left_join(roster_info, by = "norm_name") %>%
      mutate(
        mlb_team = ifelse(is.na(mlb_team) & !is.na(roster_mlb), roster_mlb, mlb_team)
      ) %>%
      select(-norm_name, -roster_mlb)

    # Players excluded from the display (e.g. too many positions that break layout)
    EXCLUDED_PLAYERS <- c("Mauricio Dubon")
    hr_stats <- hr_stats %>%
      filter(!player_name %in% EXCLUDED_PLAYERS)

    hr_stats
  })

  # =========================================================================
  # Tab 1 — League Standings table
  # =========================================================================
  output$league_standings_table <- renderUI({
    player_data <- total_hr_per_player()
    if (is.null(player_data) || nrow(player_data) == 0) {
      return(div(class = "loading-msg", "Loading standings..."))
    }

    lb_result <- leaderboard_data()
    if (is.null(lb_result)) {
      return(div(class = "loading-msg", "Loading standings..."))
    }

    lb <- lb_result$leaderboard

    recent   <- recent_team_stats()
    recent_p <- recent_player_stats()
    if (!is.null(recent)) {
      lb <- lb %>%
        left_join(recent, by = "team_name") %>%
        mutate(
          today_hr  = ifelse(is.na(today_hr),  0, today_hr),
          past7_hr  = ifelse(is.na(past7_hr),  0, past7_hr),
          past30_hr = ifelse(is.na(past30_hr), 0, past30_hr)
        )
    } else {
      lb$today_hr  <- 0
      lb$past7_hr  <- 0
      lb$past30_hr <- 0
    }

    # Sort: primary = total HR desc, tiebreaker = avg HR distance desc (BENCH excluded)
    lb <- lb %>%
      arrange(desc(team_total), desc(coalesce(avg_distance, 0))) %>%
      mutate(rank = row_number())

    # Trigger GOAT MODE if any player hit 4+ HRs today
    if (!is.null(recent_p) && "today_hr" %in% names(recent_p)) {
      goat_active <- any(recent_p$today_hr >= 4, na.rm = TRUE)
      session$sendCustomMessage("goatMode", list(active = goat_active))
    }

    # Mark tied positions for tiebreaker note
    totals  <- lb$team_total
    n_teams <- nrow(lb)
    is_tied <- vapply(seq_len(n_teams), function(i) {
      (i > 1       && totals[i] == totals[i - 1]) ||
      (i < n_teams && totals[i] == totals[i + 1])
    }, logical(1))

    total_hr_all   <- sum(lb$team_total, na.rm = TRUE)
    max_team_total <- max(lb$team_total, na.rm = TRUE)
    if (max_team_total == 0) max_team_total <- 1  # avoid division by zero

    api_total     <- total_api_hrs()
    roster_pct_val <- if (api_total > 0)
      paste0(round(100 * total_hr_all / api_total, 1), "%")
    else NULL

    # Split total_feet_all into label + number so we can colour them separately
    feet_val <- if (!is.null(recent) && "avg_distance" %in% names(lb) &&
                    "hr_count_with_distance" %in% names(lb)) {
      total_ft <- sum(lb$avg_distance * lb$hr_count_with_distance, na.rm = TRUE)
      if (total_ft > 0) paste0(format(round(total_ft), big.mark = ","), " ft") else ""
    } else ""

    inline_stat <- function(label, value) {
      div(class = "total-feet-line",
        style = "display:flex; gap:4px; justify-content:flex-end; align-items:baseline; white-space:nowrap; flex-wrap:nowrap;",
        tags$span(style = "white-space:nowrap;", label),
        tags$span(style = "color:white; font-weight:700; white-space:nowrap;", value)
      )
    }

    # Header bar
    header_bar <- div(class = "standings-header-bar",
      div(class = "header-left",
        tags$span(class = "standings-title", "2026 HR DERBY STANDINGS")
      ),
      div(class = "header-totals",
        # Dinger count — label inline to the left of the big number
        div(style = "display:flex; align-items:baseline; gap:5px; justify-content:flex-end; white-space:nowrap; flex-wrap:nowrap;",
          tags$span(class = "season-label",
                    style = "display:inline; margin-bottom:0; white-space:nowrap;",
                    "TOTAL DINGERS:"),
          tags$span(class = "total-hr-number", total_hr_all),
          tags$span(class = "hr-unit", "HR")
        ),
        if (!is.null(roster_pct_val))
          inline_stat("MLB HRs on Rosters:", roster_pct_val),
        if (nchar(feet_val) > 0)
          inline_stat("Total HR Distance:", feet_val)
      )
    )

    # Column headers
    col_headers <- div(class = "standings-col-headers",
      div(class = "sh-rk", "RK"),
      div(class = "sh-team", "TEAM"),
      div(class = "sh-div"),
      div(class = "sh-total", "TOTAL HR"),
      div(class = "sh-day", "DAY"),
      div(class = "sh-week", "7D"),
      div(class = "sh-month", "30D")
    )

    # Standings layout variant: "A" = color left/white right, "B" = white left/color right
    variant <- CONFIG$game$standings_variant

    # Team rows
    has_dist <- "avg_distance" %in% names(lb)

    team_rows <- lapply(seq_len(nrow(lb)), function(i) {
      row   <- lb[i, ]
      team  <- row$team_name
      tinfo <- CONFIG$teams$team_info[[team]]
      squad <- if (!is.null(tinfo$squad_name) && nchar(tinfo$squad_name) > 0) tinfo$squad_name else NULL

      tie_note <- if (is_tied[i] && has_dist && !is.na(row$avg_distance) && row$avg_distance > 0) {
        tags$span(class = "tie-note", paste0("Avg: ", round(row$avg_distance), " ft"))
      } else NULL

      color_bg   <- paste0("background:", tinfo$primary_color, ";")
      color_text <- paste0("color:", tinfo$text_color, ";")
      white_bg   <- "background:white;"

      left_style  <- if (variant == "A") paste0(color_bg, color_text) else white_bg
      right_style <- if (variant == "A") white_bg else paste0(color_bg, color_text)
      divider_color <- tinfo$primary_color

      # For the right side, text needs the right contrast color
      right_text_style <- if (variant == "B") color_text else ""

      day_content <- if (row$today_hr > 0) {
        tags$span(class = "day-pill", paste0("+", row$today_hr))
      } else {
        tags$span(class = "em-dash", "—")
      }

      # Rank circle: white on dark bg, team color on white bg
      rank_circle_style <- if ((variant == "A")) {
        paste0("background: rgba(255,255,255,0.25); color:", tinfo$text_color, ";")
      } else {
        paste0("background:", tinfo$primary_color, "; color:", tinfo$text_color, ";")
      }

      # Today's scorers for this team
      today_scorers_row <- NULL
      if (!is.null(recent_p)) {
        tp_today <- recent_p[recent_p$team_name == team & recent_p$today_hr > 0, ]
        if (nrow(tp_today) > 0) {
          player_lines <- mapply(function(nm, hr_count) {
            parts <- strsplit(trimws(nm), "\\s+")[[1]]
            short <- if (length(parts) <= 1) nm else
              paste0(substr(parts[1], 1, 1), ". ", paste(parts[-1], collapse = " "))
            badge_color <- if (hr_count >= 4) "#7C3AED" else "#DC2626"
            flair_text  <- if (hr_count == 2) "BANG BANG" else
                           if (hr_count == 3) "OH BABY A TRIPLE" else
                           if (hr_count >= 4) "THATS WHY HES THE GOAT" else ""
            badge <- if (hr_count >= 2) {
              tags$span(style = paste0("margin-left:4px; background:", badge_color, "; color:white; border-radius:3px; padding:0 3px; font-size:10px; font-weight:700;"),
                paste0("\u00d7", hr_count))
            } else NULL
            flair <- if (nchar(flair_text) > 0) {
              tags$span(style = "margin-left:4px; font-weight:700; color:#DC2626; font-size:10px;", flair_text)
            } else NULL
            div(style = "display:flex; align-items:center; color:#1E293B; padding-left:2px;",
              tags$span(short), badge, flair)
          }, tp_today$player_name, tp_today$today_hr, SIMPLIFY = FALSE)
          today_scorers_row <- div(
            style = "font-size: 11px; line-height: 1.4; padding-top: 3px;",
            tags$span(style = "font-weight: 700; color: #16A34A;", "Today:"),
            tagList(player_lines)
          )
        }
      }

      div(class = "standings-team-row", style = "background:white; padding:0; overflow:hidden;",
        # Left side
        div(class = "sr-left", style = left_style,
          div(class = "rank-circle", style = rank_circle_style, i),
          div(class = "sr-team",
            div(class = "sr-name-row",
              tags$span(class = "team-name-bold", tinfo$display_name),
              tags$span(class = "team-abbr-inline", tinfo$abbr)
            ),
            if (!is.null(squad)) tags$span(class = "team-squad-name", paste0("\u201c", squad, "\u201d"))
          )
        ),
        # Divider
        div(class = "col-divider-bar",
            style = paste0("background:", divider_color, ";")),
        # Right side — column so scorers slot in below numbers
        div(class = "sr-right",
            style = paste0(right_style, " flex-direction:column; align-items:flex-start; justify-content:center;"),
          div(style = "display:flex; align-items:center;",
            div(class = "sr-total", style = right_text_style,
              div(class = "sr-total-nums",
                tags$span(style = "font-size:22px; font-weight:700;", row$team_total),
                tags$span(class = "sr-hr-unit", "HR")
              ),
              tie_note
            ),
            div(class = "sr-day", style = right_text_style, day_content),
            div(class = "sr-week", style = right_text_style, row$past7_hr),
            div(class = "sr-month", style = right_text_style, row$past30_hr)
          ),
          today_scorers_row
        )
      )
    })

    div(class = "standings-container",
      header_bar,
      col_headers,
      team_rows
    )
  })

  outputOptions(output, "league_standings_table", suspendWhenHidden = FALSE)

  # =========================================================================
  # Tab 2 — Team Roster Cards
  # =========================================================================
  output$team_roster_cards <- renderUI({
    player_data <- total_hr_per_player()
    if (is.null(player_data) || nrow(player_data) == 0) {
      return(div(class = "loading-msg", "Loading rosters..."))
    }

    lb_result <- leaderboard_data()
    lb_order  <- if (!is.null(lb_result)) {
      lb_result$leaderboard %>% arrange(desc(team_total)) %>% pull(team_name)
    } else {
      get_team_names()
    }

    recent_p   <- recent_player_stats()
    cur_sort   <- sort_mode()

    # Build team summary for card header stats
    lb_summary <- if (!is.null(lb_result)) {
      lb_result$leaderboard
    } else {
      player_data %>%
        group_by(team_name) %>%
        summarise(team_total = sum(total_home_runs), .groups = "drop")
    }

    recent_t <- recent_team_stats()

    # Global position rank — rank within each position across ALL teams
    global_pos_ranks <- player_data %>%
      group_by(position) %>%
      mutate(global_pos_rank = rank(-total_home_runs, ties.method = "min")) %>%
      ungroup() %>%
      select(team_name, player_name, global_pos_rank)

    cards <- lapply(seq_along(lb_order), function(i) {
      team  <- lb_order[i]
      tinfo <- CONFIG$teams$team_info[[team]]

      is_expanded <- isTRUE(expanded_cards[[team]])

      # Team-level stats for card header
      t_total <- lb_summary$team_total[lb_summary$team_name == team]
      t_total <- if (length(t_total) == 0 || is.na(t_total)) 0 else t_total

      t_today  <- 0; t_week <- 0; t_month <- 0
      if (!is.null(recent_t) && team %in% recent_t$team_name) {
        tr <- recent_t[recent_t$team_name == team, ]
        t_today  <- tr$today_hr[1]
        t_week   <- tr$past7_hr[1]
        t_month  <- tr$past30_hr[1]
      }

      # Card header
      chevron <- if (is_expanded) "\u25BE" else "\u25B8"

      card_header <- div(
        class   = "card-header",
        style   = paste0(
          "border-left: 4px solid ", tinfo$primary_color, ";",
          "background: linear-gradient(90deg, ", hex_to_rgba(tinfo$primary_color, 0.45), " 0%, white 55%);"
        ),
        onclick = paste0("toggleCard('", team, "')"),

        div(class = "ch-rank-name",
          div(class = "rank-circle-sm",
            style = "background:#64748B;",
            i),
          div(class = "team-color-dot",
            style = paste0("background:", tinfo$primary_color, "; margin-left:2px;")),
          tags$span(class = "team-name-bold", style = "font-size:17px;", tinfo$display_name),
          tags$span(class = "team-abbr-mono", tinfo$abbr)
        ),
        div(class = "col-divider-bar", style = "height:22px; margin: 0 6px;"),
        div(class = "ch-stats",
          # Total HR with label
          div(class = "ch-stat-col",
            div(class = "ch-total",
              style = paste0("color:", tinfo$primary_color, ";"),
              t_total),
            div(class = "ch-label", "total HR")
          ),
          # Today with label
          div(class = "ch-stat-col",
            div(class = "ch-day",
              if (t_today > 0)
                tags$span(class = "day-pill-sm", paste0("+", t_today))
              else
                tags$span(class = "em-dash", "\u2014")),
            div(class = "ch-label", "today")
          ),
          # 7-day with label
          div(class = "ch-stat-col",
            div(class = "ch-week", t_week),
            div(class = "ch-label", "7-day")
          ),
          # 30-day with label
          div(class = "ch-stat-col",
            div(class = "ch-month", t_month),
            div(class = "ch-label", "30-day")
          )
        ),
        div(class = "ch-chevron", chevron)
      )

      # Roster table (only when expanded)
      roster_table <- if (is_expanded) {
        # Get players for this team
        team_players <- player_data %>% filter(team_name == team)

        # Merge recent player stats
        if (!is.null(recent_p) && nrow(recent_p) > 0) {
          tp_recent <- recent_p %>% filter(team_name == team)
          if (nrow(tp_recent) > 0) {
            team_players <- team_players %>%
              left_join(tp_recent, by = c("team_name", "player_name")) %>%
              mutate(
                today_hr  = ifelse(is.na(today_hr),  0, today_hr),
                past7_hr  = ifelse(is.na(past7_hr),  0, past7_hr),
                past30_hr = ifelse(is.na(past30_hr), 0, past30_hr)
              )
          } else {
            team_players <- team_players %>%
              mutate(today_hr = 0, past7_hr = 0, past30_hr = 0)
          }
        } else {
          team_players <- team_players %>%
            mutate(today_hr = 0, past7_hr = 0, past30_hr = 0)
        }

        # Join global position rank
        team_players <- team_players %>%
          left_join(global_pos_ranks, by = c("team_name", "player_name"))

        # Sort — BENCH always pinned to bottom regardless of sort mode
        if (cur_sort == "position") {
          team_players <- team_players %>%
            mutate(
              is_bench  = position == "BENCH",
              pos_order = match(position, POSITION_ORDER)
            ) %>%
            arrange(is_bench, pos_order, desc(total_home_runs)) %>%
            select(-pos_order, -is_bench)
        } else {
          team_players <- team_players %>%
            mutate(is_bench = position == "BENCH") %>%
            arrange(is_bench, desc(total_home_runs)) %>%
            select(-is_bench)
        }

        sub_header_bg <- hex_to_rgba(tinfo$primary_color, 0.28)
        row_bg_uniform <- hex_to_rgba(tinfo$primary_color, 0.08)

        sub_header <- tags$tr(
          style = paste0("background:", sub_header_bg, ";"),
          tags$th("RK"),
          tags$th("POS"),
          tags$th("MLB"),
          tags$th("PLAYER"),
          tags$th("TOTAL HR"),
          tags$th(style = "width:40px;", "POS RK"),
          tags$th("AVG DIST"),
          tags$th("DAY"),
          tags$th("7D"),
          tags$th("30D")
        )

        player_rows <- lapply(seq_len(nrow(team_players)), function(j) {
          p <- team_players[j, ]

          pos_info <- CONFIG$positions[[p$position]]

          # Global position rank as ordinal pill
          g_rank <- if (!is.null(p$global_pos_rank) && !is.na(p$global_pos_rank)) {
            as.integer(p$global_pos_rank)
          } else { 1L }

          pos_rk_pill <- if (!is.null(pos_info)) {
            tags$span(class = "pos-rank-circle",
              style = paste0("background:", pos_info$bg_color, "; color:", pos_info$text_color, ";"),
              ordinal_suffix(g_rank))
          } else {
            tags$span(class = "pos-rank-circle",
              style = "background:#F1F5F9; color:#64748B;",
              ordinal_suffix(g_rank))
          }

          day_cell <- if (p$today_hr > 0) {
            tags$span(class = "day-pill-sm", paste0("+", p$today_hr))
          } else {
            tags$span(class = "em-dash", "\u2014")
          }

          mlb_abbr <- if ("mlb_team" %in% names(p) && !is.na(p$mlb_team)) p$mlb_team else ""
          is_bench <- !is.null(p$position) && p$position == "BENCH"

          display_pos <- dplyr::case_when(
            p$position == "BENCH" ~ "BN",
            p$position == "UTL"   ~ "UT",
            TRUE                  ~ p$position
          )

          pos_badge_display <- if (!is.null(pos_info)) {
            tags$span(class = "pos-badge",
              style = paste0(
                "background:", pos_info$bg_color,
                "; color:", pos_info$text_color, ";"
              ),
              display_pos)
          } else {
            tags$span(class = "pos-badge", style = "background:#F1F5F9; color:#64748B;",
              display_pos)
          }

          avg_dist_val <- if ("avg_distance" %in% names(p) && !is.na(p$avg_distance) && p$avg_distance > 0) {
            if (team == "Derek") {
              paste0(as.integer(round(p$avg_distance * 0.3048)), "m")
            } else {
              as.integer(round(p$avg_distance))
            }
          } else {
            tags$span(class = "em-dash", "\u2014")
          }

          tags$tr(
            class = if (is_bench) "bench-row" else "",
            style = paste0("background:", row_bg_uniform, ";"),
            tags$td(style = "font-size:11px;", j),
            tags$td(pos_badge_display),
            tags$td(tags$span(class = "mlb-badge", mlb_abbr)),
            tags$td(style = "text-align:left; font-weight:700;", p$player_name),
            tags$td(style = "font-weight:700; font-size:15px; color:#1E293B;", p$total_home_runs),
            tags$td(style = "width:40px;", pos_rk_pill),
            tags$td(avg_dist_val),
            tags$td(day_cell),
            tags$td(p$past7_hr),
            tags$td(p$past30_hr)
          )
        })

        div(class = "roster-table-wrapper",
          tags$table(class = "roster-table",
            tags$thead(sub_header),
            tags$tbody(player_rows)
          )
        )
      } else {
        NULL
      }

      div(class = "team-card",
        card_header,
        roster_table
      )
    })

    div(class = "cards-container", cards)
  })

  outputOptions(output, "team_roster_cards", suspendWhenHidden = FALSE)

  # =========================================================================
  # Tab 3 — By Position grid
  # =========================================================================
  output$position_grid <- renderUI({
    player_data <- total_hr_per_player()
    if (is.null(player_data) || nrow(player_data) == 0) {
      return(div(class = "loading-msg", "Loading position data..."))
    }

    lb_result  <- leaderboard_data()
    team_order <- if (!is.null(lb_result)) {
      lb_result$leaderboard %>% arrange(desc(team_total)) %>% pull(team_name)
    } else {
      get_team_names()
    }

    cur_view  <- pos_view_mode()
    positions <- c("OF", "1B", "2B", "3B", "SS", "C")

    # Typography scale for ranks 1–8: size + weight + darkness (no background color)
    rank_type <- list(
      list(size="20px", weight="800", color="#0F172A"),
      list(size="18px", weight="700", color="#1E293B"),
      list(size="17px", weight="700", color="#334155"),
      list(size="15px", weight="600", color="#475569"),
      list(size="14px", weight="500", color="#64748B"),
      list(size="13px", weight="500", color="#94A3B8"),
      list(size="12px", weight="400", color="#CBD5E1"),
      list(size="12px", weight="400", color="#E2E8F0")
    )

    # Per-team HR totals and rank within each position
    pos_data <- player_data %>%
      filter(position %in% positions) %>%
      group_by(team_name, position) %>%
      summarise(pos_hr = sum(total_home_runs), .groups = "drop") %>%
      group_by(position) %>%
      mutate(pos_rank = rank(-pos_hr, ties.method = "min")) %>%
      ungroup()

    pos_colors <- get_position_colors()

    # Header row — "TEAM" label + position badges
    header_row <- tags$tr(
      tags$th(class = "pg-team-header", "TEAM"),
      lapply(positions, function(pos) {
        pc <- pos_colors[[pos]]
        tags$th(class = "pg-pos-header",
          tags$span(class = "pos-badge",
            style = paste0("background:", pc$bg_color, "; color:", pc$text_color,
                           "; font-size:11px; padding:3px 8px;"),
            pos
          )
        )
      })
    )

    # Team rows
    team_rows <- lapply(team_order, function(team) {
      tinfo <- CONFIG$teams$team_info[[team]]

      rank_cells <- lapply(positions, function(pos) {
        row <- pos_data %>% filter(team_name == team, position == pos)
        rk  <- if (nrow(row) == 0 || is.na(row$pos_rank[1])) 8L else as.integer(row$pos_rank[1])
        hrs <- if (nrow(row) == 0 || is.na(row$pos_hr[1]))   0L  else as.integer(row$pos_hr[1])
        ty  <- rank_type[[min(rk, 8L)]]
        display_val <- if (cur_view == "rank") rk else hrs
        tags$td(class = "pg-rank-cell",
          tags$span(class = "pg-rank-num",
            style = paste0("font-size:", ty$size, "; font-weight:", ty$weight,
                           "; color:", ty$color, ";"),
            display_val)
        )
      })

      tags$tr(class = "pg-team-row",
        tags$td(class = "pg-team-cell",
          style = paste0("background:", tinfo$primary_color, "; color:", tinfo$text_color, ";"),
          tags$span(style = paste0("color:", tinfo$text_color,
                                   "; font-weight:700; font-size:13px;"),
            tinfo$display_name)
        ),
        rank_cells
      )
    })

    div(class = "pos-grid-container",
      tags$table(class = "pos-grid-table",
        tags$thead(header_row),
        tags$tbody(team_rows)
      )
    )
  })

  outputOptions(output, "position_grid", suspendWhenHidden = FALSE)

  # =========================================================================
  # Tab 4 — Players (all MLB players w/ HRs, position eligibility)
  # =========================================================================
  output$players_tab_view <- renderUI({
    data     <- players_tab_data()
    show_all <- isTRUE(players_show_all())
    sort_mode <- players_sort_mode()

    if (is.null(data) || nrow(data) == 0) {
      pos_path <- file.path(CACHE_DIR, "player_positions.csv")
      if (!file.exists(pos_path)) {
        return(div(class = "loading-msg",
          "Run ", tags$code("Rscript scrape_positions.R"),
          " to populate player positions, then reload."))
      }
      return(div(class = "loading-msg", "Loading player data..."))
    }

    # Filter: available only (not on any derby roster)
    if (!show_all) {
      data <- data %>% filter(is.na(derby_team))
    }

    if (nrow(data) == 0) {
      return(div(class = "loading-msg",
        "No available players found. Try switching to \"All Players\"."))
    }

    # Sort / filter by position or HR
    pos_filter_values <- c("OF", "1B", "2B", "3B", "SS", "C", "UTL")
    if (sort_mode %in% pos_filter_values) {
      data <- data %>%
        filter(
          (!is.na(positions) & grepl(sort_mode, positions, fixed = TRUE)) |
          (!is.na(primary_position) & primary_position == sort_mode)
        ) %>%
        arrange(desc(total_hr))
    } else {
      data <- data %>% arrange(desc(total_hr))
    }

    # Cap display at 300 rows for performance
    data <- head(data, 300)

    make_pos_badges <- function(positions_str, primary_pos) {
      if (!is.na(positions_str) && nchar(positions_str) > 0) {
        pos_list <- trimws(strsplit(positions_str, ",")[[1]])
      } else if (!is.na(primary_pos) && nchar(primary_pos) > 0) {
        pos_list <- primary_pos
      } else {
        return(list(tags$span(class = "em-dash", "\u2014")))
      }
      # When filtering by a position, show that position's badge first
      if (sort_mode %in% pos_filter_values && sort_mode %in% pos_list) {
        pos_list <- c(sort_mode, pos_list[pos_list != sort_mode])
      }
      lapply(pos_list, function(pos) {
        pos_info    <- CONFIG$positions[[pos]]
        display_pos <- if (pos == "UTL") "UT" else pos
        if (!is.null(pos_info)) {
          tags$span(class = "pos-badge",
            style = paste0("background:", pos_info$bg_color,
                           "; color:", pos_info$text_color, ";"),
            display_pos)
        } else {
          tags$span(class = "pos-badge",
            style = "background:#F1F5F9; color:#64748B;",
            display_pos)
        }
      })
    }

    rows <- lapply(seq_len(nrow(data)), function(i) {
      p <- data[i, ]

      pos_badges <- make_pos_badges(
        if ("positions"        %in% names(p)) p$positions        else NA,
        if ("primary_position" %in% names(p)) p$primary_position else NA
      )

      mlb_cell <- if (!is.na(p$mlb_team) && nchar(p$mlb_team) > 0) {
        tags$span(class = "mlb-badge", p$mlb_team)
      } else {
        tags$span(class = "em-dash", "\u2014")
      }

      derby_cell <- if (!is.na(p$derby_team)) {
        tinfo <- CONFIG$teams$team_info[[p$derby_team]]
        if (!is.null(tinfo)) {
          tags$span(class = "player-derby-badge",
            style = paste0("background:", tinfo$primary_color,
                           "; color:", tinfo$text_color, ";"),
            tinfo$abbr)
        } else {
          tags$span(style = "color:#64748B; font-size:11px;", p$derby_team)
        }
      } else {
        tags$span(class = "em-dash", "\u2014")
      }

      tags$tr(
        tags$td(mlb_cell),
        tags$td(style = "text-align:left; font-weight:700;", p$player_name),
        tags$td(class = "pos-badges-cell", pos_badges),
        tags$td(derby_cell),
        tags$td(style = "font-weight:700; font-size:15px; color:#1E293B; text-align:center;",
                p$total_hr),
        tags$td(p$past7_hr),
        tags$td(p$past30_hr)
      )
    })

    header <- tags$tr(
      style = "background:#F1F5F9;",
      tags$th("MLB"),
      tags$th(style = "text-align:left;", "PLAYER"),
      tags$th("POS"),
      tags$th("TEAM"),
      tags$th("HR"),
      tags$th("7D"),
      tags$th("30D")
    )

    div(class = "roster-table-wrapper", style = "padding: 0 10px;",
      tags$table(class = "roster-table players-table",
        tags$thead(header),
        tags$tbody(rows)
      )
    )
  })
  outputOptions(output, "players_tab_view", suspendWhenHidden = FALSE)

  # =========================================================================
  # Cumulative HR graph — tiny y-offset per team so tied lines separate
  # =========================================================================
  output$hr_graph <- renderPlot({
    cum <- cumulative_hr_data()
    if (is.null(cum) || nrow(cum) == 0) return(NULL)
    g <- build_graph_base(cum, leaderboard_data())

    n <- nlevels(g$cum_data$team_name)
    offsets <- data.frame(
      team_name = levels(g$cum_data$team_name),
      y_offset  = seq(0, 0.6, length.out = n)
    )

    jittered <- g$cum_data %>%
      left_join(offsets, by = "team_name") %>%
      mutate(y = cumulative_hr + y_offset)

    latest <- jittered %>%
      group_by(team_name) %>% filter(date == max(date)) %>% slice(1) %>% ungroup()

    ggplot(jittered, aes(x = date, y = y, color = team_name)) +
      geom_line(linewidth = 1.5) +
      geom_text(data = latest, aes(label = team_label),
                hjust = -0.15, size = 2.8, show.legend = FALSE) +
      labs(x = "Date", y = "Total HRs",
           caption = "Lines slightly offset — all values are whole numbers") +
      scale_color_manual(values = g$team_colors) +
      scale_x_date(limits = c(g$opening_day - 1, NA), expand = expansion(mult = c(0.02, 0.22))) +
      scale_y_continuous(labels = function(x) as.integer(round(x))) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.caption    = element_text(color = "#94A3B8", size = 8, hjust = 0.5)
      )
  })

}
