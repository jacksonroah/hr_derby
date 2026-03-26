# server.R - 2026 HR Derby server logic
library(shiny)
library(ggplot2)
library(dplyr)
library(httr)
library(jsonlite)

source("config.R")
source("data_processing.R")
options(viewer = NULL, shiny.launch.browser = TRUE)

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

server <- function(input, output, session) {

  status  <- reactiveVal("Initializing...")
  hr_data <- reactiveVal(NULL)

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
      processed   <- process_data(data, drafted_players)

      if (!is.null(processed) && nrow(processed) > 0) {
        hr_data(processed)
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
    req(hr_data())
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

    lb <- lb_result$leaderboard %>%
      arrange(desc(team_total)) %>%
      mutate(rank = row_number())

    recent <- recent_team_stats()
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

    total_hr_all   <- sum(lb$team_total, na.rm = TRUE)
    max_team_total <- max(lb$team_total, na.rm = TRUE)
    if (max_team_total == 0) max_team_total <- 1  # avoid division by zero

    total_feet_all <- if (!is.null(recent) && "avg_distance" %in% names(lb) &&
                          "hr_count_with_distance" %in% names(lb)) {
      total_ft <- sum(lb$avg_distance * lb$hr_count_with_distance, na.rm = TRUE)
      if (total_ft > 0) paste0(format(round(total_ft), big.mark = ","), " ft") else ""
    } else ""

    # Header bar
    header_bar <- div(class = "standings-header-bar",
      div(class = "header-left",
        tags$span(class = "season-label", "SEASON 2026"),
        tags$span(class = "standings-title", "2026 HR DERBY STANDINGS")
      ),
      div(class = "header-totals",
        tags$span(class = "total-hr-number", total_hr_all),
        tags$span(class = "hr-unit", "HR"),
        if (nchar(total_feet_all) > 0)
          div(class = "total-feet-line", total_feet_all)
      )
    )

    # Column headers
    col_headers <- div(class = "standings-col-headers",
      div(class = "sh-rk", "RK"),
      div(class = "sh-team", "TEAM"),
      div(class = "sh-div"),
      div(class = "sh-total", "TOTAL HR"),
      div(class = "sh-day", "TODAY"),
      div(class = "sh-week", "7-DAY"),
      div(class = "sh-month", "30-DAY")
    )

    # Standings layout variant: "A" = color left/white right, "B" = white left/color right
    variant <- CONFIG$game$standings_variant

    # Team rows
    team_rows <- lapply(seq_len(nrow(lb)), function(i) {
      row   <- lb[i, ]
      team  <- row$team_name
      tinfo <- CONFIG$teams$team_info[[team]]
      squad <- if (!is.null(tinfo$squad_name) && nchar(tinfo$squad_name) > 0) tinfo$squad_name else NULL

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
        # Right side
        div(class = "sr-right", style = right_style,
          div(class = "sr-total", style = right_text_style,
            tags$span(style = "font-size:22px; font-weight:700;", row$team_total),
            tags$span(class = "sr-hr-unit", "HR")
          ),
          div(class = "sr-day", style = right_text_style, day_content),
          div(class = "sr-week", style = right_text_style, row$past7_hr),
          div(class = "sr-month", style = right_text_style, row$past30_hr)
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
          tags$span(class = "team-name-bold", style = "font-size:14px;", tinfo$display_name),
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

        # Sort
        if (cur_sort == "position") {
          team_players <- team_players %>%
            mutate(pos_order = match(position, POSITION_ORDER)) %>%
            arrange(pos_order, desc(total_home_runs)) %>%
            select(-pos_order)
        } else {
          team_players <- team_players %>%
            arrange(desc(total_home_runs))
        }

        sub_header_bg <- hex_to_rgba(tinfo$primary_color, 0.28)
        row_bg_uniform <- hex_to_rgba(tinfo$primary_color, 0.08)

        sub_header <- tags$tr(
          style = paste0("background:", sub_header_bg, ";"),
          tags$th("RK"),
          tags$th("PLAYER"),
          tags$th("POS"),
          tags$th("TOTAL HR"),
          tags$th(style = "width:46px;", "POS RK"),
          tags$th("DAY"),
          tags$th("7D"),
          tags$th("30D")
        )

        player_rows <- lapply(seq_len(nrow(team_players)), function(j) {
          p <- team_players[j, ]

          pos_info <- CONFIG$positions[[p$position]]
          pos_badge <- if (!is.null(pos_info)) {
            tags$span(class = "pos-badge",
              style = paste0(
                "background:", pos_info$bg_color,
                "; color:", pos_info$text_color, ";"
              ),
              p$position)
          } else {
            tags$span(class = "pos-badge", style = "background:#F1F5F9; color:#64748B;",
              p$position)
          }

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

          is_top <- (j == 1)
          day_cell <- if (p$today_hr > 0) {
            tags$span(class = "day-pill-sm", paste0("+", p$today_hr))
          } else {
            tags$span(class = "em-dash", "\u2014")
          }

          hr_style <- if (is_top) paste0("color:", tinfo$primary_color, "; font-weight:700;") else ""

          tags$tr(style = paste0("background:", row_bg_uniform, ";"),
            tags$td(style = "color:#94A3B8; font-size:11px;", j),
            tags$td(style = "text-align:left;", p$player_name),
            tags$td(pos_badge),
            tags$td(style = hr_style, p$total_home_runs),
            tags$td(style = "width:46px;", pos_rk_pill),
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
  # Cumulative HR graph (all players)
  # =========================================================================
  output$hr_graph <- renderPlot({
    cum <- cumulative_hr_data()
    if (is.null(cum) || nrow(cum) == 0) {
      opening_day <- get_opening_day()
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = paste0("Season starts ", format(opening_day, "%B %d, %Y")),
                   size = 5, color = "#94A3B8") +
          theme_void()
      )
    }

    lb_result <- leaderboard_data()
    standings  <- if (!is.null(lb_result)) {
      lb_result$leaderboard %>% arrange(desc(team_total))
    } else {
      data.frame(team_name = get_team_names())
    }

    team_colors <- get_team_colors(for_graph = TRUE)
    opening_day <- get_opening_day()

    cum_data <- cum %>%
      mutate(team_name = factor(team_name, levels = standings$team_name))

    ggplot(cum_data, aes(x = date, y = cumulative_hr, color = team_name)) +
      geom_line(linewidth = 1.5) +
      labs(
        title = "Cumulative Home Runs by Team",
        x     = "Date",
        y     = "Total Home Runs"
      ) +
      scale_color_manual(values = team_colors) +
      scale_x_date(limits = c(opening_day, NA)) +
      theme_minimal() +
      theme(
        legend.title    = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 13)
      )
  })
}
