# server.R - Server logic for the Home Run Derby app
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(httr)
library(jsonlite)

# Source the configuration and data processing files
source("config.R")
source("data_processing.R")

observe({
  cat("Roster loaded:", nrow(drafted_players), "players\n")
  if (nrow(drafted_players) > 0) {
    cat("Sample roster names:", paste(head(drafted_players$player_name, 5), collapse=", "), "\n")
  }
})

server <- function(input, output, session) {
  # Initialize status tracking
  status <- reactiveVal("Initializing...")
  
  # Reactive polling function to fetch data
  # Reactive polling function to fetch data
  hr_data <- reactivePoll(
    get_poll_interval(), 
    session,
    checkFunc = function() {
      tryCatch({
        status("Checking for updates...")
        response <- httr::GET(get_api_url())
        status("Ready")
        return(response$headers$date)
      }, error = function(e) {
        status(paste("Error checking API:", e$message))
        return(Sys.time())
      })
    },
    valueFunc = function() {
      tryCatch({
        # Check if we should use sample data
        if (CONFIG$api$use_sample_data) {
          status("Using sample data mode...")
          sample_data <- generate_sample_hr_data()
          return(sample_data)
        }
        
        status("Fetching data from API...")
        api_url <- get_api_url()
        
        # Get the raw data
        raw_response <- httr::GET(api_url)
        
        if (http_status(raw_response)$category != "Success") {
          status(paste("API Error:", http_status(raw_response)$message))
          status("No data available - showing empty stats")
          return(NULL)
        }
        
        # Parse the response
        raw_content <- content(raw_response, "text", encoding = "UTF-8")
        data <- jsonlite::fromJSON(raw_content)
        
        status(paste("Received", nrow(data), "home run records from API"))
        
        # Process the data using our custom function
        processed_data <- process_data(data, drafted_players)
        
        # If processing failed or returned no matching data, show empty stats
        if (is.null(processed_data) || nrow(processed_data) == 0) {
          status("No home runs matched to roster players. Check player names in roster.csv match API exactly.")
          return(NULL)
        }
        
        status(paste("Data processed successfully:", nrow(processed_data), "home runs matched to roster"))
        return(processed_data)
      }, error = function(e) {
        status(paste("Error fetching data:", e$message))
        print(paste("Detailed error:", e))
        status("Error occurred - showing empty stats")
        return(NULL)
      })
    }
  )
  
  # Output the current status
  output$status_message <- renderText({
    status()
  })
  
  # Calculate total home runs per player
  total_hr_per_player <- reactive({
    # Use the updated calculate_total_hr_per_player function
    calculate_total_hr_per_player(hr_data())
  })
  
  # Create leaderboard
  leaderboard_data <- reactive({
    if (is.null(total_hr_per_player())) return(NULL)
    create_leaderboard(total_hr_per_player(), hr_data())
  })
  
  # Updated team_totals function with TOP 5, Total Distance, and Avg Distance
  
  # Modified team_totals output to use display names
  # Modified team_totals output to use display names
  output$team_totals <- renderDT({
    if (is.null(leaderboard_data())) return(NULL)
    
    # Get colors and display names from configuration
    team_colors <- get_team_colors()
    text_colors <- get_team_text_colors()
    highlight_colors <- get_team_colors(for_graph = TRUE)
    display_names <- get_team_display_names()
    
    # Calculate recent statistics
    recent_stats <- calculate_recent_hr_stats(hr_data())
    
    # Prepare leaderboard data - SORT BY TOP 5 TOTAL
    leaderboard <- leaderboard_data()$leaderboard %>%
      arrange(desc(top_n_total))  # Sort by top_n_total descending
    
    # Add rank column based on top_n_total
    leaderboard$Rank <- 1:nrow(leaderboard)
    
    # Join with recent stats
    if (!is.null(recent_stats)) {
      # Join with recent stats
      leaderboard <- leaderboard %>%
        left_join(recent_stats, by = "team_name") %>%
        mutate(
          today_hr = ifelse(is.na(today_hr), 0, today_hr),
          past7_hr = ifelse(is.na(past7_hr), 0, past7_hr),
          longest_hr = ifelse(is.na(longest_hr), 0, round(as.numeric(longest_hr), 0)),
          avg_distance = ifelse(is.na(avg_distance), 0, avg_distance),
          last_name = ifelse(is.na(last_name), "", last_name)
        )
    } else {
      # If no recent stats, add empty columns
      leaderboard$today_hr <- 0
      leaderboard$past7_hr <- 0
      leaderboard$longest_hr <- 0
      leaderboard$last_name <- ""
      leaderboard$avg_distance <- 0
    }
    
    # Format the distance column with average distance above longest HR
    leaderboard <- leaderboard %>%
      mutate(
        formatted_distance = case_when(
          # If we have both average and longest distance data
          avg_distance > 0 & longest_hr > 0 & last_name != "" ~ 
            paste0(avg_distance, " ft<br><small>(", last_name, "--", longest_hr, ")</small>"),
          # If we only have longest HR data
          longest_hr > 0 & last_name != "" ~ 
            paste0("<br><small>(", last_name, " - ", longest_hr, " ft)</small>"),
          # If we only have average distance
          avg_distance > 0 ~ 
            paste0("Avg: ", avg_distance, " ft"),
          # Default case
          TRUE ~ "No data"
        )
      )
    
    # Apply display names for the main leaderboard
    leaderboard <- leaderboard %>%
      mutate(
        display_team_name = sapply(team_name, function(team) {
          return(display_names[[team]])
        })
      )
    
    # Rename columns and select what we need
    leaderboard <- leaderboard %>%
      rename(
        Team = display_team_name,  # Use display name instead of team_name
        `TOP 5` = top_n_total,
        `Avg Distance (Longest)` = formatted_distance,
        `24 Hour` = today_hr,
        `7 Days` = past7_hr
      ) %>%
      select(Rank, Team, `TOP 5`, `Avg Distance (Longest)`, `24 Hour`, `7 Days`)
    
    # Create the datatable with styling
    dt <- datatable(
      leaderboard, 
      options = list(
        dom = 't',  # Just show the table, no pagination or search
        pageLength = -1,  # Show all rows
        autoWidth = FALSE,  # We'll control widths with CSS
        ordering = FALSE,  # Disable sorting
        searching = FALSE,  # Disable search
        paging = FALSE,     # Disable pagination
        scrollX = FALSE     # Disable horizontal scrolling
      ), 
      rownames = FALSE, 
      escape = FALSE  # Important to allow HTML in the table
    ) 
    
    # We need to map back to original team names for colors since they're keyed by original names
    # Create a mapping from display names back to original team names
    display_to_original <- setNames(names(display_names), unlist(display_names))
    
    # Apply team colors (mapping display names back to original for color lookup)
    dt <- dt %>% formatStyle(
      columns = names(leaderboard),
      backgroundColor = styleEqual(
        leaderboard$Team, 
        sapply(leaderboard$Team, function(display_team) {
          original_team <- display_to_original[[display_team]]
          return(team_colors[[original_team]])
        })
      ),
      color = styleEqual(
        leaderboard$Team, 
        sapply(leaderboard$Team, function(display_team) {
          original_team <- display_to_original[[display_team]]
          return(text_colors[[original_team]])
        })
      ),
      textAlign = 'center'
    )
    
    # Highlight TOP 5 column with lighter team colors
    dt <- dt %>% formatStyle(
      columns = "TOP 5",
      backgroundColor = styleEqual(
        leaderboard$Team,
        sapply(leaderboard$Team, function(display_team) {
          original_team <- display_to_original[[display_team]]
          highlight_color <- highlight_colors[original_team]
          return(highlight_color)
        })
      ),
      fontWeight = 'bold',
      fontSize = '120%'
    )
    
    return(dt)
  })
  
  # Make sure the leaderboard is always rendered
  outputOptions(output, "team_totals", suspendWhenHidden = FALSE)
  
  # Modified createPlayerTable function to use squad names
  createPlayerTable <- function(team, team_data, recent_player_data = NULL) {
    team_info <- CONFIG$teams$team_info[[team]]
    counting_players <- get_counting_players()
    bottom_display <- get_bottom_display_count()
    squad_names <- get_team_squad_names()
    
    # Get text color from team config
    text_color <- team_info$text_color
    
    # Merge with recent player data if available
    if (!is.null(recent_player_data)) {
      # Filter for just this team's players
      team_recent <- recent_player_data %>%
        filter(team_name == team)
      
      # Join with team_data
      if (nrow(team_recent) > 0) {
        team_data <- left_join(
          team_data, 
          team_recent, 
          by = c("team_name", "player_name")
        ) %>%
          mutate(
            today_hr = ifelse(is.na(today_hr), 0, today_hr),
            past7_hr = ifelse(is.na(past7_hr), 0, past7_hr)
          )
      } else {
        team_data$today_hr <- 0
        team_data$past7_hr <- 0
      }
    } else {
      team_data$today_hr <- 0
      team_data$past7_hr <- 0
    }
    
    # Create header - make Rank smaller to accommodate more columns
    header <- tags$tr(
      style = paste0(
        "background-color:", team_info$primary_color, "; color:", 
        ifelse(team_info$text_color == "black", "black", "white"), ";"
      ),
      tags$th("R", style = "width:8% !important;"),
      tags$th("Player", style = "width:50% !important;"),
      tags$th("TOTAL", style = "width:14% !important;"),
      tags$th("24 Hour", style = "width:14% !important;"),
      tags$th("7 Days", style = "width:14% !important;")
    )
    
    # Create rows
    rows <- lapply(1:nrow(team_data), function(i) {
      row <- team_data[i, ]
      rank_num <- as.integer(row$rank)
      
      # Determine row style based on rank
      row_style <- ""
      if (!is.na(rank_num)) {
        if (rank_num <= counting_players) {
          # Top N players - highlighted with team color
          bg_color <- adjustcolor(team_info$primary_color, alpha.f = 0.5)
          row_style <- paste0("background-color:", bg_color, ";")
          
          # Set player name color to match team text color
          name_style <- paste0("font-weight: bold;")
        } else if (rank_num > counting_players && rank_num <= counting_players + bottom_display) {
          # Bottom display players - grayed out
          row_style <- "background-color:#f0f0f0; color:#a0a0a0;"
          name_style <- "color:#a0a0a0;"
        } else {
          # Regular players
          name_style <- paste0("color:", text_color, ";")
        }
      } else {
        # Total rows
        row_style <- paste0(
          "background-color:", team_info$primary_color, 
          "; color:", ifelse(team_info$text_color == "black", "black", "white"), 
          "; font-weight:bold;"
        )
        name_style <- "" # No additional styling needed
      }
      
      tags$tr(
        style = row_style,
        tags$td(style = "text-align:center; padding: 4px 2px !important;", row$rank),
        tags$td(style = paste0(name_style, " text-align:left; padding: 4px 2px !important;"), row$player_name),
        tags$td(style = "text-align:center; padding: 4px 2px !important;", row$total_home_runs),
        tags$td(style = "text-align:center; padding: 4px 2px !important;", row$today_hr),
        tags$td(style = "text-align:center; padding: 4px 2px !important;", row$past7_hr)
      )
    })
    
    # Create table using squad name instead of team name
    squad_name <- squad_names[[team]]
    
    tags$div(
      class = "team-box",
      style = "margin-bottom: 20px;",
      tags$h3(squad_name),  # Use squad name here instead of team
      tags$table(
        class = "team-table",
        style = "border-collapse:collapse; width:100%; margin-bottom:15px;",
        tags$thead(header),
        tags$tbody(rows)
      )
    )
  }
  
  # Update the player_stats output
  output$player_stats <- renderUI({
    if (is.null(total_hr_per_player())) return(NULL)
    
    # Get teams and order them by leaderboard position
    leaderboard_order <- leaderboard_data()$leaderboard %>%
      arrange(desc(top_n_total)) %>%
      pull(team_name)
    
    # Calculate recent stats for players
    recent_player_stats <- calculate_player_recent_stats(hr_data())
    
    counting_players <- get_counting_players()
    
    # Create tables in leaderboard order
    team_tables <- lapply(leaderboard_order, function(team) {
      # Filter and prepare data for this team
      team_data <- total_hr_per_player() %>%
        filter(team_name == team) %>%
        arrange(desc(total_home_runs)) %>%
        mutate(
          rank = as.character(row_number()),
          player_name = as.character(player_name)
        )
      
      # Calculate totals
      top_n_total <- team_data %>%
        filter(row_number() <= counting_players) %>%
        summarise(total = sum(total_home_runs)) %>%
        pull(total)
      
      all_total <- sum(team_data$total_home_runs)
      
      # Create total rows with explicit team_name matching
      total_rows <- data.frame(
        rank = c(NA_character_, NA_character_),
        player_name = c(paste0("Top ", counting_players, " Total"), "Team Total"),
        total_home_runs = c(top_n_total, all_total),
        team_name = c(team, team),
        today_hr = c(0, 0),  # Add these columns to match
        past7_hr = c(0, 0),  # Add these columns to match
        stringsAsFactors = FALSE
      )
      
      # Combine player data with total rows - explicitly use only necessary columns
      combined_data <- rbind(
        team_data[, c("rank", "player_name", "total_home_runs", "team_name")],
        total_rows[, c("rank", "player_name", "total_home_runs", "team_name")]
      )
      
      # Create the table
      createPlayerTable(team, combined_data, recent_player_stats)
    })
    
    # Arrange in a responsive grid
    tags$div(
      class = "team-grid",
      team_tables
    )
  })
  # Prepare data for cumulative HR graph
  cumulative_hr_data <- reactive({
    if (is.null(hr_data())) return(NULL)
    prepare_cumulative_data(hr_data())
  })
  
  # Render home run graph over time
  output$hr_graph <- renderPlot({
    # Check if we have data
    if (is.null(cumulative_hr_data())) return(NULL)
    
    # Get current team standings for legend order
    standings <- leaderboard_data()$leaderboard %>%
      arrange(desc(top_n_total))
    
    # Get colors from config
    team_colors <- get_team_colors(for_graph = TRUE)
    
    # Reorder data based on current standings
    cumulative_hr_data_reordered <- cumulative_hr_data() %>%
      mutate(team_name = factor(team_name, levels = standings$team_name))
    
    # Get opening day date
    opening_day <- get_opening_day()
    
    # Create the plot
    ggplot(
      cumulative_hr_data_reordered, 
      aes(x = date, y = cumulative_hr, color = team_name)
    ) +
      geom_line(size = 1.5) +
      labs(
        title = paste0("Cumulative Top ", get_counting_players(), " Home Runs by Team"),
        x = "Date", 
        y = "Total Home Runs (Top 5)"
      ) +
      scale_color_manual(values = team_colors) +
      scale_x_date(limits = c(opening_day, NA)) +  # Start x-axis at opening day
      theme_minimal() +
      theme(
        legend.title = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  })
}