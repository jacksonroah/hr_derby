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
  
  # Updated team_totals function with improved column sizing
  
  # # LEADERBOARD FOR Top 5, team total, and total HR distance. 
  # output$team_totals <- renderDT({
  #   if (is.null(leaderboard_data())) return(NULL)
  #   
  #   # Get colors from configuration
  #   team_colors <- get_team_colors()
  #   text_colors <- get_team_text_colors()
  #   highlight_colors <- get_team_colors(for_graph = TRUE)
  #   
  #   # Check if we have distance data
  #   has_distance <- "total_distance" %in% colnames(leaderboard_data()$leaderboard)
  #   
  #   # Prepare leaderboard data - SORT BY TOP 5 TOTAL
  #   leaderboard <- leaderboard_data()$leaderboard %>%
  #     arrange(desc(top_n_total))  # Sort by top_n_total descending
  #   
  #   # Add rank column based on top_n_total
  #   leaderboard$Rank <- 1:nrow(leaderboard)
  #   
  #   # Rename columns and select what we need based on available data
  #   if (has_distance) {
  #     leaderboard <- leaderboard %>%
  #       rename(
  #         Team = team_name,
  #         `TOP 5` = top_n_total,
  #         Total = all_players_total,
  #         `Total Distance (ft)` = total_distance
  #       
  #       ) %>%
  #       select(Rank, Team, `TOP 5`, Total, `Total Distance (ft)`)
  #   } else {
  #     leaderboard <- leaderboard %>%
  #       rename(
  #         Team = team_name,
  #         `TOP 5` = top_n_total,
  #         Total = all_players_total
  #       ) %>%
  #       select(Rank, Team, `TOP 5`, Total)
  #   }
  #   
  #   # Create the datatable with styling
  #   dt <- datatable(
  #     leaderboard, 
  #     options = list(
  #       dom = 't',  # Just show the table, no pagination or search
  #       pageLength = -1,  # Show all rows
  #       autoWidth = FALSE,  # We'll control widths with CSS
  #       ordering = FALSE,  # Disable sorting
  #       searching = FALSE,  # Disable search
  #       paging = FALSE,     # Disable pagination
  #       scrollX = FALSE     # Disable horizontal scrolling
  #     ), 
  #     rownames = FALSE, 
  #     escape = FALSE
  #   ) 
  #   
  #   # Apply team colors
  #   dt <- dt %>% formatStyle(
  #     columns = names(leaderboard),
  #     backgroundColor = styleEqual(
  #       leaderboard$Team, 
  #       sapply(leaderboard$Team, function(team) team_colors[[team]])
  #     ),
  #     color = styleEqual(
  #       leaderboard$Team, 
  #       sapply(leaderboard$Team, function(team) text_colors[[team]])
  #     ),
  #     textAlign = 'center'
  #   )
  #   
  #   # Highlight TOP 5 column with lighter team colors
  #   dt <- dt %>% formatStyle(
  #     columns = "TOP 5",
  #     backgroundColor = styleEqual(
  #       leaderboard$Team,
  #       sapply(leaderboard$Team, function(team) {
  #         # Get highlight color (already defined in config)
  #         highlight_color <- highlight_colors[team]
  #         return(highlight_color)
  #       })
  #     ),
  #     fontWeight = 'bold',
  #     fontSize = '120%'
  #   )
  #   
  #   return(dt)
  # })
  
  # Updated team_totals function with TOP 5, Total Distance, and Avg Distance
  
  output$team_totals <- renderDT({
    if (is.null(leaderboard_data())) return(NULL)
    
    # Get colors from configuration
    team_colors <- get_team_colors()
    text_colors <- get_team_text_colors()
    highlight_colors <- get_team_colors(for_graph = TRUE)
    
    # Check if we have distance data
    has_distance <- "total_distance" %in% colnames(leaderboard_data()$leaderboard)
    
    # Prepare leaderboard data - SORT BY TOP 5 TOTAL
    leaderboard <- leaderboard_data()$leaderboard %>%
      arrange(desc(top_n_total))  # Sort by top_n_total descending
    
    # Add rank column based on top_n_total
    leaderboard$Rank <- 1:nrow(leaderboard)
    
    # Rename columns and select what we need based on available data
    if (has_distance) {
      # Calculate average distance
      leaderboard <- leaderboard %>%
        mutate(avg_distance = round(total_distance / all_players_total, 1)) %>%
        rename(
          Team = team_name,
          `TOP 5` = top_n_total,
          `Total Dinger Distance (ft)` = total_distance,
          `Avg (ft)` = avg_distance
        ) %>%
        select(Rank, Team, `TOP 5`, `Total Dinger Distance (ft)`, `Avg (ft)`)
    } else {
      leaderboard <- leaderboard %>%
        rename(
          Team = team_name,
          `TOP 5` = top_n_total
        ) %>%
        select(Rank, Team, `TOP 5`)
    }
    
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
      escape = FALSE
    ) 
    
    # Apply team colors
    dt <- dt %>% formatStyle(
      columns = names(leaderboard),
      backgroundColor = styleEqual(
        leaderboard$Team, 
        sapply(leaderboard$Team, function(team) team_colors[[team]])
      ),
      color = styleEqual(
        leaderboard$Team, 
        sapply(leaderboard$Team, function(team) text_colors[[team]])
      ),
      textAlign = 'center'
    )
    
    # Highlight TOP 5 column with lighter team colors
    dt <- dt %>% formatStyle(
      columns = "TOP 5",
      backgroundColor = styleEqual(
        leaderboard$Team,
        sapply(leaderboard$Team, function(team) {
          # Get highlight color (already defined in config)
          highlight_color <- highlight_colors[team]
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
  
  # Replace the createPlayerTable function in your server.R with this version:
  
  createPlayerTable <- function(team, team_data) {
    team_info <- CONFIG$teams$team_info[[team]]
    counting_players <- get_counting_players()
    bottom_display <- get_bottom_display_count()
    
    # Get text color from team config
    text_color <- team_info$text_color
    
    # Create header
    header <- tags$tr(
      style = paste0(
        "background-color:", team_info$primary_color, "; color:", 
        ifelse(team_info$text_color == "black", "black", "white"), ";"
      ),
      tags$th("Rank"),
      tags$th("Player Name"),
      tags$th("HR")
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
        tags$td(row$rank),
        tags$td(style = name_style, row$player_name),
        tags$td(row$total_home_runs)
      )
    })
    
    # Create table
    tags$div(
      class = "team-box",
      style = "margin-bottom: 20px;",
      tags$h3(team),
      tags$table(
        class = "team-table",
        style = "border-collapse:collapse; width:100%; margin-bottom:15px;",
        tags$thead(header),
        tags$tbody(rows)
      )
    )
  }
  
  # Updated version of the player_stats output function 
  # This fixes the NA row issue by ensuring we have exact team data
  
  output$player_stats <- renderUI({
    if (is.null(total_hr_per_player())) return(NULL)
    
    # Get teams and order them by leaderboard position
    leaderboard_order <- leaderboard_data()$leaderboard %>%
      arrange(desc(top_n_total)) %>%
      pull(team_name)
    
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
        stringsAsFactors = FALSE
      )
      
      # Combine player data with total rows - explicitly use only necessary columns
      # This prevents the extra NA row from appearing
      combined_data <- rbind(
        team_data[, c("rank", "player_name", "total_home_runs", "team_name")],
        total_rows[, c("rank", "player_name", "total_home_runs", "team_name")]
      )
      
      # Create the table
      createPlayerTable(team, combined_data)
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
        title = paste0("Cumulative Top ", get_counting_players(), " Home Runs by Team (Since Opening Day)"),
        x = "Date", 
        y = "Cumulative Home Runs"
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