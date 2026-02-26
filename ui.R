# ui.R - User interface for the Home Run Derby app
library(shiny)
library(DT)

# Source configuration
source("config.R")

# Define UI elements
ui <- fluidPage(
  # CSS Styling
  tags$head(
    tags$style(HTML("
      /* Global styles */
      body {
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
      }
      
      /* Title styling */
      .main-title {
        text-align: center;
        margin-bottom: 20px;
      }
      
      /* Section headers */
      h2 {
        text-align: center;
        border-bottom: 2px solid #eee;
        padding-bottom: 5px;
        margin-top: 25px;
      }
      
      /* Leaderboard styling */
      .leaderboard-container {
        width: 100%;
        margin-bottom: 20px;
      }
      
      /* DataTable styles */
      .dataTables_wrapper {
        width: 100% !important;
        margin: 0 auto;
      }
      
      .dataTable {
        width: 100% !important;
        margin: 0 auto;
      }
      
      
      /* Column widths for main leaderboard */
      .dataTable th:nth-child(1), /* Rank */
      .dataTable td:nth-child(1) {
        width: 40px !important;
        text-align: center !important;
      }
      
      .dataTable th:nth-child(2), /* Team */
      .dataTable td:nth-child(2) {
        width: 80px !important;
        text-align: center !important;
      }
      
      .dataTable th:nth-child(3), /* TOP 5 */
      .dataTable td:nth-child(3) {
        width: 60px !important;
        text-align: center !important;
      }
      
      .dataTable th:nth-child(4), /* Total */
      .dataTable td:nth-child(4) {
        width: 60px !important;
        text-align: center !important;
      }
      
      .dataTable th:nth-child(5), /* Distance */
      .dataTable td:nth-child(5) {
        width: 80px !important;
        text-align: center !important;
      }
      
      /* Team grid layout */
      .team-grid {
        display: grid;
        grid-template-columns: repeat(2, 1fr);
        gap: 15px;
        width: 100%;
      }
      
      /* Team box styling */
      .team-box {
        border: 1px solid #ddd;
        border-radius: 5px;
        padding: 15px;
        margin-bottom: 15px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
        background-color: #fafafa;
      }
      
      .team-box h3 {
        text-align: center;
        margin-top: 0;
        margin-bottom: 10px;
        padding-bottom: 5px;
        border-bottom: 1px solid #eee;
        font-size: 18px;
      }
      
      /* Team table styling */
      .team-table {
        width: 100%;
        border-collapse: collapse;
      }
      
      .team-table th, 
      .team-table td {
        padding: 8px;
        text-align: center !important;
        border: 1px solid #ddd;
      }
      
      .team-table th {
        font-weight: bold;
      }
      
      /* Team table column widths */
      .team-table th:nth-child(1), 
      .team-table td:nth-child(1) {
        width: 20%;
      }
      
      .team-table th:nth-child(2), 
      .team-table td:nth-child(2) {
        width: 55%;
      }
      
      .team-table th:nth-child(3), 
      .team-table td:nth-child(3) {
        width: 25%;
      }
      
      /* Graph container */
      .graph-container {
        width: 100%;
        margin-top: 20px;
      }
      
      /* For smaller screens */
      @media (max-width: 767px) {
        .team-grid {
          grid-template-columns: 1fr;
        }
      }
      
          # Add/modify these CSS rules in your ui.R file's tags$style section
    
    /* Leaderboard table header - bigger text */
    .dataTable th {
      font-size: 20px !important;
      font-weight: bold !important;
      padding: 12px 8px !important;
      text-align: center !important;
    }
    
    /* Team names in leaderboard - bigger and bolder */
    .dataTable td:nth-child(2) {
      font-size: 22px !important;
      font-weight: bold !important;
      text-align: center !important;
    }
    
    /* Other leaderboard cells - ensure proper sizing and alignment */
    .dataTable td {
      font-size: 18px !important;
      text-align: center !important;
      padding: 10px 8px !important;
    }
    
    /* Team names in individual team sections - bigger */
    .team-box h3 {
      font-size: 24px !important;
      font-weight: bold !important;
      text-align: center !important;
      margin-top: 0;
      margin-bottom: 15px;
      padding-bottom: 8px;
      border-bottom: 1px solid #eee;
    }
    
    /* Team table headers */
    .team-table th {
      font-size: 18px !important;
      font-weight: bold !important;
      padding: 8px !important;
      text-align: center !important;
    }
    
    /* Team table cells */
    .team-table td {
      font-size: 16px !important;
      padding: 8px !important;
      text-align: center !important;
    }
    "))
  ),
  
  # Team standings - current leaderboard
  h2("The 2025 HR Derbski Leaderboard:"),
  div(class = "leaderboard-container",
      DTOutput("team_totals")
  ),
  
  # Team details - show players and home runs
  h2("Individual Squad Standings"),
  div(class = "team-details-container",
      uiOutput("player_stats")
  ),
  
  # Graph showing progress over time
  h2("Home Run Tracker Since Opening Day"),
  div(class = "graph-container",
      plotOutput("hr_graph", height = "500px")
  ),
  
  # Footer with last update info
  div(class = "footer",
      hr(),
      p("Data updates automatically every minute.", style = "text-align: center; color: #666; font-size: 0.9em;")
  )
)