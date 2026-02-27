# ui.R - User interface for the Home Run Derby app
library(shiny)
library(DT)

# Source configuration
source("config.R")

# Define UI elements
ui <- fluidPage(
  # CSS Styling
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"),
    tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
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
      
      h6 {
        text-align: center;
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
      
      /* Main leaderboard column widths */
      .dataTable th:nth-child(1), /* Rank */
      .dataTable td:nth-child(1) {
        width: 30px !important;
        text-align: center !important;
        padding: 8px 2px !important;
      }
      
      .dataTable td:nth-child(1) {
        font-size: 20px !important; /* Rank values larger */
      }
      
      .dataTable th:nth-child(2), /* Team */
      .dataTable td:nth-child(2) {
        width: 90px !important;
        text-align: center !important;
        font-size: 22px !important;
        font-weight: bold !important;
      }
      
      /* Make sure the TOP 5 heading text is centered */
      .dataTable th:nth-child(3) {
        text-align: center !important;
        font-size: 16px !important;
        font-weight: bold !important;
        padding: 8px 2px !important;
        width: 50px !important;
        border-right: 2px solid black !important;
      }
      
      .dataTable td:nth-child(3) { /* TOP 5 values */
        width: 50px !important;
        text-align: center !important;
        border-right: 2px solid black !important;
        font-size: 20px !important; /* Slightly reduced but still larger */
      }
      
      .dataTable th:nth-child(4), /* Longest */
      .dataTable td:nth-child(4) {
        width: 90px !important;
        text-align: center !important;
      }
      
      .dataTable th:nth-child(5), /* Today */
      .dataTable td:nth-child(5) {
        width: 50px !important;
        text-align: center !important;
        font-size: 25px;
      }
      
      .dataTable th:nth-child(6), /* Past 7 */
      .dataTable td:nth-child(6) {
        width: 50px !important;
        text-align: center !important;
      }
      
      /* Other headers (ensure all headers are centered) */
      .dataTable th {
        text-align: center !important;
        font-size: 16px !important;
        font-weight: bold !important;
        padding: 8px 2px !important;
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
        margin-bottom: 15px;
        padding-bottom: 8px;
        border-bottom: 1px solid #eee;
        font-size: 24px !important;
        font-weight: bold !important;
      }
      
      /* Team table styling */
      .team-table {
        width: 100%;
        border-collapse: collapse;
      }
      
      .team-table th, 
      .team-table td {
        border: 1px solid #ddd;
        text-align: center !important;
      }
      
      /* Team table column widths - UPDATED */
      .team-table th:nth-child(1), 
      .team-table td:nth-child(1) {
        width: 6% !important;
        padding: 4px 2px !important;
      }
      
      .team-table td:nth-child(1) {
        font-size: 17px !important; /* Rank values larger */
      }
      
      /* Player column styling */
      .team-table th:nth-child(2) {
        width: 45% !important;
        padding: 4px 2px !important;
        text-align: center !important;
        font-size: 17px !important; /* Player header larger */
      }
      
      .team-table td:nth-child(2) {
        width: 45% !important;
        padding: 4px 2px !important;
        text-align: center !important;
        font-size: 16px !important; /* Player names larger */
      }
      
      .team-table th:nth-child(3), 
      .team-table td:nth-child(3) {
        width: 15% !important;
        padding: 4px 2px !important;
        border-right: 2px solid black !important;
      }
      
      .team-table th:nth-child(4), 
      .team-table td:nth-child(4) {
        width: 14% !important;
        padding: 4px 2px !important;
      }
      
      .team-table th:nth-child(5), 
      .team-table td:nth-child(5) {
        width: 20% !important;
        padding: 4px 2px !important;
      }
      
      /* Make the total HR values bold in team tables */
      .team-table tr:nth-last-child(-n+2) td:nth-child(3) {
        font-weight: bold !important;
      }
      
      /* Font sizes */
      .dataTable td {
        font-size: 18px !important;
        padding: 10px 8px !important;
      }
      
      .team-table th {
        font-size: 16px !important;
        font-weight: bold !important;
        padding: 4px 2px !important;
      }
      
      .team-table td {
        font-size: 15px !important;
        padding: 4px 2px !important;
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
        
        /* Additional mobile optimizations */
        .dataTable th, 
        .team-table th {
          font-size: 13px !important;
          padding: 3px 1px !important;
        }
        
        .dataTable td {
          font-size: 14px !important;
          padding: 3px 1px !important;
        }
        
        .team-table td {
          font-size: 17px !important;
          padding: 3px 1px !important;
        }
        
        .team-box h3 {
          font-size: 20px !important;
          margin-bottom: 8px !important;
        }
      }
    "))
  ),
  
  # Team standings - current leaderboard
  h2("The 2025 HR Derbski Leaderboard:"),
  
  h6("(giancarlo stanton for mvp)"),
  
  div(class = "leaderboard-container",
      DTOutput("team_totals")
  ),
  
  # Team details - show players and home runs
  h2("Squad Standings"),
  div(class = "team-details-container",
      uiOutput("player_stats")
  ),
  
  # Graph showing progress over time
  h2("Team Top 5 Total Since Opening Day"),
  div(class = "graph-container",
      plotOutput("hr_graph", height = "500px")
  ),
  
  # Footer with last update info
  div(class = "footer",
      hr(),
      p("Data updates automatically every minute (except for Derek)", style = "text-align: center; color: #666; font-size: 0.9em;")
  )
)