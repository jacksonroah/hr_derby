# ui.R - 2026 HR Derby UI
library(shiny)
source("config.R")

ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport",
              content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"),
    tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
    tags$meta(name = "mobile-web-app-capable", content = "yes"),

    # -----------------------------------------------------------------------
    # CSS
    # -----------------------------------------------------------------------
    tags$style(HTML("
      * { box-sizing: border-box; }

      body {
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
        background: #F1F5F9;
        margin: 0;
        padding: 0;
      }

      .container-fluid { padding: 0 !important; }

      /* App wrapper — mobile-first, centered on desktop */
      .derby-app {
        max-width: 480px;
        margin: 0 auto;
        background: #F1F5F9;
        min-height: 100vh;
      }

      /* ---- Top Banner -------------------------------------------------- */
      .top-banner {
        display: flex;
        align-items: center;
        justify-content: space-between;
        background: #0F172A;
        padding: 10px 14px;
        position: sticky;
        top: 0;
        z-index: 200;
      }

      .banner-logo {
        font-size: 19px;
        font-weight: 800;
        color: white;
        letter-spacing: -0.5px;
        white-space: nowrap;
      }

      .banner-nav {
        display: flex;
        gap: 2px;
        flex-shrink: 0;
      }

      .nav-pill {
        padding: 4px 8px;
        border-radius: 20px;
        font-size: 11px;
        font-weight: 600;
        color: rgba(255,255,255,0.55);
        cursor: pointer;
        border: 1px solid transparent;
        background: transparent;
        outline: none;
        white-space: nowrap;
        transition: background 0.15s, color 0.15s;
      }

      .nav-pill:hover { color: rgba(255,255,255,0.85); }
      .nav-pill.active { background: white; color: #0F172A; }

      /* ---- Tab panes ---------------------------------------------------- */
      .tab-pane { padding: 0 0 24px 0; }

      /* ---- Standings ---------------------------------------------------- */
      .standings-container {
        background: white;
        border-radius: 0 0 12px 12px;
        overflow: hidden;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        margin-bottom: 16px;
      }

      .standings-header-bar {
        background: #0F172A;
        color: white;
        padding: 12px 14px 10px;
        display: flex;
        justify-content: space-between;
        align-items: flex-end;
      }

      .season-label {
        display: block;
        font-size: 9px;
        color: rgba(255,255,255,0.45);
        text-transform: uppercase;
        letter-spacing: 0.12em;
        margin-bottom: 3px;
      }

      .standings-title {
        font-size: 21px;
        font-weight: 800;
        letter-spacing: -0.3px;
        line-height: 1.15;
      }

      .header-left {
        flex: 1;
        min-width: 0;
        padding-right: 10px;
      }

      .header-totals { text-align: right; }

      .total-hr-number {
        font-size: 30px;
        font-weight: 800;
        line-height: 1;
      }

      .hr-unit {
        font-size: 13px;
        color: rgba(255,255,255,0.55);
        margin-left: 3px;
      }

      .total-feet-line {
        font-size: 10px;
        color: rgba(255,255,255,0.4);
        margin-top: 2px;
      }

      /* Column header row */
      .standings-col-headers {
        display: flex;
        align-items: center;
        background: #F8FAFC;
        padding: 7px 14px;
        border-bottom: 1px solid #E2E8F0;
        font-size: 10px;
        font-weight: 700;
        color: #94A3B8;
        text-transform: uppercase;
        letter-spacing: 0.08em;
      }

      /* Team row */
      .standings-team-row {
        display: flex;
        align-items: stretch;
        border-bottom: 1px solid rgba(0,0,0,0.05);
      }

      .standings-team-row:last-child { border-bottom: none; }

      /* Rank circle */
      .rank-circle {
        width: 28px;
        height: 28px;
        border-radius: 50%;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 13px;
        font-weight: 700;
        color: white;
        flex-shrink: 0;
      }

      /* Team name section */
      .sr-team {
        display: flex;
        align-items: flex-start;
        flex-direction: column;
        flex: 1;
        min-width: 0;
        padding: 0 8px;
        gap: 1px;
        overflow: hidden;
      }

      .team-name-bold {
        font-weight: 600;
        font-size: 18px;
        white-space: nowrap;
        letter-spacing: -0.3px;
      }

      .sr-name-row {
        display: flex;
        align-items: baseline;
        gap: 5px;
        white-space: nowrap;
      }

      .team-abbr-inline {
        font-family: 'Courier New', monospace;
        font-size: 10px;
        opacity: 0.55;
        letter-spacing: 0.05em;
        font-weight: 500;
      }

      .team-squad-name {
        font-size: 12px;
        opacity: 0.7;
        white-space: nowrap;
        font-weight: 600;
      }

      .team-abbr-mono {
        font-family: 'Courier New', monospace;
        font-size: 10px;
        color: #94A3B8;
        letter-spacing: 0.05em;
      }

      /* Divider column — change background to adjust divider color */
      .col-divider-bar {
        width: 3px;
        min-height: 32px;
        background: black;
        flex-shrink: 0;
        border-radius: 2px;
        margin: 0 10px;
        align-self: stretch;
      }

      /* Split-side layout for standings rows */
      .sr-left {
        display: flex;
        align-items: center;
        padding: 11px 4px 11px 14px;
        flex: 1;
        min-width: 0;
      }

      .sr-right {
        display: flex;
        align-items: center;
        padding: 11px 14px 11px 0;
        flex-shrink: 0;
        width: 165px;
      }

      /* Stats columns — narrowed ~12px total to give name column more room */
      .sr-total {
        width: 56px;
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        flex-shrink: 0;
      }

      .sr-total-nums {
        display: flex;
        align-items: baseline;
        gap: 3px;
      }

      .sr-hr-unit {
        font-size: 11px;
        color: #94A3B8;
        font-weight: 400;
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
      }

      .sr-day {
        width: 37px;
        text-align: center;
        flex-shrink: 0;
      }

      .sr-week,
      .sr-month {
        width: 36px;
        text-align: center;
        font-size: 14px;
        font-weight: 600;
        color: #475569;
        flex-shrink: 0;
      }

      /* Column header widths — mirror stat columns */
      .sh-rk    { width: 28px; text-align: center; flex-shrink: 0; }
      .sh-team  { flex: 1; padding: 0 10px; }
      .sh-div   { width: 23px; flex-shrink: 0; }
      .sh-total { width: 56px; text-align: center; flex-shrink: 0; color: #3B82F6; }
      .sh-day   { width: 37px; text-align: center; flex-shrink: 0; }
      .sh-week  { width: 36px; text-align: center; flex-shrink: 0; }
      .sh-month { width: 36px; text-align: center; flex-shrink: 0; }

      /* Today pill */
      .day-pill {
        display: inline-block;
        background: #16A34A;
        color: white;
        font-size: 11px;
        font-weight: 700;
        padding: 2px 6px;
        border-radius: 10px;
        white-space: nowrap;
      }

      .em-dash { color: #CBD5E1; font-size: 14px; }

      /* Leader row tint */
      .leader-row { box-shadow: inset 3px 0 0 currentColor; }

      /* ---- Graph section ------------------------------------------------ */
      .graph-section {
        background: white;
        border-radius: 12px;
        padding: 14px 10px 10px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        margin: 0 0 16px;
        overflow-x: auto;
      }

      .section-title {
        font-size: 13px;
        font-weight: 700;
        color: #64748B;
        text-align: center;
        margin: 0 0 10px;
        text-transform: uppercase;
        letter-spacing: 0.08em;
      }

      /* ---- Roster Cards ------------------------------------------------- */
      .rosters-controls {
        display: flex;
        align-items: center;
        justify-content: space-between;
        flex-wrap: wrap;
        gap: 8px;
        padding: 12px 14px 8px;
        background: #F1F5F9;
      }

      .controls-left { }
      .controls-title {
        font-size: 14px;
        font-weight: 700;
        color: #1E293B;
        display: block;
      }
      .controls-hint {
        font-size: 11px;
        color: #94A3B8;
        display: block;
      }

      .controls-right {
        display: flex;
        align-items: center;
        gap: 6px;
        flex-wrap: wrap;
      }

      /* Shiny action buttons — strip default styling */
      .sort-pill {
        padding: 5px 10px !important;
        border-radius: 20px !important;
        font-size: 11px !important;
        font-weight: 600 !important;
        border: 1px solid #CBD5E1 !important;
        background: white !important;
        color: #64748B !important;
        cursor: pointer !important;
        outline: none !important;
        box-shadow: none !important;
      }

      .sort-pill.active-sort {
        background: #0F172A !important;
        color: white !important;
        border-color: #0F172A !important;
      }

      .text-btn {
        padding: 4px 8px !important;
        font-size: 11px !important;
        font-weight: 600 !important;
        color: #3B82F6 !important;
        background: transparent !important;
        border: none !important;
        box-shadow: none !important;
        cursor: pointer !important;
        outline: none !important;
      }

      .ctrl-sep {
        color: #CBD5E1;
        font-size: 13px;
        user-select: none;
      }

      /* Cards container */
      .cards-container { padding: 4px 0; }

      .team-card {
        margin: 6px 10px;
        border-radius: 10px;
        overflow: hidden;
        box-shadow: 0 1px 4px rgba(0,0,0,0.08);
        background: white;
      }

      /* Card header */
      .card-header {
        display: flex;
        align-items: center;
        padding: 10px 12px;
        cursor: pointer;
        user-select: none;
        -webkit-tap-highlight-color: transparent;
      }

      .card-header:active { opacity: 0.85; }

      .ch-rank-name {
        display: flex;
        align-items: center;
        flex: 1;
        min-width: 0;
        gap: 6px;
      }

      .rank-circle-sm {
        width: 24px;
        height: 24px;
        border-radius: 50%;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 11px;
        font-weight: 700;
        color: white;
        flex-shrink: 0;
      }

      .ch-stats {
        display: flex;
        align-items: flex-end;
        gap: 10px;
        flex-shrink: 0;
        margin-right: 6px;
      }

      /* Each stat wrapped in a column with label below */
      .ch-stat-col {
        display: flex;
        flex-direction: column;
        align-items: center;
        gap: 1px;
      }

      .ch-label {
        font-size: 9px;
        color: #94A3B8;
        text-transform: uppercase;
        letter-spacing: 0.05em;
        white-space: nowrap;
      }

      .ch-total {
        display: flex;
        align-items: baseline;
        gap: 2px;
        font-size: 20px;
        font-weight: 700;
        white-space: nowrap;
      }

      .ch-hr-unit {
        font-size: 9px;
        color: #94A3B8;
        font-weight: 400;
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
        letter-spacing: 0.03em;
      }

      .ch-day {
        font-size: 11px;
        text-align: center;
        min-height: 20px;
        display: flex;
        align-items: center;
        justify-content: center;
      }

      .ch-week,
      .ch-month {
        text-align: center;
        font-size: 14px;
        font-weight: 600;
        color: #475569;
        min-width: 24px;
      }

      .ch-chevron {
        font-size: 15px;
        color: #94A3B8;
        flex-shrink: 0;
        width: 18px;
        text-align: center;
        transition: transform 0.2s;
      }

      /* Roster table */
      .roster-table-wrapper { overflow-x: auto; }

      .roster-table {
        width: 100%;
        border-collapse: collapse;
        font-size: 13px;
      }

      .roster-table th {
        padding: 6px 2px;
        font-size: 9px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.04em;
        color: #1E293B;
        text-align: center;
        border-bottom: 1px solid rgba(0,0,0,0.10);
      }

      .roster-table th:nth-child(1) { width: 18px; padding: 6px 1px; }
      .roster-table th:nth-child(4) { text-align: left; padding-left: 6px; }

      .roster-table td {
        padding: 6px 2px;
        text-align: center;
        border-bottom: 1px solid #F1F5F9;
        font-size: 13px;
        color: #1E293B;
      }

      .roster-table td:nth-child(1) { width: 18px; padding: 6px 1px; font-size: 11px; }
      .roster-table td:nth-child(4) { text-align: left; padding-left: 6px; font-size: 13px; }

      /* Bench player row — greyed out with separator line */
      .bench-row { border-top: 2px solid #475569; opacity: 0.45; }
      .roster-table tr:last-child td { border-bottom: none; }

      /* Position badge */
      .pos-badge {
        display: inline-block;
        padding: 2px 6px;
        border-radius: 8px;
        font-size: 10px;
        font-weight: 700;
        letter-spacing: 0.04em;
        white-space: nowrap;
      }

      .mlb-badge {
        display: inline-block;
        padding: 2px 5px;
        border-radius: 3px;
        background: #E2E8F0;
        color: #475569;
        font-size: 10px;
        font-weight: 700;
        letter-spacing: 0.03em;
        white-space: nowrap;
      }

      .tie-note {
        font-size: 9px;
        color: #94A3B8;
        font-weight: 500;
        text-align: center;
        white-space: nowrap;
        line-height: 1.2;
      }

      /* Players controls — vertical pill stacks */
      .players-controls-right {
        align-items: flex-start;
      }

      .pill-col {
        display: flex;
        flex-direction: column;
        gap: 4px;
      }

      /* Position filter bubble row */
      .pos-filter-row {
        display: flex;
        flex-wrap: wrap;
        gap: 3px;
        margin-top: 2px;
        max-width: 130px;
      }

      .pos-filter-btn {
        padding: 3px 7px !important;
        border-radius: 8px !important;
        font-size: 10px !important;
        font-weight: 700 !important;
        border: none !important;
        cursor: pointer !important;
        outline: none !important;
        box-shadow: none !important;
        opacity: 0.4;
        transition: opacity 0.15s;
      }

      .pos-filter-btn.active-pos-filter {
        opacity: 1;
        box-shadow: 0 0 0 2px rgba(0,0,0,0.18) !important;
      }

      /* Players table — column sizing: MLB, PLAYER, POS, TEAM, HR, 7D, 30D */
      .players-table th:nth-child(1),
      .players-table td:nth-child(1) { width: 38px; padding: 6px 2px; }
      .players-table th:nth-child(2),
      .players-table td:nth-child(2) { text-align: left !important; padding-left: 6px !important;
                                        white-space: nowrap; min-width: 130px; }
      .players-table th:nth-child(3),
      .players-table td:nth-child(3) { text-align: center !important; padding-left: 2px !important; }
      .players-table th:nth-child(4),
      .players-table td:nth-child(4) { width: 38px; text-align: center !important; padding-left: 2px !important; }
      .players-table th:nth-child(5),
      .players-table td:nth-child(5) { width: 46px; text-align: center; }

      /* Players tab — derby team badge */
      .player-derby-badge {
        display: inline-block;
        padding: 2px 6px;
        border-radius: 4px;
        font-size: 10px;
        font-weight: 700;
        letter-spacing: 0.04em;
        white-space: nowrap;
      }

      /* Players tab — multiple pos badges in one cell */
      .pos-badges-cell {
        white-space: nowrap;
      }

      /* Position rank — pill shape to fit ordinals */
      .pos-rank-circle {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        padding: 2px 5px;
        border-radius: 10px;
        font-size: 10px;
        font-weight: 700;
        white-space: nowrap;
      }

      /* Today pill small */
      .day-pill-sm {
        display: inline-block;
        background: #16A34A;
        color: white;
        font-size: 10px;
        font-weight: 700;
        padding: 1px 5px;
        border-radius: 8px;
      }

      /* ---- By Position toggle ------------------------------------------ */
      .pos-view-controls {
        display: flex;
        padding: 14px 14px 6px;
      }

      .pos-view-pill {
        padding: 6px 16px !important;
        font-size: 12px !important;
        font-weight: 600 !important;
        border: 1px solid #CBD5E1 !important;
        background: white !important;
        color: #64748B !important;
        cursor: pointer !important;
        outline: none !important;
        box-shadow: none !important;
        border-radius: 0 !important;
      }
      .pos-view-pill:first-child {
        border-radius: 20px 0 0 20px !important;
      }
      .pos-view-pill:last-child {
        border-radius: 0 20px 20px 0 !important;
        border-left: none !important;
      }
      .pos-view-pill.active-pos {
        background: #0F172A !important;
        color: white !important;
        border-color: #0F172A !important;
      }

      /* ---- By Position grid -------------------------------------------- */
      .pos-grid-container { padding: 6px 10px 24px; }

      .pos-grid-table {
        width: 100%;
        border-collapse: collapse;
        background: white;
        border-radius: 10px;
        overflow: hidden;
        box-shadow: 0 1px 6px rgba(0,0,0,0.09);
      }

      .pg-team-header {
        width: 82px;
        min-width: 82px;
        padding: 10px 4px;
        background: #C4CAD4;
        border-bottom: 2px solid #E2E8F0;
        font-size: 10px;
        font-weight: 700;
        color: #475569;
        text-transform: uppercase;
        letter-spacing: 0.08em;
        text-align: center;
      }

      .pg-pos-header {
        padding: 10px 4px;
        text-align: center;
        background: #C4CAD4;
        border-bottom: 2px solid #E2E8F0;
      }

      .pg-team-row { border-bottom: 1px solid #F1F5F9; }
      .pg-team-row:last-child { border-bottom: none; }

      .pg-team-cell {
        width: 82px;
        min-width: 82px;
        padding: 24px 4px;
        white-space: normal;
        word-break: break-word;
        text-align: center;
        border-right: 2px solid #E2E8F0;
      }

      .pg-rank-cell {
        padding: 24px 2px;
        text-align: center;
        border-left: 1px solid white;
      }

      .pg-rank-num {
        display: inline-block;
        line-height: 1;
      }

      /* Loading state */
      .loading-msg {
        text-align: center;
        padding: 40px 20px;
        color: #94A3B8;
        font-size: 14px;
      }

      /* ---- Initial loading overlay ------------------------------------- */
      #app-loading {
        position: fixed;
        inset: 0;
        background: #0F172A;
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        z-index: 10000;
        transition: opacity 0.35s ease;
      }

      .load-spinner {
        width: 42px;
        height: 42px;
        border: 3px solid rgba(255,255,255,0.15);
        border-top-color: white;
        border-radius: 50%;
        animation: ld-spin 0.75s linear infinite;
      }

      .load-text {
        color: rgba(255,255,255,0.55);
        font-size: 13px;
        font-weight: 600;
        margin-top: 14px;
        letter-spacing: 0.06em;
        text-transform: uppercase;
      }

      @keyframes ld-spin { to { transform: rotate(360deg); } }

      @keyframes goat-flash {
        0%   { background-color: #FF0000; }
        14%  { background-color: #FF7F00; }
        28%  { background-color: #FFD700; }
        42%  { background-color: #00CC44; }
        57%  { background-color: #0088FF; }
        71%  { background-color: #8B00FF; }
        85%  { background-color: #FF1493; }
        100% { background-color: #FF0000; }
      }
      body.goat-mode { animation: goat-flash 0.4s linear infinite; }
      body.goat-mode * { color: white !important; text-shadow: 0 0 6px rgba(0,0,0,0.6); }

      /* Suppress Shiny's recalculating dim/spinner so UI never flashes on poll */
      .recalculating { opacity: 1 !important; transition: none !important; }
      .shiny-busy-indicator { display: none !important; }

      /* Disconnected overlay — centered, clean */
      #shiny-disconnected-overlay {
        display: flex !important;
        align-items: center;
        justify-content: center;
        background: rgba(15, 23, 42, 0.75) !important;
        position: fixed;
        inset: 0;
        z-index: 9999;
      }

      #shiny-disconnected-overlay span {
        background: white;
        color: #0F172A;
        font-size: 15px;
        font-weight: 700;
        padding: 20px 28px;
        border-radius: 12px;
        box-shadow: 0 8px 32px rgba(0,0,0,0.25);
        text-align: center;
      }
    ")),

    # -----------------------------------------------------------------------
    # JavaScript — tab switching + card toggles
    # -----------------------------------------------------------------------
    tags$script(HTML("
      function switchTab(tab) {
        ['standings', 'rosters', 'players', 'position'].forEach(function(t) {
          document.getElementById('tab-' + t).style.display = (t === tab) ? 'block' : 'none';
          var pill = document.getElementById('nav-' + t);
          if (pill) { pill.classList.toggle('active', t === tab); }
        });
      }

      function toggleCard(team) {
        Shiny.setInputValue('toggle_card',
          {team: team, ts: Date.now()},
          {priority: 'event'});
      }

      // Update position view toggle from server message
      Shiny.addCustomMessageHandler('updatePosButtons', function(msg) {
        var rankBtn = document.getElementById('pos_show_rank');
        var hrBtn   = document.getElementById('pos_show_hr');
        if (!rankBtn || !hrBtn) return;
        if (msg.active === 'rank') {
          rankBtn.classList.add('active-pos');
          hrBtn.classList.remove('active-pos');
        } else {
          hrBtn.classList.add('active-pos');
          rankBtn.classList.remove('active-pos');
        }
      });

      // Update sort pill active state from server message
      Shiny.addCustomMessageHandler('updateSortButtons', function(msg) {
        ['sort_by_hr', 'sort_by_position'].forEach(function(id) {
          var el = document.getElementById(id);
          if (!el) return;
          if ((id === 'sort_by_hr' && msg.active === 'hr') ||
              (id === 'sort_by_position' && msg.active === 'position')) {
            el.classList.add('active-sort');
          } else {
            el.classList.remove('active-sort');
          }
        });
      });

      // Players tab — HR sort button
      Shiny.addCustomMessageHandler('updatePlayersButtons', function(msg) {
        var hrBtn = document.getElementById('players_sort_hr');
        if (hrBtn) hrBtn.classList.toggle('active-sort', msg.active === 'hr');
      });

      // Players tab — position filter bubbles
      Shiny.addCustomMessageHandler('updatePlayersPosFilter', function(msg) {
        ['OF','1B','2B','3B','SS','C','UTL'].forEach(function(pos) {
          var el = document.getElementById('players_pos_' + pos);
          if (!el) return;
          el.classList.toggle('active-pos-filter', msg.active === pos);
        });
      });

      Shiny.addCustomMessageHandler('updatePlayersFilter', function(msg) {
        ['players_filter_available', 'players_filter_all'].forEach(function(id) {
          var el = document.getElementById(id);
          if (!el) return;
          if ((id === 'players_filter_available' && msg.active === 'available') ||
              (id === 'players_filter_all' && msg.active === 'all')) {
            el.classList.add('active-sort');
          } else {
            el.classList.remove('active-sort');
          }
        });
      });

      Shiny.addCustomMessageHandler('goatMode', function(msg) {
        if (msg.active) {
          document.body.classList.add('goat-mode');
        } else {
          document.body.classList.remove('goat-mode');
        }
      });

      // Hide loading overlay once the standings table first renders
      $(document).on('shiny:value', function(e) {
        if (e.name === 'league_standings_table') {
          var el = document.getElementById('app-loading');
          if (el) {
            el.style.opacity = '0';
            setTimeout(function() { el.style.display = 'none'; }, 380);
          }
        }
      });
    "))
  ), # end tags$head

  # =========================================================================
  # Loading overlay — visible immediately, fades out once standings render
  # =========================================================================
  div(id = "app-loading",
    div(class = "load-spinner"),
    div(class = "load-text", "HR Derbski")
  ),

  # =========================================================================
  # App shell
  # =========================================================================
  div(class = "derby-app",

    # ---- Top banner --------------------------------------------------------
    div(class = "top-banner",
      div(class = "banner-logo", "HR Derbski"),
      div(class = "banner-nav",
        tags$button(class = "nav-pill active", id = "nav-standings",
                    onclick = "switchTab('standings')", "League"),
        tags$button(class = "nav-pill", id = "nav-rosters",
                    onclick = "switchTab('rosters')", "Rosters"),
        tags$button(class = "nav-pill", id = "nav-players",
                    onclick = "switchTab('players')", "Players"),
        tags$button(class = "nav-pill", id = "nav-position",
                    onclick = "switchTab('position')", "Rankings")
      )
    ),

    # ---- Tab 1: League Standings -------------------------------------------
    div(id = "tab-standings", class = "tab-pane",
      uiOutput("league_standings_table"),
      div(class = "graph-section",
        tags$h3(class = "section-title", "Cumulative HRs Since Opening Day"),
        plotOutput("hr_graph", height = "380px")
      )
    ),

    # ---- Tab 2: Team Rosters -----------------------------------------------
    div(id = "tab-rosters", class = "tab-pane", style = "display:none;",
      div(class = "rosters-controls",
        div(class = "controls-left",
          tags$span(class = "controls-title", "All Team Rosters"),
          tags$span(class = "controls-hint", "Tap any team to expand")
        ),
        div(class = "controls-right",
          actionButton("sort_by_hr",       "Sort by HR",       class = "sort-pill active-sort"),
          actionButton("sort_by_position", "Sort by Position", class = "sort-pill"),
          tags$span(class = "ctrl-sep", "|"),
          actionButton("expand_all",   "Expand All",   class = "text-btn"),
          actionButton("collapse_all", "Collapse All", class = "text-btn")
        )
      ),
      uiOutput("team_roster_cards")
    ),

    # ---- Tab 3: Players (all MLB players w/ HRs + position eligibility) ----
    div(id = "tab-players", class = "tab-pane", style = "display:none;",
      div(class = "rosters-controls",
        div(class = "controls-left",
          tags$span(class = "controls-title", "Players"),
          tags$span(class = "controls-hint", "Sorted by home runs")
        ),
        div(class = "controls-right players-controls-right",
          div(class = "pill-col",
            actionButton("players_sort_hr", "Sort by Total HR", class = "sort-pill active-sort"),
            div(class = "pos-filter-row",
              tagList(lapply(c("OF","1B","2B","3B","SS","C","UTL"), function(pos) {
                pi  <- CONFIG$positions[[pos]]
                lbl <- if (pos == "UTL") "UT" else pos
                sty <- if (!is.null(pi))
                  paste0("background:", pi$bg_color, " !important; color:", pi$text_color, " !important;")
                else
                  "background:#F9FAFB !important; color:#6B7280 !important;"
                actionButton(paste0("players_pos_", pos), lbl,
                             class = "pos-filter-btn", style = sty)
              }))
            )
          ),
          tags$span(class = "ctrl-sep", "|"),
          div(class = "pill-col",
            actionButton("players_filter_all",       "All Players",    class = "sort-pill active-sort"),
            actionButton("players_filter_available", "Available Only", class = "sort-pill")
          )
        )
      ),
      uiOutput("players_tab_view")
    ),

    # ---- Tab 4: By Position ------------------------------------------------
    div(id = "tab-position", class = "tab-pane", style = "display:none;",
      div(class = "pos-view-controls",
        actionButton("pos_show_rank", "Position Rank", class = "pos-view-pill active-pos"),
        actionButton("pos_show_hr",   "HR Total",      class = "pos-view-pill")
      ),
      uiOutput("position_grid")
    )

  ) # end .derby-app
)
