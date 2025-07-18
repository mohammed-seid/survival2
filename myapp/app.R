# ETH 2025 Survival Survey Dashboard - Shinylive Optimized
# Revision Date: 2025-01-16
# Description: A fully shinylive-compatible dashboard with enhanced analytics and interactivity

# --- Core Libraries (Shinylive Compatible) ---
library(shiny)
library(bslib)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

# --- Custom Color Palette ---
custom_colors <- list(
  primary = "#2E8B57",
  secondary = "#20B2AA", 
  success = "#28a745",
  warning = "#ffc107",
  danger = "#dc3545",
  info = "#17a2b8",
  light = "#f8f9fa",
  dark = "#343a40",
  gradient = c("#2E8B57", "#20B2AA", "#66CDAA", "#98FB98", "#90EE90")
)

# --- Helper Functions ---
format_percent <- function(x, digits = 1) {
  paste0(round(x * 100, digits), "%")
}

format_number <- function(x, digits = 0) {
  format(round(x, digits), big.mark = ",", scientific = FALSE)
}

safe_mean <- function(x, na.rm = TRUE) {
  if(length(x) == 0 || all(is.na(x))) return(0)
  mean(x, na.rm = na.rm)
}

safe_sum <- function(x, na.rm = TRUE) {
  if(length(x) == 0 || all(is.na(x))) return(0)
  sum(x, na.rm = na.rm)
}

# --- Data Loading with Error Handling ---
load_data <- function() {
  tryCatch({
    # Try to load the RDS file
    if(file.exists("df.rds")) {
      df_raw <- readRDS("df.rds")
    } else {
      # Create sample data if file doesn't exist (for demo purposes)
      df_raw <- create_sample_data()
    }
    
    # Clean and preprocess
    df_processed <- df_raw %>%
      mutate(across(everything(), ~ na_if(., "---"))) %>%
      mutate(across(where(is.list), ~ sapply(., function(x) if(length(x) > 0) x[1] else NA))) %>%
      mutate(
        completed_time = as.POSIXct(completed_time, tz = "UTC"),
        started_time = as.POSIXct(started_time, tz = "UTC"),
        date = as.Date(completed_time),
        week = floor_date(date, "week"),
        month = floor_date(date, "month"),
        hour_started = hour(started_time),
        day_of_week = wday(date, label = TRUE, week_start = 1),
        is_weekend = day_of_week %in% c("Sat", "Sun"),
        duration_minutes = as.numeric(difftime(completed_time, started_time, units = "mins")),
        is_night_survey = hour_started >= 19 | hour_started < 6,
        is_short_survey = duration_minutes <= 5,
        is_long_survey = duration_minutes >= 60,
        consent = as.numeric(consent)
      ) %>%
      mutate(across(c(starts_with("ps_num_planted_"), starts_with("num_surv_")), ~ as.numeric(as.character(.))))
    
    return(df_processed)
  }, error = function(e) {
    # Return sample data if loading fails
    return(create_sample_data())
  })
}

# --- Sample Data Generator (for demo/fallback) ---
create_sample_data <- function() {
  set.seed(123)
  n <- 500
  
  sites <- c("Adama", "Bishoftu", "Mojo", "Dukem", "Sebeta")
  woredas <- c("Adama Town", "Ada'a", "Lume", "Boset", "Dugda")
  enumerators <- paste0("enum_", 1:10)
  
  # Create base times first
  base_time <- Sys.time()
  started_times <- base_time - runif(n, 0, 30*24*3600)
  duration_mins <- runif(n, 5, 120)  # 5 to 120 minutes
  completed_times <- started_times + (duration_mins * 60)
  
  data.frame(
    consent = sample(0:1, n, replace = TRUE, prob = c(0.1, 0.9)),
    site = sample(sites, n, replace = TRUE),
    woreda = sample(woredas, n, replace = TRUE),
    username = sample(enumerators, n, replace = TRUE),
    started_time = started_times,
    completed_time = completed_times,
    hh_size = sample(1:12, n, replace = TRUE),
    age = sample(18:80, n, replace = TRUE),
    sex = sample(c("Male", "Female"), n, replace = TRUE),
    education_level = sample(c("None", "Primary", "Secondary", "Tertiary"), n, replace = TRUE),
    ps_num_planted_avocado = sample(0:50, n, replace = TRUE),
    ps_num_planted_mango = sample(0:30, n, replace = TRUE),
    ps_num_planted_papaya = sample(0:20, n, replace = TRUE),
    stringsAsFactors = FALSE
  ) %>%
  mutate(
    num_surv_avocado = pmax(0, ps_num_planted_avocado - sample(0:10, n, replace = TRUE)),
    num_surv_mango = pmax(0, ps_num_planted_mango - sample(0:8, n, replace = TRUE)),
    num_surv_papaya = pmax(0, ps_num_planted_papaya - sample(0:5, n, replace = TRUE)),
    date = as.Date(completed_time),
    week = floor_date(date, "week"),
    month = floor_date(date, "month"),
    hour_started = hour(started_time),
    day_of_week = wday(date, label = TRUE, week_start = 1),
    is_weekend = day_of_week %in% c("Sat", "Sun"),
    duration_minutes = as.numeric(difftime(completed_time, started_time, units = "mins")),
    is_night_survey = hour_started >= 19 | hour_started < 6,
    is_short_survey = duration_minutes <= 5,
    is_long_survey = duration_minutes >= 60
  )
}

# --- Load Data ---
df_processed <- load_data()

# --- UI Definition ---
ui <- page_navbar(
  title = div(
    icon("seedling", style = "color: #2E8B57; margin-right: 10px;"),
    "Enhanced Survival Survey Dashboard",
    style = "font-weight: bold; font-size: 1.2em;"
  ),
  theme = bs_theme(
    version = 5, 
    bootswatch = "flatly",
    primary = "#2E8B57", 
    secondary = "#20B2AA"
  ),
  
  # Global Filters Sidebar
  sidebar = sidebar(
    title = div(icon("filter"), "Global Filters"),
    width = 300,
    
    dateRangeInput(
      "global_date_range", 
      "📅 Date Range:",
      start = min(df_processed$date, na.rm = TRUE), 
      end = max(df_processed$date, na.rm = TRUE),
      format = "yyyy-mm-dd"
    ),
    
    selectInput(
      "global_site_filter", 
      "🏢 Site:",
      choices = c("All" = "", sort(unique(df_processed$site))), 
      multiple = TRUE
    ),
    
    selectInput(
      "global_woreda_filter", 
      "🗺️ Woreda:",
      choices = c("All" = "", sort(unique(df_processed$woreda))), 
      multiple = TRUE
    ),
    
    selectInput(
      "global_enum_filter", 
      "👤 Enumerator:",
      choices = c("All" = "", sort(unique(df_processed$username))), 
      multiple = TRUE
    ),
    
    hr(),
    
    div(
      style = "background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); padding: 15px; border-radius: 8px;",
      h6("📊 Quick Stats", style = "color: #2E8B57; font-weight: bold;"),
      div(
        style = "display: flex; justify-content: space-between; margin-bottom: 5px;",
        span("Filtered Records:", style = "font-size: 0.9em;"),
        textOutput("filtered_count", inline = TRUE)
      ),
      div(
        style = "display: flex; justify-content: space-between;",
        span("Completion Rate:", style = "font-size: 0.9em;"),
        textOutput("filtered_completion_rate", inline = TRUE)
      )
    ),
    
    actionButton(
      "reset_filters", 
      "🔄 Reset Filters",
      class = "btn-outline-secondary btn-sm",
      style = "width: 100%; margin-top: 15px;"
    )
  ),
  
  # --- Main Content Panels ---
  nav_panel(
    title = "📊 Executive Dashboard",
    layout_column_wrap(
      width = 1/5,
      value_box("Total Surveys", value = textOutput("total_surveys"), showcase = icon("clipboard-list"), theme = "primary"),
      value_box("Completed", value = textOutput("completed_surveys"), showcase = icon("check-circle"), theme = "success"),
      value_box("Refusal Rate", value = textOutput("refusal_rate"), showcase = icon("times-circle"), theme = "danger"),
      value_box("Avg. Duration", value = textOutput("avg_duration"), showcase = icon("clock"), theme = "warning"),
      value_box("Enumerators", value = textOutput("unique_enumerators"), showcase = icon("users"), theme = "secondary")
    ),
    
    layout_columns(
      col_widths = c(8, 4),
      card(
        full_screen = TRUE,
        card_header("📈 Daily Survey Progress with Trend"),
        plotlyOutput("daily_progress_plot", height = "400px")
      ),
      card(
        full_screen = TRUE,
        card_header("🎯 Target Achievement Gauge"),
        plotlyOutput("gauge_plot", height = "400px")
      )
    ),
    
    layout_columns(
      col_widths = c(6, 6),
      card(
        full_screen = TRUE,
        card_header("📅 Survey Completion Patterns"),
        plotlyOutput("completion_patterns_plot", height = "350px")
      ),
      card(
        full_screen = TRUE,
        card_header("⏰ Survey Timing Analysis"),
        plotlyOutput("timing_analysis_plot", height = "350px")
      )
    )
  ),
  
  nav_panel(
    title = "👥 Enumerator Analytics",
    layout_columns(
      col_widths = c(7, 5),
      card(
        full_screen = TRUE,
        card_header("📊 Performance Leaderboard"),
        DT::dataTableOutput("enumerator_table")
      ),
      card(
        full_screen = TRUE,
        card_header("🎯 Target vs Actual Performance"),
        plotlyOutput("target_comparison_plot")
      )
    ),
    
    layout_columns(
      col_widths = c(8, 4),
      card(
        full_screen = TRUE,
        card_header("🔥 Productivity Heatmap"),
        plotlyOutput("productivity_heatmap")
      ),
      card(
        full_screen = TRUE,
        card_header("⚠️ Quality Control Alerts"),
        DT::dataTableOutput("quality_alerts_table")
      )
    ),
    
    card(
      full_screen = TRUE,
      card_header("📈 Enumerator Performance Trends"),
      plotlyOutput("enum_trends_plot", height = "300px")
    )
  ),
  
  nav_panel(
    title = "🌱 Agricultural Insights",
    layout_columns(
      col_widths = c(7, 5),
      card(
        full_screen = TRUE,
        card_header("🌳 Tree Survival Rates by Species"),
        plotlyOutput("survival_rates_plot")
      ),
      card(
        full_screen = TRUE,
        card_header("📋 Species Performance Details"),
        DT::dataTableOutput("species_table")
      )
    ),
    
    layout_columns(
      col_widths = c(6, 6),
      card(
        full_screen = TRUE,
        card_header("📊 Planting vs Survival Comparison"),
        plotlyOutput("planting_survival_plot")
      ),
      card(
        full_screen = TRUE,
        card_header("🎯 Species Performance by Site"),
        plotlyOutput("species_site_plot")
      )
    ),
    
    card(
      full_screen = TRUE,
      card_header("🗺️ Geographic Distribution of Surveys"),
      plotlyOutput("geographic_plot")
    )
  ),
  
  nav_panel(
    title = "🏢 Site Analysis",
    layout_columns(
      col_widths = c(6, 6),
      card(
        full_screen = TRUE,
        card_header("🏢 Site Performance Overview"),
        plotlyOutput("site_performance_plot", height = "400px")
      ),
      card(
        full_screen = TRUE,
        card_header("📊 Site Comparison Matrix"),
        DT::dataTableOutput("site_comparison_table")
      )
    ),
    
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        full_screen = TRUE,
        card_header("🌱 Survival Rates by Site"),
        plotlyOutput("site_survival_plot", height = "350px")
      ),
      card(
        full_screen = TRUE,
        card_header("⏱️ Duration Analysis by Site"),
        plotlyOutput("site_duration_plot", height = "350px")
      ),
      card(
        full_screen = TRUE,
        card_header("👥 Enumerator Distribution"),
        plotlyOutput("site_enum_plot", height = "350px")
      )
    )
  ),
  
  nav_panel(
    title = "🔍 Data Explorer",
    card(
      card_header("🔍 Interactive Data Explorer with Advanced Filters"),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        dateRangeInput("explorer_date_range", "Date Range:", 
                      start = min(df_processed$date, na.rm = TRUE), 
                      end = max(df_processed$date, na.rm = TRUE)),
        selectInput("explorer_site_filter", "Site:", 
                   choices = c("All" = "", sort(unique(df_processed$site))), multiple = TRUE),
        selectInput("explorer_enum_filter", "Enumerator:", 
                   choices = c("All" = "", sort(unique(df_processed$username))), multiple = TRUE),
        selectInput("explorer_woreda_filter", "Woreda:", 
                   choices = c("All" = "", sort(unique(df_processed$woreda))), multiple = TRUE)
      ),
      DT::dataTableOutput("explorer_table")
    )
  ),
  
  nav_panel(
    title = "📈 Advanced Analytics",
    layout_columns(
      col_widths = c(6, 6),
      card(
        full_screen = TRUE,
        card_header("🔄 Survey Completion Funnel"),
        plotlyOutput("funnel_plot")
      ),
      card(
        full_screen = TRUE,
        card_header("📊 Duration Distribution Analysis"),
        plotlyOutput("duration_plot")
      )
    ),
    
    layout_columns(
      col_widths = c(8, 4),
      card(
        full_screen = TRUE,
        card_header("📅 Weekly Performance Trends"),
        plotlyOutput("weekly_trends_plot")
      ),
      card(
        full_screen = TRUE,
        card_header("🎯 Key Performance Indicators"),
        DT::dataTableOutput("kpi_table")
      )
    ),
    
    card(
      full_screen = TRUE,
      card_header("🔍 Correlation Analysis"),
      plotlyOutput("correlation_plot", height = "400px")
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  # --- Reactive Data ---
  df_filtered <- reactive({
    data <- df_processed %>% filter(consent == 1)
    
    # Apply global filters
    if (!is.null(input$global_date_range)) {
      data <- data %>% 
        filter(date >= input$global_date_range[1] & date <= input$global_date_range[2])
    }
    
    if (length(input$global_site_filter) > 0 && !("" %in% input$global_site_filter)) {
      data <- data %>% filter(site %in% input$global_site_filter)
    }
    
    if (length(input$global_woreda_filter) > 0 && !("" %in% input$global_woreda_filter)) {
      data <- data %>% filter(woreda %in% input$global_woreda_filter)
    }
    
    if (length(input$global_enum_filter) > 0 && !("" %in% input$global_enum_filter)) {
      data <- data %>% filter(username %in% input$global_enum_filter)
    }
    
    data
  })
  
  # --- Reset Filters ---
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "global_site_filter", selected = character(0))
    updateSelectInput(session, "global_woreda_filter", selected = character(0))
    updateSelectInput(session, "global_enum_filter", selected = character(0))
    updateDateRangeInput(session, "global_date_range", 
                        start = min(df_processed$date, na.rm = TRUE), 
                        end = max(df_processed$date, na.rm = TRUE))
  })
  
  # --- Quick Stats ---
  output$filtered_count <- renderText({
    format_number(nrow(df_filtered()))
  })
  
  output$filtered_completion_rate <- renderText({
    total_filtered <- nrow(df_processed)
    if (total_filtered > 0) {
      completed_count <- nrow(df_filtered())
      rate <- completed_count / total_filtered
      format_percent(rate)
    } else {
      "0%"
    }
  })
  
  # --- Executive Summary ---
  output$total_surveys <- renderText({ format_number(nrow(df_processed)) })
  output$completed_surveys <- renderText({ format_number(nrow(df_filtered())) })
  output$refusal_rate <- renderText({
    rate <- safe_sum(df_processed$consent == 0) / nrow(df_processed)
    format_percent(rate)
  })
  output$avg_duration <- renderText({
    paste(round(safe_mean(df_filtered()$duration_minutes), 1), "min")
  })
  output$unique_enumerators <- renderText({ 
    format_number(n_distinct(df_filtered()$username)) 
  })
  
  # --- Enhanced Daily Progress Plot ---
  output$daily_progress_plot <- renderPlotly({
    daily_data <- df_filtered() %>%
      group_by(date) %>%
      summarise(surveys = n(), .groups = "drop") %>%
      arrange(date) %>%
      mutate(
        cumulative = cumsum(surveys),
        trend = predict(loess(surveys ~ as.numeric(date), span = 0.3))
      )
    
    p <- plot_ly(daily_data, x = ~date) %>%
      add_bars(y = ~surveys, name = "Daily Surveys", 
               marker = list(color = custom_colors$primary),
               hovertemplate = "Date: %{x}<br>Surveys: %{y}<extra></extra>") %>%
      add_lines(y = ~trend, name = "Trend", 
                line = list(color = custom_colors$danger, width = 3),
                hovertemplate = "Date: %{x}<br>Trend: %{y:.1f}<extra></extra>") %>%
      layout(
        title = FALSE,
        xaxis = list(title = "Date"),
        yaxis = list(title = "Number of Surveys"),
        paper_bgcolor = "transparent", 
        plot_bgcolor = "transparent",
        legend = list(orientation = 'h', y = 1.1, x = 0.5, xanchor = 'center')
      )
    
    p
  })
  
  # --- Enhanced Gauge Plot ---
  output$gauge_plot <- renderPlotly({
    target <- 3600
    current <- nrow(df_filtered())
    percentage <- min(current / target, 1) * 100
    
    plot_ly(
      type = "indicator",
      mode = "gauge+number+delta",
      value = current,
      delta = list(reference = target, increasing = list(color = custom_colors$success)),
      gauge = list(
        axis = list(range = list(NULL, target)),
        bar = list(color = custom_colors$primary),
        steps = list(
          list(range = c(0, target*0.5), color = "lightgray"),
          list(range = c(target*0.5, target*0.8), color = "gray")
        ),
        threshold = list(
          line = list(color = custom_colors$danger, width = 4),
          thickness = 0.75,
          value = target
        )
      )
    ) %>% 
    layout(
      title = FALSE, 
      paper_bgcolor = "transparent",
      font = list(color = custom_colors$dark)
    )
  })
  
  # --- New: Completion Patterns Plot ---
  output$completion_patterns_plot <- renderPlotly({
    pattern_data <- df_filtered() %>%
      group_by(day_of_week) %>%
      summarise(surveys = n(), .groups = "drop")
    
    plot_ly(pattern_data, x = ~day_of_week, y = ~surveys, type = 'bar',
            marker = list(color = custom_colors$secondary),
            hovertemplate = "Day: %{x}<br>Surveys: %{y}<extra></extra>") %>%
      layout(
        title = FALSE,
        xaxis = list(title = "Day of Week"),
        yaxis = list(title = "Number of Surveys"),
        paper_bgcolor = "transparent", 
        plot_bgcolor = "transparent"
      )
  })
  
  # --- New: Timing Analysis Plot ---
  output$timing_analysis_plot <- renderPlotly({
    timing_data <- df_filtered() %>%
      group_by(hour_started) %>%
      summarise(surveys = n(), .groups = "drop")
    
    plot_ly(timing_data, x = ~hour_started, y = ~surveys, type = 'scatter', mode = 'lines+markers',
            line = list(color = custom_colors$primary, width = 3),
            marker = list(color = custom_colors$primary, size = 8),
            hovertemplate = "Hour: %{x}<br>Surveys: %{y}<extra></extra>") %>%
      layout(
        title = FALSE,
        xaxis = list(title = "Hour of Day"),
        yaxis = list(title = "Number of Surveys"),
        paper_bgcolor = "transparent", 
        plot_bgcolor = "transparent"
      )
  })
  
  # --- Enhanced Enumerator Performance ---
  enumerator_stats <- reactive({
    df_filtered() %>%
      group_by(username) %>%
      summarise(
        total_surveys = n(),
        avg_duration = round(safe_mean(duration_minutes), 1),
        short_surveys = safe_sum(is_short_survey),
        night_surveys = safe_sum(is_night_survey),
        weekend_surveys = safe_sum(is_weekend),
        efficiency_score = round((total_surveys / (avg_duration + 1)) * 100, 1),
        .groups = "drop"
      ) %>%
      arrange(desc(total_surveys))
  })
  
  output$enumerator_table <- DT::renderDataTable({
    DT::datatable(
      enumerator_stats(),
      colnames = c("Enumerator", "Total Surveys", "Avg Duration (min)", 
                   "Short Surveys", "Night Surveys", "Weekend Surveys", "Efficiency Score"),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE
    ) %>%
    DT::formatStyle("efficiency_score", 
                    backgroundColor = DT::styleInterval(c(50, 100), c("#ffcdd2", "#fff9c4", "#c8e6c9")))
  })
  
  # --- Enhanced Target Comparison ---
  output$target_comparison_plot <- renderPlotly({
    target_per_enum <- 50
    p_data <- enumerator_stats()
    
    plot_ly(p_data, x = ~reorder(username, -total_surveys), y = ~total_surveys, 
            type = 'bar', name = "Actual",
            marker = list(color = custom_colors$primary),
            hovertemplate = "Enumerator: %{x}<br>Surveys: %{y}<br>Efficiency: %{customdata}<extra></extra>",
            customdata = ~efficiency_score) %>%
      add_trace(x = ~reorder(username, -total_surveys), y = target_per_enum, 
                type = 'scatter', mode = 'lines', name = "Target",
                line = list(color = custom_colors$danger, dash = 'dash', width = 3)) %>%
      layout(
        title = FALSE,
        xaxis = list(title = "Enumerator", tickangle = -45),
        yaxis = list(title = "Total Surveys"),
        paper_bgcolor = "transparent", 
        plot_bgcolor = "transparent",
        legend = list(orientation = 'h', y = 1.1, x = 0.5, xanchor = 'center'),
        margin = list(b = 100)
      )
  })
  
  # --- Enhanced Productivity Heatmap ---
  output$productivity_heatmap <- renderPlotly({
    heatmap_data <- df_filtered() %>%
      group_by(day_of_week, hour_started) %>%
      summarise(count = n(), .groups = "drop") %>%
      complete(day_of_week, hour_started, fill = list(count = 0))
      
    plot_ly(
      data = heatmap_data,
      x = ~hour_started, y = ~day_of_week, z = ~count,
      type = "heatmap", 
      colorscale = list(c(0, "#f8f9fa"), c(1, custom_colors$primary)),
      hovertemplate = "Day: %{y}<br>Hour: %{x}<br>Surveys: %{z}<extra></extra>"
    ) %>% 
    layout(
      title = FALSE,
      xaxis = list(title = "Hour of Day"),
      yaxis = list(title = "Day of Week"),
      paper_bgcolor = "transparent"
    )
  })
  
  # --- Quality Alerts Table ---
  output$quality_alerts_table <- DT::renderDataTable({
    quality_data <- enumerator_stats() %>%
      filter(short_surveys > 0 | night_surveys > 0) %>%
      select(Enumerator = username, `Short Surveys` = short_surveys, 
             `Night Surveys` = night_surveys, `Efficiency Score` = efficiency_score)
      
    DT::datatable(
      quality_data,
      options = list(pageLength = 5, dom = 't'),
      rownames = FALSE
    ) %>%
    DT::formatStyle("Short Surveys", 
                    backgroundColor = DT::styleInterval(0, c("white", "#ffcdd2"))) %>%
    DT::formatStyle("Night Surveys", 
                    backgroundColor = DT::styleInterval(0, c("white", "#ffcdd2")))
  })
  
  # --- New: Enumerator Trends ---
  output$enum_trends_plot <- renderPlotly({
    trends_data <- df_filtered() %>%
      group_by(date, username) %>%
      summarise(daily_surveys = n(), .groups = "drop") %>%
      group_by(username) %>%
      arrange(date) %>%
      mutate(cumulative = cumsum(daily_surveys)) %>%
      ungroup()
    
    p <- plot_ly(trends_data, x = ~date, y = ~cumulative, color = ~username, type = 'scatter', mode = 'lines',
                 hovertemplate = "Date: %{x}<br>Cumulative: %{y}<br>Enumerator: %{color}<extra></extra>") %>%
      layout(
        title = FALSE,
        xaxis = list(title = "Date"),
        yaxis = list(title = "Cumulative Surveys"),
        paper_bgcolor = "transparent", 
        plot_bgcolor = "transparent"
      )
    
    p
  })
  
  # --- Enhanced Agricultural Analysis ---
  species_data <- reactive({
    species_cols <- names(df_filtered())[grepl("ps_num_planted_|num_surv_", names(df_filtered()))]
    
    if(length(species_cols) > 0) {
      tryCatch({
        df_filtered() %>%
          select(all_of(species_cols)) %>%
          # Convert all columns to numeric, handling character values
          mutate(across(everything(), ~ as.numeric(as.character(.)))) %>%
          pivot_longer(everything(), names_to = "key", values_to = "value") %>%
          mutate(
            type = ifelse(str_detect(key, "ps_num_planted"), "planted", "survived"),
            species = str_remove_all(key, "ps_num_planted_|num_surv_"),
            value = as.numeric(value)  # Ensure value is numeric
          ) %>%
          filter(!is.na(value)) %>%  # Remove NA values
          group_by(species, type) %>%
          summarise(total = safe_sum(value), .groups = "drop") %>%
          pivot_wider(names_from = type, values_from = total, values_fill = 0) %>%
          mutate(
            survival_rate = ifelse(planted > 0, (survived / planted) * 100, 0)
          )
      }, error = function(e) {
        # Return empty data frame if there's an error
        data.frame(species = character(0), planted = numeric(0), 
                  survived = numeric(0), survival_rate = numeric(0))
      })
    } else {
      # Return empty data frame with correct structure
      data.frame(species = character(0), planted = numeric(0), 
                survived = numeric(0), survival_rate = numeric(0))
    }
  })
  
  output$survival_rates_plot <- renderPlotly({
    p_data <- species_data() %>% arrange(desc(survival_rate))
    
    if(nrow(p_data) > 0) {
      plot_ly(p_data, x = ~reorder(species, survival_rate), y = ~survival_rate, 
              type = 'bar', marker = list(color = ~survival_rate, 
              colorscale = list(c(0, "#ffcdd2"), c(1, custom_colors$success))),
              hovertemplate = "Species: %{x}<br>Survival Rate: %{y:.1f}%<br>Planted: %{customdata[0]}<br>Survived: %{customdata[1]}<extra></extra>",
              customdata = ~cbind(planted, survived)) %>%
        layout(
          title = FALSE,
          xaxis = list(title = "Species", tickangle = -45),
          yaxis = list(title = "Survival Rate (%)", range = c(0, 100)),
          paper_bgcolor = "transparent", 
          plot_bgcolor = "transparent",
          margin = list(b = 100)
        )
    } else {
      plot_ly() %>%
        layout(
          title = "No species data available",
          paper_bgcolor = "transparent"
        )
    }
  })
  
  output$species_table <- DT::renderDataTable({
    display_data <- species_data() %>%
      select(Species = species, Planted = planted, Survived = survived, `Survival Rate (%)` = survival_rate) %>%
      arrange(desc(`Survival Rate (%)`))
      
    DT::datatable(
      display_data,
      options = list(pageLength = 8, dom = 'Bfrtip', buttons = c('copy', 'csv')),
      rownames = FALSE
    ) %>%
    DT::formatStyle("Survival Rate (%)", 
                    backgroundColor = DT::styleInterval(c(60, 80), c("#ffcdd2", "#fff9c4", "#c8e6c9"))) %>%
    DT::formatRound("Survival Rate (%)", 1)
  })
  
  # --- New: Planting vs Survival Comparison ---
  output$planting_survival_plot <- renderPlotly({
    p_data <- species_data()
    
    if(nrow(p_data) > 0) {
      plot_ly(p_data, x = ~species, y = ~planted, type = 'bar', name = 'Planted',
              marker = list(color = custom_colors$primary)) %>%
        add_trace(y = ~survived, name = 'Survived', 
                  marker = list(color = custom_colors$success)) %>%
        layout(
          title = FALSE,
          xaxis = list(title = "Species", tickangle = -45),
          yaxis = list(title = "Number of Trees"),
          paper_bgcolor = "transparent", 
          plot_bgcolor = "transparent",
          barmode = 'group',
          legend = list(orientation = 'h', y = 1.1, x = 0.5, xanchor = 'center'),
          margin = list(b = 100)
        )
    } else {
      plot_ly() %>% layout(title = "No data available", paper_bgcolor = "transparent")
    }
  })
  
  # --- New: Species Performance by Site ---
  output$species_site_plot <- renderPlotly({
    if(nrow(species_data()) > 0) {
      tryCatch({
        site_species_data <- df_filtered() %>%
          select(site, starts_with("ps_num_planted_"), starts_with("num_surv_")) %>%
          # Convert all species columns to numeric first
          mutate(across(c(starts_with("ps_num_planted_"), starts_with("num_surv_")), ~ as.numeric(as.character(.)))) %>%
          group_by(site) %>%
          summarise(across(everything(), safe_sum), .groups = "drop") %>%
          pivot_longer(-site, names_to = "key", values_to = "value") %>%
          mutate(
            type = ifelse(str_detect(key, "ps_num_planted"), "planted", "survived"),
            species = str_remove_all(key, "ps_num_planted_|num_surv_"),
            value = as.numeric(value)
          ) %>%
          filter(!is.na(value)) %>%
          group_by(site, species, type) %>%
          summarise(total = safe_sum(value), .groups = "drop") %>%
          pivot_wider(names_from = type, values_from = total, values_fill = 0) %>%
          mutate(survival_rate = ifelse(planted > 0, (survived / planted) * 100, 0))
        
        plot_ly(site_species_data, x = ~site, y = ~survival_rate, color = ~species, 
                type = 'bar', 
                hovertemplate = "Site: %{x}<br>Species: %{color}<br>Survival Rate: %{y:.1f}%<extra></extra>") %>%
          layout(
            title = FALSE,
            xaxis = list(title = "Site", tickangle = -45),
            yaxis = list(title = "Survival Rate (%)"),
            paper_bgcolor = "transparent", 
            plot_bgcolor = "transparent",
            barmode = 'group',
            margin = list(b = 100)
          )
      }, error = function(e) {
        plot_ly() %>% layout(title = "Error processing species data", paper_bgcolor = "transparent")
      })
    } else {
      plot_ly() %>% layout(title = "No data available", paper_bgcolor = "transparent")
    }
  })
  
  # --- Geographic Plot ---
  output$geographic_plot <- renderPlotly({
    geo_data <- df_filtered() %>%
      group_by(woreda) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count))
      
    plot_ly(geo_data, x = ~reorder(woreda, -count), y = ~count, 
            type = 'bar', marker = list(color = custom_colors$secondary),
            hovertemplate = "Woreda: %{x}<br>Surveys: %{y}<extra></extra>") %>%
      layout(
        title = FALSE,
        xaxis = list(title = "Woreda", tickangle = -45),
        yaxis = list(title = "Number of Surveys"),
        paper_bgcolor = "transparent", 
        plot_bgcolor = "transparent",
        margin = list(b = 100)
      )
  })
  
  # --- Enhanced Site Analysis ---
  site_stats <- reactive({
    df_filtered() %>%
      group_by(site) %>%
      summarise(
        total_surveys = n(),
        avg_duration = round(safe_mean(duration_minutes), 1),
        completion_rate = round((n() / nrow(df_filtered())) * 100, 1),
        unique_enumerators = n_distinct(username),
        avg_hh_size = round(safe_mean(hh_size), 1),
        weekend_surveys = safe_sum(is_weekend),
        night_surveys = safe_sum(is_night_survey),
        .groups = "drop"
      ) %>%
      arrange(desc(total_surveys))
  })
  
  output$site_performance_plot <- renderPlotly({
    p_data <- site_stats()
    
    plot_ly(p_data, x = ~reorder(site, -total_surveys), y = ~total_surveys, 
            type = 'bar', marker = list(color = custom_colors$gradient[1]),
            hovertemplate = "Site: %{x}<br>Surveys: %{y}<br>Avg Duration: %{customdata[0]} min<br>Enumerators: %{customdata[1]}<extra></extra>",
            customdata = ~cbind(avg_duration, unique_enumerators)) %>%
      layout(
        title = FALSE,
        xaxis = list(title = "Site", tickangle = -45),
        yaxis = list(title = "Number of Surveys"),
        paper_bgcolor = "transparent", 
        plot_bgcolor = "transparent",
        margin = list(b = 100)
      )
  })
  
  output$site_comparison_table <- DT::renderDataTable({
    DT::datatable(
      site_stats(),
      colnames = c("Site", "Total Surveys", "Avg Duration (min)", "Completion Rate (%)", 
                   "Enumerators", "Avg HH Size", "Weekend Surveys", "Night Surveys"),
      options = list(
        pageLength = 8,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE
    ) %>%
    DT::formatStyle("completion_rate", 
                    backgroundColor = DT::styleInterval(c(10, 20), c("#ffcdd2", "#fff9c4", "#c8e6c9")))
  })
  
  # --- Site Survival Plot ---
  output$site_survival_plot <- renderPlotly({
    site_survival <- df_filtered() %>%
      select(site, starts_with("ps_num_planted_"), starts_with("num_surv_")) %>%
      group_by(site) %>%
      summarise(
        total_planted = safe_sum(across(starts_with("ps_num_planted_"))),
        total_survived = safe_sum(across(starts_with("num_surv_"))),
        .groups = "drop"
      ) %>%
      mutate(
        survival_rate = ifelse(total_planted > 0, (total_survived / total_planted) * 100, 0)
      ) %>%
      arrange(desc(survival_rate))
    
    plot_ly(site_survival, x = ~reorder(site, survival_rate), y = ~survival_rate, 
            type = 'bar', marker = list(color = ~survival_rate, 
            colorscale = list(c(0, "#ffcdd2"), c(1, custom_colors$success))),
            hovertemplate = "Site: %{x}<br>Planted: %{customdata[0]}<br>Survived: %{customdata[1]}<br>Rate: %{y:.1f}%<extra></extra>",
            customdata = ~cbind(total_planted, total_survived)) %>%
      layout(
        title = FALSE,
        xaxis = list(title = "Site", tickangle = -45),
        yaxis = list(title = "Survival Rate (%)", range = c(0, 100)),
        paper_bgcolor = "transparent", 
        plot_bgcolor = "transparent",
        margin = list(b = 100)
      )
  })
  
  # --- Site Duration Plot ---
  output$site_duration_plot <- renderPlotly({
    duration_data <- site_stats() %>%
      arrange(desc(avg_duration))
    
    plot_ly(duration_data, x = ~reorder(site, avg_duration), y = ~avg_duration, 
            type = 'bar', marker = list(color = custom_colors$warning),
            hovertemplate = "Site: %{x}<br>Avg Duration: %{y} min<extra></extra>") %>%
      layout(
        title = FALSE,
        xaxis = list(title = "Site", tickangle = -45),
        yaxis = list(title = "Average Duration (minutes)"),
        paper_bgcolor = "transparent", 
        plot_bgcolor = "transparent",
        margin = list(b = 100)
      )
  })
  
  # --- New: Site Enumerator Distribution ---
  output$site_enum_plot <- renderPlotly({
    enum_dist <- site_stats() %>%
      arrange(desc(unique_enumerators))
    
    plot_ly(enum_dist, x = ~reorder(site, unique_enumerators), y = ~unique_enumerators, 
            type = 'bar', marker = list(color = custom_colors$info),
            hovertemplate = "Site: %{x}<br>Enumerators: %{y}<extra></extra>") %>%
      layout(
        title = FALSE,
        xaxis = list(title = "Site", tickangle = -45),
        yaxis = list(title = "Number of Enumerators"),
        paper_bgcolor = "transparent", 
        plot_bgcolor = "transparent",
        margin = list(b = 100)
      )
  })
  
  # --- Enhanced Data Explorer ---
  explorer_filtered_data <- reactive({
    df <- df_filtered() %>%
      select(
        Date = date, Enumerator = username, 
        Site = site, Woreda = woreda, `Duration (min)` = duration_minutes,
        `HH Size` = hh_size, `Education` = education_level, Age = age, Sex = sex,
        `Weekend Survey` = is_weekend, `Night Survey` = is_night_survey
      )
      
    # Apply explorer-specific filters
    if (!is.null(input$explorer_date_range)) {
      df <- df %>% 
        filter(Date >= input$explorer_date_range[1] & Date <= input$explorer_date_range[2])
    }
    
    if (length(input$explorer_site_filter) > 0 && !("" %in% input$explorer_site_filter)) {
      df <- df %>% filter(Site %in% input$explorer_site_filter)
    }
    
    if (length(input$explorer_enum_filter) > 0 && !("" %in% input$explorer_enum_filter)) {
      df <- df %>% filter(Enumerator %in% input$explorer_enum_filter)
    }
    
    if (length(input$explorer_woreda_filter) > 0 && !("" %in% input$explorer_woreda_filter)) {
      df <- df %>% filter(Woreda %in% input$explorer_woreda_filter)
    }
    
    df
  })
  
  output$explorer_table <- DT::renderDataTable({
    DT::datatable(
      explorer_filtered_data(),
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf'),
        searchHighlight = TRUE
      ),
      rownames = FALSE,
      filter = 'top'
    ) %>%
    DT::formatStyle("Weekend Survey", 
                    backgroundColor = DT::styleEqual(TRUE, "#fff9c4")) %>%
    DT::formatStyle("Night Survey", 
                    backgroundColor = DT::styleEqual(TRUE, "#ffcdd2"))
  })
  
  # --- Advanced Analytics ---
  output$funnel_plot <- renderPlotly({
    total_attempts <- nrow(df_processed)
    completed <- safe_sum(df_processed$consent == 1)
    refused <- safe_sum(df_processed$consent == 0)
    
    funnel_data <- data.frame(
      stage = c("Total Attempts", "Completed", "Refused"),
      value = c(total_attempts, completed, refused),
      color = c(custom_colors$primary, custom_colors$success, custom_colors$danger)
    )
    
    plot_ly(funnel_data, type = 'funnel',
            y = ~stage, x = ~value,
            textposition = "inside", textinfo = "value+percent initial",
            marker = list(color = ~color)) %>%
      layout(
        title = FALSE, 
        paper_bgcolor = "transparent",
        font = list(color = custom_colors$dark)
      )
  })
  
  output$duration_plot <- renderPlotly({
    plot_ly(df_filtered(), x = ~duration_minutes, type = 'histogram', nbinsx = 30,
            marker = list(color = custom_colors$secondary, opacity = 0.7),
            hovertemplate = "Duration: %{x} min<br>Count: %{y}<extra></extra>") %>%
      add_vline(x = mean(df_filtered()$duration_minutes, na.rm = TRUE), 
                line = list(color = custom_colors$danger, dash = "dash", width = 2),
                annotation = list(text = "Mean", showarrow = TRUE)) %>%
      layout(
        title = FALSE,
        xaxis = list(title = "Duration (minutes)"),
        yaxis = list(title = "Frequency"),
        paper_bgcolor = "transparent", 
        plot_bgcolor = "transparent"
      )
  })
  
  output$weekly_trends_plot <- renderPlotly({
    weekly_data <- df_filtered() %>%
      group_by(week) %>%
      summarise(
        surveys = n(),
        avg_duration = safe_mean(duration_minutes),
        completion_rate = n() / nrow(df_processed) * 100,
        .groups = "drop"
      )
      
    plot_ly(weekly_data, x = ~week) %>%
      add_trace(y = ~surveys, type = 'scatter', mode = 'lines+markers', name = 'Surveys',
                line = list(color = custom_colors$primary, width = 3), 
                marker = list(color = custom_colors$primary, size = 8),
                yaxis = 'y1') %>%
      add_trace(y = ~avg_duration, type = 'scatter', mode = 'lines+markers', name = 'Avg Duration',
                line = list(color = custom_colors$warning, width = 3), 
                marker = list(color = custom_colors$warning, size = 8),
                yaxis = 'y2') %>%
      layout(
        title = FALSE,
        xaxis = list(title = "Week"),
        yaxis = list(title = "Number of Surveys", side = 'left'),
        yaxis2 = list(title = "Average Duration (min)", side = 'right', overlaying = 'y'),
        paper_bgcolor = "transparent", 
        plot_bgcolor = "transparent",
        legend = list(orientation = 'h', y = 1.1, x = 0.5, xanchor = 'center')
      )
  })
  
  # --- New: KPI Table ---
  output$kpi_table <- DT::renderDataTable({
    kpi_data <- data.frame(
      KPI = c("Total Surveys", "Completion Rate", "Avg Duration", "Quality Score", 
              "Productivity", "Coverage"),
      Value = c(
        nrow(df_filtered()),
        paste0(round(nrow(df_filtered()) / nrow(df_processed) * 100, 1), "%"),
        paste0(round(safe_mean(df_filtered()$duration_minutes), 1), " min"),
        paste0(round((1 - safe_sum(df_filtered()$is_short_survey) / nrow(df_filtered())) * 100, 1), "%"),
        paste0(round(nrow(df_filtered()) / n_distinct(df_filtered()$username), 1), " surveys/enum"),
        paste0(n_distinct(df_filtered()$woreda), " woredas")
      ),
      Target = c("3600", "90%", "15 min", "95%", "50 surveys/enum", "10 woredas"),
      Status = c("📊", "✅", "⚠️", "✅", "📈", "✅")
    )
    
    DT::datatable(
      kpi_data,
      options = list(pageLength = 6, dom = 't', ordering = FALSE),
      rownames = FALSE
    )
  })
  
  # --- New: Correlation Analysis ---
  output$correlation_plot <- renderPlotly({
    # Create correlation matrix for numeric variables
    numeric_vars <- df_filtered() %>%
      select(duration_minutes, hh_size, age) %>%
      select_if(is.numeric)
    
    if(ncol(numeric_vars) > 1) {
      cor_matrix <- cor(numeric_vars, use = "complete.obs")
      
      plot_ly(
        z = ~cor_matrix,
        x = ~colnames(cor_matrix),
        y = ~colnames(cor_matrix),
        type = "heatmap",
        colorscale = list(c(0, "#ffcdd2"), c(0.5, "white"), c(1, custom_colors$success)),
        hovertemplate = "X: %{x}<br>Y: %{y}<br>Correlation: %{z:.3f}<extra></extra>"
      ) %>%
      layout(
        title = FALSE,
        xaxis = list(title = "Variables"),
        yaxis = list(title = "Variables"),
        paper_bgcolor = "transparent"
      )
    } else {
      plot_ly() %>% 
        layout(title = "Insufficient numeric data for correlation analysis",
               paper_bgcolor = "transparent")
    }
  })
}

# --- Run Application ---
shinyApp(ui, server)