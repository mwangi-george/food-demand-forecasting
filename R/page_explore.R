
explore_page_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    layout_sidebar(
      fillable = FALSE,
      fill = FALSE,
      
      sidebar = sidebar(
        width = 280,
        
        # Header
        div(
          class = "mb-4",
          
          div(
            class = "d-flex align-items-center gap-2 mb-1",
            
            bsicons::bs_icon("sliders", style = "color: #0B538E;" ),
            
            h6("Explore Data", class = "fw-bold mb-0" )
          ),
          
          p(
            "Select a center and meal to explore historical demand.",
            class = "text-muted small mb-0"
          )
        ),
        
        
        # Center
        div(
          class = "mb-3",
          
          selectInput(ns("center"), "Food Center", choices = center_ids, width = "100%" )
        ),
        
        
        # Meal
        div(
          class = "mb-3",
          
          selectInput(ns("meal"), "Meal ID", choices = NULL, width = "100%" )
        ),
        
        
        # Context note
        div(
          class = "mt-4 p-3 rounded-2",
          style = "background-color: #e6f2fd; border: 1px solid #d6e9f8;",
          
          div(
            class = "d-flex gap-2 align-items-start",
            
            bsicons::bs_icon(
              "info-circle",
              style = "color: #0B538E; margin-top: 2px; flex-shrink: 0;"
            ),
            
            p(
              "Charts update automatically when you change the selected center or meal.",
              class = "small mb-0",
              style = "color: #0B538E;"
            )
          )
        )
      ),
      
      nav_panel(
        "Demand Analysis",
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          gap = "1rem", # Slightly larger gap gives the UI room to breathe
          
          value_box(
            title = "Min. Weekly Orders",
            value = textOutput(ns("min_weekly_orders")),
            theme = value_box_theme(bg = "#e6f2fd", fg = "#0B538E"),
            height = "100px"
          ),
          
          value_box(
            title = "Avg. Weekly Orders",
            value = textOutput(ns("avg_weekly_orders")),
            theme = value_box_theme(bg = "#e6f2fd", fg = "#0B538E"),
            height = "100px"
          ),
          
          value_box(
            title = "Max. Weekly Orders",
            value = textOutput(ns("max_weekly_orders")),
            theme = value_box_theme(bg = "#e6f2fd", fg = "#0B538E"),
            height = "100px"
          ),
          
          
          value_box(
            title = "Total Orders",
            value = textOutput(ns("total_orders")),
            theme = value_box_theme(bg = "#e6f2fd", fg = "#0B538E"),
            class = "border",
            height = "100px"
          )
        ),
        
        layout_columns(
          col_widths = c(6, 6),
          
          div(
            class = "border rounded-2 p-2",
            style = "border-color: #e9ecef !important;",
            
            plotlyOutput(ns("time_series_plot"),height = "500px") |> 
              withSpinner(type = 4, size = 0.5)
          ),
          
          div(
            class = "border rounded-2 p-2",
            style = "border-color: #e9ecef !important;",
            
            plotlyOutput(ns("anomaly_diagnostics_plot"),height = "500px") |> 
              withSpinner(type = 4, size = 0.5)
          )
        )
      )
    ),
  )
}


explore_page_server <- function(id, meals_df, iqr_alpha = 0.05, show_smooth_line = TRUE) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$center, {
      
      req(input$center)
      
      dynamic_meal_ids <- meals_df |> 
        filter(center_id == input$center) |> 
        distinct(meal_id) |> 
        pull(meal_id)
      
      # If center id changes, the meal ids are also updated
      updateSelectInput(
        session,
        inputId = "meal",
        choices = dynamic_meal_ids
      )
    })
    
    plotting_df <- reactive({
      req(input$center, input$meal, meals_df)
      
      meals_df |> 
        filter(center_id == input$center, meal_id == input$meal)
    })
    
    # Summary statistics
    summary_stats <- reactive({
      df <- plotting_df()
      
      tibble(
        min_weekly_orders = min(df$num_orders, na.rm = TRUE),
        max_weekly_orders = max(df$num_orders, na.rm = TRUE),
        avg_weekly_orders   = mean(df$num_orders, na.rm = TRUE),
        total_orders  = sum(df$num_orders, na.rm = TRUE)
      )
    })
    
    # =========================================================
    # PRIMARY KPI OUTPUTS
    # =========================================================
    output$min_weekly_orders <- renderText({ scales::comma(summary_stats()$min_weekly_orders) })
    output$avg_weekly_orders       <- renderText({ scales::comma(round(summary_stats()$avg_weekly_orders)) })
    output$max_weekly_orders        <- renderText({ scales::comma(summary_stats()$max_weekly_orders) })
    output$total_orders       <- renderText({ scales::comma(round(summary_stats()$total_orders)) })
    
    output$time_series_plot <- renderPlotly({
      df <- plotting_df()
      
      # Handle empty data case safely within the reactive endpoint
      if (nrow(df) == 0) {
        return(NULL)
      }
      
      df |> 
        plot_time_series(
          .date_var = period, 
          .value = num_orders, 
          .color_var = year(period), 
          .title = glue("Historical Demand"),
          .y_lab = "Meal Orders",
          .smooth = show_smooth_line
        ) |> 
        config(displayModeBar = FALSE)
    })
    
    output$anomaly_diagnostics_plot <- renderPlotly({
      df <- plotting_df()
      
      # Handle empty data case safely within the reactive endpoint
      if (nrow(df) == 0) {
        return(NULL)
      }
      
      df |> 
        plot_anomaly_diagnostics(
          .date_var = period, 
          .value = num_orders,
          .alpha = iqr_alpha,
          .title = glue("Anomaly Diagnostics"),
          .y_lab = "",
          .message = FALSE,
          .legend_show = TRUE
        ) |> 
        config(displayModeBar = FALSE)
    })
  })
}






















