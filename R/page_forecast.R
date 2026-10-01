
forecast_page_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    layout_sidebar(
      fillable = FALSE,
      fill = FALSE,
      
      sidebar = sidebar(
        width = 280,
        
        # -----------------------------------------------------
        # Forecast configuration
        # -----------------------------------------------------
        
        div(
          class = "mb-1",
          
          h5("Forecast Configuration", class = "fw-semibold mb-1"),
          
          p(
            "Select a meal and define the forecast period.",
            class = "text-muted small mb-0"
          )
        ),
        
        selectInput(ns("center"), "Food Center", choices = center_ids),
        
        selectInput(ns("meal"), "Meal ID", choices = NULL),
        
        numericInput(
          ns("horizon"),
          "Forecast Horizon (Weeks)",
          value = 52, min = 1, step = 1
        ),
        
        sliderTextInput(
          inputId = ns("confidence_level"),
          label = "Forecast Uncertainty",
          choices = c(0.75, 0.80, 0.85, 0.90, 0.95),
          selected = 0.90,
          grid = TRUE,
          width = "100%"
        ),
        
        br(),
        
        actionButton(ns("run_forecast"), "Run Forecast",class = "btn-primary w-100"),
        
        # ---------------------------------------------------------
        # Model Information
        # ---------------------------------------------------------
        
        div(
          class = "mt-5 p-2 rounded-2",
          style = "background-color: #e6f2fd; border: 1px solid #d6e9f8;",
          
          # Header
          div(
            class = "d-flex align-items-center gap-2 mb-3",
            bsicons::bs_icon("info-circle", style = "color: #0B538E;"),
            
            h6("Forecasting Algorithms", class = "fw-semibold mb-0", style = "color: #0B538E;")
          ),
          
          div(
            class = "mb-2",
            tags$strong("XGBOOST", style = "font-size: 0.78rem; color: #0B538E;"),
            div("Gradient Boosting", class = "text-muted", style = "font-size: 0.75rem;")
          ),
          
          div(
            class = "mb-2",
            tags$strong("RANGER", style = "font-size: 0.78rem; color: #0B538E;"),
            div("Random Forest", class = "text-muted", style = "font-size: 0.75rem;")
          ),
          
          div(
            class = "mb-2",
            tags$strong("KERNLAB", style = "font-size: 0.78rem; color: #0B538E;"),
            div("Radial Basis Function Support Vector Machines", class = "text-muted", style = "font-size: 0.75rem;")
          ),
          
          div(
            class = "mb-2",
            tags$strong("KKNN", style = "font-size: 0.78rem; color: #0B538E;"),
            div("K-Nearest Neighbors", class = "text-muted", style = "font-size: 0.75rem;")
          ),
          
          div(
            tags$strong("RPART", style = "font-size: 0.78rem; color: #0B538E;"),
            div("Decision Tree", class = "text-muted", style = "font-size: 0.75rem;")
          )
        )
      ),
      
      
      # =======================================================
      # MAIN CONTENT
      # =======================================================
      
      div(
        
        # -----------------------------------------------------
        # Forecast
        # -----------------------------------------------------
        div(
          class = "border rounded-2 p-2",
          style = "border-color: #e9ecef !important;",
          div(
            class = "d-flex justify-content-between align-items-center mb-2",
            
            div(
              p(
                "Review the generated forecast and model performance.",
                class = "text-muted mb-0"
              )
            ),
            
            downloadButton(
              ns("download_forecast"),
              "Download Forecast",
              class = "btn btn-outline-primary btn-sm"
            )
          ),
          
          plotlyOutput(
            ns("forecast_plot"),
            height = "450px"
          )
        ),
        
        # -----------------------------------------------------
        # Model performance
        # -----------------------------------------------------
        
        layout_columns(
          class = "border rounded-2 p-2 mt-2",
          style = "border-color: #e9ecef !important;",
          col_widths = c(8, 4),
          
          # Performance plot
          plotlyOutput(
            ns("model_performance_plot"),
            height = "400px"
          ),
          
          # Performance table
          div(
            style = "solid #e2e8f0; min-height: 400px;",
            
            gt_output(ns("model_performance_table"))
          )
        )
      )
    )
  )
}


forecast_page_server <- function(id, meals_df, iqr_alpha = 0.05) {
  
  moduleServer(id, function(input, output, session) {
    
    # ---------------------------------------------------------
    # Update meals when center changes
    # ---------------------------------------------------------
    
    observeEvent(input$center, {
      
      req(input$center)
      
      dynamic_meal_ids <- meals_df |> 
        filter(center_id == input$center) |> 
        distinct(meal_id) |> 
        pull(meal_id)
      
      updateSelectInput(
        session,
        inputId = "meal",
        choices = dynamic_meal_ids
      )
    })
    
    
    # ---------------------------------------------------------
    # Run forecasting pipeline ONLY when button is clicked
    # ---------------------------------------------------------
    
    forecast_results <- eventReactive(input$run_forecast, {
      
      req(
        input$center,
        input$meal,
        input$horizon,
        input$confidence_level
      )
      
      # Snapshot the inputs at the time the button is clicked
      center  <- input$center
      meal    <- input$meal
      horizon <- input$horizon
      confidence_level <- input$confidence_level
      
      showPageSpinner(
        caption = "Generating forecast...",
        expr = {
          
          # Filter dataset
          forecast_data <- meals_df |> 
            filter(
              center_id == center,
              meal_id == meal
            )
          
          # Forecasting splits
          forecasting_splits <- generate_forecasting_splits(
            forecast_data
          )
          
          training_dataset <- training(forecasting_splits)
          testing_dataset  <- testing(forecasting_splits)
          
          # Feature engineering
          ml_recipe <- generate_feature_engineering_recipe(
            training_dataset
          )
          
          # Model specifications
          model_specs <- generate_model_specifications()
          
          # Fit workflows
          fitted_workflows <- generate_fitted_workflows(
            model_spec_list = model_specs,
            feat_engineering_recipe = ml_recipe,
            training_df = training_dataset
          )
          
          # Modeltime table
          models_table <- generate_modeltime_table(
            fitted_workflows
          )
          
          # Calibration
          calibration_table_data <- generate_calibration_table(
            models_table,
            testing_dataset
          )
          
          # Future forecast
          future_forecast_df <- generate_future_forecast_data(
            calibration_table = calibration_table_data,
            original_series = forecast_data,
            horizon_in_weeks = horizon,
            confidence_interval = confidence_level
          )
        }
      )
      
      # Return everything required by the outputs
      list(
        center = center,
        meal = meal,
        horizon = horizon,
        confidence_level = confidence_level,
        original_data = forecast_data,
        testing_data = testing_dataset,
        calibration_table = calibration_table_data,
        forecast_data = future_forecast_df
      )
      
    })
    
    # ---------------------------------------------------------
    # Forecast Plot
    # ---------------------------------------------------------
    
    output$forecast_plot <- renderPlotly({
      
      results <- forecast_results()
      
      generate_future_forecast_plot(
        forecast_df = results$forecast_data,
        plot_title = glue(
          "{results$horizon} Weeks Forecast"
        )
      )
      
    })
    
    
    # ---------------------------------------------------------
    # Model Performance Plot
    # ---------------------------------------------------------
    
    output$model_performance_plot <- renderPlotly({
      
      results <- forecast_results()
      
      generate_model_performance_plot(
        calibration_table = results$calibration_table,
        testing_df = results$testing_data,
        original_series = results$original_data,
        plot_title = "Model Performance Plot",
        confidence_interval = results$confidence_level
      )
    })
    
    
    # ---------------------------------------------------------
    # Model Performance Table
    # ---------------------------------------------------------
    
    model_performance_metrics <- reactive({
      results <- forecast_results()
      
      results$calibration_table |> 
        genarate_model_performance_metrics_table()
    })
    
    output$model_performance_table <- render_gt({
      
      model_performance_metrics() |> 
        format_model_performance_metrics_table()
    })
    
    # ---------------------------------------------------------
    # Export Forecast data as CSV
    # ---------------------------------------------------------
    output$download_forecast <- downloadHandler(
      
      filename = function() {
        results <- forecast_results()
        glue("center-{results$center}-meal-{results$meal}.csv")
      },
      
      content = function(file) {
        
        results <- forecast_results()
        best_model_desc <- generate_best_model_description(model_performance_metrics())
        
        # Format output data
        to_download <- results$forecast_data |> 
          filter(.key == "prediction", .model_desc == best_model_desc) |> 
          mutate(
            across(
              c(.value, .conf_lo, .conf_hi), 
              round
            )
          ) |> 
          transmute(
            date = .index,
            prediction_orders = .value,
            lower_bound = .conf_lo, 
            upper_bound = .conf_hi
          )
        
        # export csv
        write.csv(to_download, file, row.names = FALSE)
      }
    )
    
  })
}