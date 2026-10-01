load_modules <- function(){
  dir_ls("R/") |> 
    map(~source(.x))
}


generate_forecasting_splits <- function(df, prop = 0.85) {
  return(
    initial_time_split(df, prop) 
  )
}

generate_anomaly_treated_df <- function(df, normal_range_width = 0.10) {
  cleaned_df <- df |> 
    group_by(center_id, meal_id) |> 
    anomalize(
      .date_var = period,
      .value = num_orders,
      .iqr_alpha = normal_range_width,
      .message = FALSE
    ) |> 
    ungroup()
  
  print(cleaned_df |> count(anomaly))
  
  return(
    cleaned_df |> 
      transmute(center_id, meal_id, period, num_orders = observed_clean)
  )
}


generate_feature_engineering_recipe <- function(training_df) {
  
  if(is.null(training_df)) stop("Training data is required")
  
  return(
    recipe(num_orders ~ period,  data = training_df) |>
      step_timeseries_signature(period) |> 
      step_rm(period) |> 
      step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
      step_zv(all_predictors()) |> 
      step_nzv(all_predictors())
  )
}


generate_model_specifications <- function() {
  
  xgb_model <- boost_tree() |> 
    set_mode("regression") |> 
    set_engine("xgboost")
  
  rf_model <- rand_forest() |> 
    set_mode("regression") |> 
    set_engine("ranger")
  
  svm_model <- svm_rbf() |> 
    set_mode("regression") |> 
    set_engine("kernlab")
  
  knn_model <- nearest_neighbor() |> 
    set_mode("regression") |> 
    set_engine("kknn")
  
  dt_model <- decision_tree() |> 
    set_mode("regression") |> 
    set_engine("rpart")
  
  return(
    list(
      xgb_model = xgb_model,
      rf_model = rf_model,
      svm_model = svm_model,
      knn_model = knn_model,
      dt_model = dt_model
    )
  )
}

generate_fitted_workflows <- function(
  model_spec_list, 
  feat_engineering_recipe,
  training_df
) {
  
  xgb_workflow <- workflow() |> 
    add_model(model_spec_list[["xgb_model"]]) |> 
    add_recipe(feat_engineering_recipe) |> 
    fit(training_df)
  
  rf_workflow <- workflow() |> 
    add_model(model_spec_list[["rf_model"]]) |> 
    add_recipe(feat_engineering_recipe) |> 
    fit(training_df)
  
  svm_workflow <- workflow() |> 
    add_model(model_spec_list[["svm_model"]]) |> 
    add_recipe(feat_engineering_recipe) |> 
    fit(training_df)
  
  knn_workflow <- workflow() |> 
    add_model(model_spec_list[["knn_model"]]) |> 
    add_recipe(feat_engineering_recipe) |> 
    fit(training_df)
  
  dt_workflow <- workflow() |> 
    add_model(model_spec_list[["dt_model"]]) |> 
    add_recipe(feat_engineering_recipe) |> 
    fit(training_df)
  
  return(
    list(
      xgb_workflow = xgb_workflow,
      rf_workflow = rf_workflow,
      svm_workflow = svm_workflow,
      knn_workflow = knn_workflow,
      dt_workflow = dt_workflow
    )
  )
}


generate_modeltime_table <- function(workflows_list) {
  return(
    modeltime_table(
      workflows_list[["xgb_workflow"]], 
      workflows_list[["rf_workflow"]],
      workflows_list[["svm_workflow"]],
      workflows_list[["knn_workflow"]],
      workflows_list[["dt_workflow"]]
    )
  )
}

generate_calibration_table <- function(models_df, testing_df) {
  return(
    modeltime_calibrate(models_df, new_data = testing_df)
  )
}

genarate_model_performance_metrics_table <- function(calibration_table) {
  return(
    calibration_table |> 
      modeltime_accuracy(metric_set = metric_set(mae, rmse, mape)) |> 
      select(-.model_id, -.type)
  )
}

format_model_performance_metrics_table <- function(model_performance_df) {
  
  model_performance_df %>%
    arrange(rmse) %>%
    gt(rowname_col = ".model_desc") %>%
    
  # Title
  tab_header(
    title = "Model Performance Comparison",
    subtitle = "Forecast accuracy across candidate models"
  ) %>%
    
  # Column labels
  cols_label(
    mae  = md("**MAE**"),
    rmse = md("**RMSE**"),
    mape = md("**MAPE**")
  ) %>%
    
    tab_stubhead(
      label = md("**Model**")
    ) %>%
    
  # Number formatting
  fmt_number(
    columns = c(mae, rmse),
    decimals = 1,
    use_seps = TRUE
  ) %>%
    
    fmt_percent(
      columns = mape,
      decimals = 1,
      scale_values = FALSE
    ) %>%
    
  # Highlight best values
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = mae,
      rows = mae == min(mae, na.rm = TRUE)
    )
  ) %>%
    
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = rmse,
        rows = rmse == min(rmse, na.rm = TRUE)
      )
    ) %>%
    
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = mape,
        rows = mape == min(mape, na.rm = TRUE)
      )
    ) %>%
    
  # Alignment
  cols_align(
    align = "left",
    columns = .model_desc
  ) %>%
    
    cols_align(
      align = "right",
      columns = c(mae, rmse, mape)
    ) %>%
    
  # Footnote
  tab_source_note(
    source_note = md(
      "*Lower values indicate better predictive performance. 
        Bold values represent the best-performing model for each metric.*"
    )
  ) %>%
    
  # Overall table styling
  tab_options(
    table.width = pct(100),
    
    heading.align = "left",
    
    heading.title.font.size = px(20),
    heading.subtitle.font.size = px(13),
    column_labels.border.top.width = px(1),
    
    source_notes.font.size = px(11),
    source_notes.padding = px(8),
    
    table.border.top.width = px(2),
    table.border.bottom.width = px(2),
    
    data_row.padding = px(13),
    column_labels.font.weight = "650",
    column_labels.border.top.style = "none",
    column_labels.border.bottom.color = "#0B538E",
    column_labels.border.bottom.width = px(1),
    
    table.font.size = px(14),
    
    table.border.top.style = "none",
    table.border.bottom.style = "none"
  )
}


generate_model_performance_plot <- function(
    calibration_table, 
    testing_df,
    original_series,
    plot_title = "Forecast Plot",
    confidence_interval = 0.90
    ) {
  return(
    calibration_table %>%
      modeltime_forecast(
        new_data    = testing_df,
        actual_data = original_series,
        conf_interval = confidence_interval,
        keep_data = T
      ) %>%
      plot_modeltime_forecast(
        .title = plot_title,
        .y_lab = "Meal Orders",
        .interactive      = T
      ) |> 
      config(displayModeBar = FALSE)
  )
}

generate_best_model_description <- function(metrics_table) {
   return(
     metrics_table |> 
       slice_min(order_by = mape) |> 
       pull(.model_desc)
   )
}

generate_future_forecast_data <- function(
    calibration_table, 
    original_series, 
    horizon_in_weeks = 1,
    confidence_interval = 0.90
    ) {
  return(
    calibration_table %>%
      modeltime_refit(data = original_series) |> 
      modeltime_forecast(
        h = horizon_in_weeks,
        actual_data = original_series,
        conf_interval = confidence_interval
      )
  )
}

generate_future_forecast_plot <- function(
    forecast_df,
    plot_title = "Forecast Plot"
    ) {
  return(
    forecast_df |> 
      plot_modeltime_forecast(
        .title = plot_title,
        .y_lab = "Meal Orders",
        .legend_max_width = 25, 
        .interactive      = T
      ) |> 
      config(displayModeBar = FALSE)
  )
}


keep_shiny_app_alive <- function(interval = 90000) {
  
  tags$script(
    HTML(
      sprintf(
        "
        setInterval(function() {
          if (
            typeof Shiny !== 'undefined' &&
            Shiny.setInputValue
          ) {
            Shiny.setInputValue(
              'keep_alive',
              Date.now(),
              {priority: 'event'}
            );
          }
        }, %s);
        ",
        interval
      )
    )
  )
}

