
# Necessary packages
pacman::p_load(
  fs,
  glue,
  shiny,
  shinycssloaders,
  shinyWidgets,
  bslib,
  bsicons,
  timetk,
  plotly,
  gt,
  DT,
  tidyverse,
  tidymodels,
  modeltime,
  
  # internal modeltime dependencies
  ranger,
  rpart,
  kernlab,
  xgboost,
  kknn
)


# Read dataset from disk
meals_info_df <- read_csv("data/meals_dataset.csv", show_col_types = FALSE) |> 
  arrange(center_id, meal_id)

meal_ids <- meals_info_df |> 
  distinct(meal_id) |>
  pull(meal_id)


center_ids <- meals_info_df |> 
  distinct(center_id) |>
  pull(center_id)

