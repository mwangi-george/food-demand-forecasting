about_stat <- function(icon_name, value_output, label, detail) {
  div(
    class = "about-stat",
    div(class = "about-stat__icon", bsicons::bs_icon(icon_name)),
    div(
      class = "about-stat__content",
      div(value_output, class = "about-stat__value"),
      div(label, class = "about-stat__label"),
      div(detail, class = "about-stat__detail")
    )
  )
}


about_pipeline_step <- function(number, icon_name, title, description) {
  div(
    class = "about-step",
    div(class = "about-step__number", number),
    div(class = "about-step__icon", bsicons::bs_icon(icon_name)),
    h3(title),
    p(description)
  )
}


about_model_chip <- function(short_name, model_name, engine_name) {
  div(
    class = "about-model",
    div(short_name, class = "about-model__mark"),
    div(
      div(model_name, class = "about-model__name"),
      div(engine_name, class = "about-model__engine")
    )
  )
}


about_page_ui <- function(id, meals_df) {
  ns <- NS(id)
  
  observation_count <- scales::comma(nrow(meals_df))
  center_count <- scales::comma(dplyr::n_distinct(meals_df$center_id))
  meal_count <- scales::comma(dplyr::n_distinct(meals_df$meal_id))
  total_orders <- scales::comma(sum(meals_df$num_orders, na.rm = TRUE))
  
  date_range <- paste0(
    format(min(meals_df$period, na.rm = TRUE), "%b %Y"),
    " — ",
    format(max(meals_df$period, na.rm = TRUE), "%b %Y")
  )

  tagList(
    tags$head(includeCSS("www/about.css")),

    div(
      class = "about-page",

      tags$section(
        class = "about-hero",
        div(
          class = "about-hero__content",
          div(
            class = "about-eyebrow about-eyebrow--light",
            span(class = "about-eyebrow__dot"),
            "Meal Orders Demand forecasting Tool"
          ),
          h1(
            "From historical orders to ",
            span("clearer planning decisions."),
            class = "about-hero__title"
          ),
          p(
            paste(
              "An end-to-end analytical application for exploring weekly meal demand,",
              "diagnosing unusual observations, and comparing machine-learning forecasts",
              "at the center–meal level."
            ),
            class = "about-hero__lede"
          ),
          div(
            class = "about-hero__actions",
            tags$a(
              href = "https://www.kaggle.com/datasets/kannanaikkal/food-demand-forecasting",
              target = "_blank",
              rel = "noopener noreferrer",
              class = "about-button about-button--primary",
              bsicons::bs_icon("database"),
              "View source dataset",
              bsicons::bs_icon("arrow-up-right")
            ),
            tags$a(
              href = "https://github.com/mwangi-george/food-demand-forecasting",
              target = "_blank",
              rel = "noopener noreferrer",
              class = "about-button about-button--primary",
              bsicons::bs_icon("code-square"),
              "View source code",
              bsicons::bs_icon("arrow-up-right")
            ),
            div(
              class = "about-hero__hint",
              bsicons::bs_icon("compass"),
              span("Explore the data, then run a forecast from the navigation above.")
            )
          )
        ),

        div(
          class = "about-hero__visual",
          div(
            class = "about-signal",
            div(
              class = "about-signal__topline",
              span("DEMAND SIGNAL"),
              span(class = "about-signal__status", "ANALYSIS READY")
            ),
            div(
              class = "about-signal__chart",
              tags$svg(
                viewBox = "0 0 520 210",
                role = "img",
                `aria-label` = "Stylized weekly demand signal",
                preserveAspectRatio = "none",
                tags$defs(
                  tags$linearGradient(
                    id = ns("areaGradient"),
                    x1 = "0", y1 = "0", x2 = "0", y2 = "1",
                    tags$stop(offset = "0%", `stop-color` = "#53c7d8", `stop-opacity` = "0.34"),
                    tags$stop(offset = "100%", `stop-color` = "#53c7d8", `stop-opacity` = "0")
                  )
                ),
                tags$path(
                  class = "about-signal__grid",
                  d = "M0 35H520 M0 87H520 M0 139H520 M0 191H520"
                ),
                tags$path(
                  class = "about-signal__area",
                  fill = sprintf("url(#%s)", ns("areaGradient")),
                  d = paste(
                    "M0 174 L22 165 L43 170 L65 145 L87 151 L108 122",
                    "L130 132 L152 108 L173 118 L195 83 L217 95 L238 72",
                    "L260 91 L282 56 L303 69 L325 38 L347 57 L368 44",
                    "L390 67 L412 42 L433 48 L455 21 L477 38 L498 18 L520 31",
                    "L520 210 L0 210 Z"
                  )
                ),
                tags$path(
                  class = "about-signal__line",
                  d = paste(
                    "M0 174 L22 165 L43 170 L65 145 L87 151 L108 122",
                    "L130 132 L152 108 L173 118 L195 83 L217 95 L238 72",
                    "L260 91 L282 56 L303 69 L325 38 L347 57 L368 44",
                    "L390 67 L412 42 L433 48 L455 21 L477 38 L498 18 L520 31"
                  )
                ),
                tags$circle(cx = "455", cy = "21", r = "6", class = "about-signal__point"),
                tags$circle(cx = "455", cy = "21", r = "12", class = "about-signal__pulse")
              )
            ),
            div(
              class = "about-signal__footer",
              div(span("GRAIN"), strong("Center × Meal × Week")),
              div(span("OUTPUT"), strong("Forecast + interval"))
            )
          ),
          div(class = "about-orbit about-orbit--one"),
          div(class = "about-orbit about-orbit--two")
        )
      ),

      tags$section(
        class = "about-stats",
        about_stat(
          "table", observation_count,
          "Weekly observations", "The analysis-ready project extract"
        ),
        about_stat(
          "building", center_count,
          "Fulfilment centers", "Distinct operational locations"
        ),
        about_stat(
          "cup-hot", meal_count,
          "Meal identifiers", "Distinct items represented"
        ),
        about_stat(
          "calendar3", date_range,
          "Weekly coverage", "Observed period in this application"
        )
      ),

      tags$section(
        class = "about-section about-analysis",
        div(
          class = "about-analysis__heading",
          div(
            div("Dataset profile", class = "about-eyebrow"),
            h2("The demand history behind the forecasts."),
            p(
              paste(
                "An aggregate-level view of weekly order volume and its distribution",
                "before the analysis moves into individual center–meal series."
              )
            )
          ),
          div(
            class = "about-analysis__actions",
            div(
              class = "about-analysis__metric",
              div(class = "about-analysis__metric-icon", bsicons::bs_icon("cart-check")),
              div(
                span("TOTAL ORDERS"),
                strong(total_orders)
              )
            ),
            downloadButton(
              ns("download_meals_dataset"),
              "Download dataset",
              class = "about-download"
            )
          )
        ),
        div(
          class = "about-analysis__grid",
          div(
            class = "about-analysis-card about-analysis-card--chart",
            div(
              class = "about-analysis-card__header",
              div(
                h3("Total weekly orders"),
                p("Aggregated across all centers and meals")
              ),
              span(class = "about-analysis-card__badge", "WEEKLY")
            ),
            plotlyOutput(ns("weekly_demand_plot"), height = "400px") |>
              withSpinner(type = 4, size = 0.45)
          ),
          div(
            class = "about-analysis-card about-analysis-card--table",
            div(
              class = "about-analysis-card__header",
              div(
                h3("Descriptive statistics"),
                p("Distribution of weekly meal orders")
              )
            ),
            gt_output(ns("descriptive_statistics_table"))
          )
        )
      ),

      tags$section(
        class = "about-section about-story",
        div(
          class = "about-section__intro",
          div("The project", class = "about-eyebrow"),
          h2("A focused decision-support workflow, not just another dashboard."),
          p(
            paste(
              "Food demand is noisy, local, and time-dependent. A single headline total",
              "can hide the patterns planners actually need. This application keeps the",
              "analysis at the center–meal level so users can move from portfolio context",
              "to a specific operational question."
            )
          )
        ),
        div(
          class = "about-story__cards",
          div(
            class = "about-story-card about-story-card--accent",
            div(class = "about-story-card__icon", bsicons::bs_icon("bullseye")),
            div(
              div("THE QUESTION", class = "about-story-card__label"),
              h3("How much demand should a center prepare for next?"),
              p(
                "The workflow turns historical weekly orders into a comparable set of forecasts with visible uncertainty."
              )
            )
          ),
          div(
            class = "about-story-card",
            div(class = "about-story-card__icon", bsicons::bs_icon("person-check")),
            div(
              div("THE USER", class = "about-story-card__label"),
              h3("An analyst or operations planner"),
              p(
                "The interface favors traceability: select a series, inspect its history, compare candidates, then export the result."
              )
            )
          )
        )
      ),

      tags$section(
        class = "about-section about-workflow",
        div(
          class = "about-section__heading",
          div(
            div("How it works", class = "about-eyebrow"),
            h2("A transparent path from raw history to forecast.")
          ),
          p(
            paste(
              "Each stage is represented in the application, making the analytical",
              "process easier to inspect and explain."
            )
          )
        ),
        div(
          class = "about-steps",
          about_pipeline_step(
            "01", "database-check", "Profile",
            "Validate coverage, volume, and time-series granularity before modeling."
          ),
          about_pipeline_step(
            "02", "activity", "Explore",
            "Inspect weekly movement and surface unusual observations for a selected series."
          ),
          about_pipeline_step(
            "03", "bezier2", "Engineer",
            "Create calendar-based predictors from the weekly date index for reproducible training."
          ),
          about_pipeline_step(
            "04", "speedometer2", "Compare",
            "Calibrate five regression algorithms on a time-ordered holdout using MAE, RMSE, and MAPE."
          ),
          about_pipeline_step(
            "05", "graph-up-arrow", "Forecast",
            "Refit on available history, project the chosen horizon, and export prediction intervals."
          )
        )
      ),

      tags$section(
        class = "about-section about-modeling",
        div(
          class = "about-modeling__copy",
          div("Model workbench", class = "about-eyebrow about-eyebrow--light"),
          h2("Five different learning strategies. One comparable evaluation frame."),
          p(
            paste(
              "The forecasting module fits a deliberately varied candidate set rather than",
              "assuming one algorithm will be best for every center–meal series. Results are",
              "ranked on the holdout period, while forecast intervals communicate a plausible",
              "range around point estimates."
            )
          ),
          div(
            class = "about-modeling__note",
            bsicons::bs_icon("shield-check"),
            span("Time-ordered splitting helps prevent future observations from leaking into model training.")
          )
        ),
        div(
          class = "about-model-grid",
          about_model_chip("XGB", "Gradient boosting", "xgboost"),
          about_model_chip("RF", "Random forest", "ranger"),
          about_model_chip("SVM", "Radial SVM", "kernlab"),
          about_model_chip("KNN", "Nearest neighbors", "kknn"),
          about_model_chip("DT", "Decision tree", "rpart")
        )
      ),

      tags$section(
        class = "about-section about-details",
        div(
          class = "about-data-card",
          div(
            class = "about-card-heading",
            div(class = "about-card-heading__icon", bsicons::bs_icon("braces")),
            div(
              div("Data contract", class = "about-eyebrow"),
              h2("Four fields, one clear analytical grain.")
            )
          ),
          div(
            class = "about-dictionary",
            div(class = "about-dictionary__row about-dictionary__row--head",
                span("Field"), span("Meaning"), span("Type")),
            div(class = "about-dictionary__row",
                tags$code("center_id"), span("Fulfilment center identifier"), span("ID")),
            div(class = "about-dictionary__row",
                tags$code("meal_id"), span("Meal identifier"), span("ID")),
            div(class = "about-dictionary__row",
                tags$code("period"), span("Week-start observation date"), span("Date")),
            div(class = "about-dictionary__row",
                tags$code("num_orders"), span("Observed meal orders"), span("Integer"))
          ),
          div(
            class = "about-source-note",
            bsicons::bs_icon("link-45deg"),
            span(
              "Source: ",
              tags$a(
                "Food Demand Forecasting on Kaggle",
                href = "https://www.kaggle.com/datasets/kannanaikkal/food-demand-forecasting",
                target = "_blank",
                rel = "noopener noreferrer"
              ),
              ". Statistics shown here describe the analysis-ready extract bundled with this project."
            )
          )
        ),
        div(
          class = "about-build-card",
          div("Built with", class = "about-eyebrow"),
          h2("A reproducible R analytics stack."),
          p(
            "The application combines a modular Shiny interface with a tidymodels and modeltime forecasting workflow."
          ),
          div(
            class = "about-tech-list",
            span("R"), span("Shiny"), span("bslib"), span("tidymodels"),
            span("modeltime"), span("Plotly"), span("timetk"), span("GT")
          ),
          div(
            class = "about-build-card__footer",
            bsicons::bs_icon("boxes"),
            div(
              strong("Modular by design"),
              span("Exploration, forecasting, and project context are separated into reusable modules.")
            )
          )
        )
      ),

      tags$section(
        class = "about-section about-scope",
        div(class = "about-scope__icon", bsicons::bs_icon("info-circle")),
        div(
          class = "about-scope__copy",
          div("Responsible interpretation", class = "about-eyebrow"),
          h2("What this application does—and what it does not claim."),
          p(
            paste(
              "This is a portfolio demonstration and exploratory decision-support tool.",
              "Forecast quality can vary by series, and the current models learn from calendar",
              "and order history without explicit price, promotion, stock, holiday, weather, or",
              "operational-capacity inputs. Production use would require data validation,",
              "backtesting across multiple windows, monitoring, and review by domain owners."
            )
          )
        ),
        div(
          class = "about-scope__checks",
          div(bsicons::bs_icon("check2"), span("Useful for exploration and model comparison")),
          div(bsicons::bs_icon("check2"), span("Prediction intervals are shown alongside forecasts")),
          div(bsicons::bs_icon("dash"), span("Not a substitute for inventory or procurement controls"))
        )
      ),

      tags$footer(
        class = "about-footer",
        div(
          div("MEAL DEMAND INTELLIGENCE", class = "about-footer__brand"),
          p("Designed to make an analytical workflow easy to inspect, use, and discuss.")
        ),
        tags$a(
          href = "https://www.kaggle.com/datasets/kannanaikkal/food-demand-forecasting",
          target = "_blank",
          rel = "noopener noreferrer",
          "Dataset Source",
          bsicons::bs_icon("arrow-up-right")
        )
      )
    )
  )
}


about_page_server <- function(id, meals_df, show_smooth_line) {
  moduleServer(id, function(input, output, session) {
    dataset <- reactive({
      req(meals_df())
      meals_df() |>
        arrange(center_id, meal_id, period)
    })

    descriptive_statistics <- reactive({
      orders <- dataset()$num_orders
      orders <- orders[!is.na(orders)]
      req(length(orders) > 0)

      tibble(
        Statistic = c(
          "Minimum", "1st Quartile", "Median", "Mean",
          "3rd Quartile", "Maximum", "Std. Deviation", "IQR"
        ),
        Value = c(
          min(orders),
          unname(quantile(orders, probs = 0.25)),
          median(orders),
          mean(orders),
          unname(quantile(orders, probs = 0.75)),
          max(orders),
          sd(orders),
          IQR(orders)
        )
      )
    })

    output$descriptive_statistics_table <- render_gt({
      descriptive_statistics() |>
        gt() |>
        cols_label(Statistic = "Statistic", Value = "Orders") |>
        fmt_number(columns = Value, decimals = 1, use_seps = TRUE) |>
        cols_align(align = "left", columns = Statistic) |>
        cols_align(align = "right", columns = Value) |>
        tab_style(
          style = list(
            cell_fill(color = "#eaf5fc"),
            cell_text(color = "#0B538E", weight = "650")
          ),
          locations = cells_column_labels(columns = everything())
        ) |>
        tab_style(
          style = cell_text(color = "#0B538E", weight = "600"),
          locations = cells_body(columns = Statistic)
        ) |>
        tab_options(
          table.width = pct(100),
          data_row.padding = px(11),
          column_labels.font.weight = "650",
          column_labels.border.top.style = "none",
          column_labels.border.bottom.color = "#c8dce9",
          column_labels.border.bottom.width = px(1),
          table.font.size = px(13),
          table.border.top.style = "none",
          table.border.bottom.style = "none"
        )
    })

    output$weekly_demand_plot <- renderPlotly({
      plot_data <- dataset() |>
        filter(!is.na(period)) |>
        group_by(period) |>
        summarise(total_orders = sum(num_orders, na.rm = TRUE), .groups = "drop")

      plot_data |>
        plot_time_series(
          .date_var = period,
          .value = total_orders,
          .title = "",
          .y_lab = "Orders",
          .smooth = isTRUE(show_smooth_line())
        ) |>
        config(displayModeBar = FALSE, responsive = TRUE)
    })

    output$download_meals_dataset <- downloadHandler(
      filename = function() {
        glue("meals_fulfillment_dataset_{today()}.csv")
      },
      content = function(file) {
        write.csv(dataset(), file, row.names = FALSE)
      }
    )
  })
}
