source("global.R")
source("utils.R")

load_modules()

ui <- page_navbar(
  
  keep_shiny_app_alive(),
  
  title = "Meal Demand Intelligence & Forecasting Platform",
  selected = "About",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  sidebar = sidebar(
    title = "Configuration",
    open = FALSE,
    width = 320,

    # ---------------------------------------------------------
    # Anomaly detection
    # ---------------------------------------------------------

    div(
      class = "border rounded-2 p-2 mb-2",
      style = "border-color: white",
      div(
        class = "d-flex align-items-center gap-2 mb-1",
        bsicons::bs_icon("exclamation-diamond", style = "color: #0B538E;"),
        h6("Anomaly Detection", class = "fw-semibold mb-0")
      ),
      p(
        "Adjust how unusual observations are identified.",
        class = "text-muted small mb-3"
      ),
      sliderTextInput(
        inputId = "iqr_alpha",
        label = "Detection sensitivity",
        choices = c(0.01, 0.05, 0.10, 0.15, 0.20),
        selected = 0.10,
        grid = TRUE,
        width = "100%"
      ),
      div(
        class = "mt-2 p-2 rounded-2",
        style = "
        background-color: #e6f2fd;
        color: #0B538E;
        font-size: 0.78rem;
        line-height: 1.4;
      ",
        bsicons::bs_icon(
          "info-circle",
          style = "
          margin-right: 4px;
          vertical-align: -2px;
        "
        ),
        HTML(
          paste(
            "<strong>Lower values</strong> use a more conservative",
            "normal range. <strong>Higher values</strong> reduce the",
            "chance of normal observations being flagged as unusual."
          )
        )
      )
    ),


    # ---------------------------------------------------------
    # Chart options
    # ---------------------------------------------------------

    div(
      class = "border rounded-2 p-2 mb-2",
      style = "border-color: white",
      div(
        class = "d-flex align-items-center gap-2 mb-1",
        bsicons::bs_icon("graph-up", style = "color: #0B538E;"),
        h6("Chart Options", class = "fw-semibold mb-0")
      ),
      p(
        "Customize how demand trends are displayed.",
        class = "text-muted small mb-3"
      ),
      div(
        class = "d-flex justify-content-between align-items-center",
        div(
          class = "pe-3",
          div("Smoothed trend line", class = "fw-medium small"),
          div(
            "Overlay a smoother to highlight the underlying trend.",
            class = "text-muted",
            style = "
            font-size: 0.75rem;
            line-height: 1.35;
          "
          )
        ),
        materialSwitch(
          inputId = "show_smooth_line",
          label = NULL,
          value = TRUE,
          status = "info",
          inline = TRUE
        )
      )
    )
  ),
  
  nav_spacer(),
  
  nav_panel(
    "Explore",
    icon = icon("chart-line"),
    explore_page_ui("explore_page"),
  ),
  nav_panel(
    "Forecast",
    icon = icon("chart-line"),
    forecast_page_ui("forecast_page")
  ),
  nav_panel(
    "About",
    icon = icon("circle-info"),
    about_page_ui("about_page", meals_info_df)
  )
)


server <- function(input, output, session) {
  
  # Log keep-alive heartbeats to verify the Shiny session remains active
  observeEvent(input$keep_alive, {
    
    cat(
      "Keep-alive heartbeat received:",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      "\n"
    )
    
  }, ignoreInit = TRUE)
  
  
  app_data <- reactive({meals_info_df})

  observeEvent(list(input$iqr_alpha, input$show_smooth_line), {
    explore_page_server(
      "explore_page", 
      app_data(), 
      input$iqr_alpha, 
      input$show_smooth_line
      )
  })

  forecast_page_server("forecast_page", app_data())
  
  about_page_server(
    "about_page",
    meals_df = app_data,
    show_smooth_line = reactive(input$show_smooth_line)
  )
}


shinyApp(
  ui = ui,
  server = server,
  options = list(port = 3434),
  onStart = function() {
    print("Starting App...")

    # load global variables
    source("global.R")

    onStop(fun = function() {
      print("App is shutting down...")
    })
  },
)
