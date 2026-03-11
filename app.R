# My Intern Project App
# Author: B.Davis
# Date: 9/27/2025
# Purpose: Create a UI for the anadrofish package

library(shiny)
library(anadrofish)
library(ggplot2)
library(shinycssloaders)
library(bslib)

ALL_SPECIES_VECTOR <- c("Alewife" = "ALE", "American Shad" = "AMS", "Blueback Herring" = "BBH")

# ----------------------------------------------------
# PART 1: The User Interface (UI)
# ----------------------------------------------------
ui <- fluidPage(
  title = "Anadromous Fish Population Simulator",
  
  # Set the theme to a clean Bootstrap 5 design
  theme = bs_theme(
    version = 5, 
    base_font = font_google("Inter"),
    primary = "#003366" # Optional: A nice deep blue for the primary buttons
  ),

  # CSS for metric tiles, trend banner, and empty state
  tags$style(HTML("
    .metric-tile {
      background: #f8f9fa;
      border: 1px solid #e9ecef;
      border-radius: 8px;
      padding: 16px 20px;
      text-align: center;
      height: 100%;
    }
    .metric-tile .metric-label {
      font-size: 0.75rem;
      text-transform: uppercase;
      letter-spacing: 0.05em;
      color: #6c757d;
      margin-bottom: 6px;
    }
    .metric-tile .metric-value {
      font-size: 1.5rem;
      font-weight: 700;
      color: #003366;
      line-height: 1.2;
    }
    .trend-banner {
      background: #f8f9fa;
      border: 1px solid #e9ecef;
      border-radius: 8px;
      padding: 14px 20px;
      display: flex;
      align-items: center;
      gap: 10px;
      margin-top: 16px;
    }
    .trend-banner .trend-label {
      font-size: 0.85rem;
      color: #6c757d;
      margin: 0;
    }
    .trend-banner .trend-value {
      font-size: 1.1rem;
      font-weight: 600;
      margin: 0;
    }
    .empty-state {
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      height: 460px;
      color: #adb5bd;
      text-align: center;
      gap: 12px;
    }
    .empty-state i {
      font-size: 3rem;
      opacity: 0.4;
    }
    .empty-state p {
      font-size: 0.95rem;
      max-width: 280px;
      line-height: 1.5;
    }
  ")),
  
  # Help & About Modal definition
  tags$div(
    id = "helpModal",
    class = "modal fade",
    tabindex = "-1",
    tags$div(
      class = "modal-dialog modal-lg modal-dialog-scrollable",
      tags$div(
        class = "modal-content",
        tags$div(
          class = "modal-header",
          style = "background-color: #003366; color: white;",
          tags$h5(class = "modal-title", icon("circle-info"), " About the Anadromous Fish Population Simulator"),
          tags$button(type = "button", class = "btn-close btn-close-white", `data-bs-dismiss` = "modal")
        ),
        tags$div(
          class = "modal-body",
          
          # Overview
          tags$h5(icon("fish"), " What is this app?"),
          tags$p("This simulator provides an interactive interface to the ", 
                 tags$a("anadrofish", href = "https://github.com/danStich/anadrofish", target = "_blank"), 
                 " R package, which models anadromous fish population responses to dams, fisheries, and 
                 restoration activities in Atlantic coastal rivers of Canada and the United States."),
          tags$p("The core ", tags$code("sim_pop()"), " function links dam passage rates to habitat availability 
                 and stochastic population models to simulate species-specific responses across marine and 
                 freshwater habitats."),
          
          tags$hr(),
          
          # Species
          tags$h5(icon("water"), " Supported Species"),
          tags$ul(
            tags$li(tags$strong("Alewife"), tags$em(" (Alosa pseudoharengus)"), 
                    " — 222 populations in Atlantic Coastal rivers"),
            tags$li(tags$strong("American Shad"), tags$em(" (Alosa sapidissima)"), 
                    " — 167 populations; peer-reviewed in the 2020 ASMFC Benchmark Stock Assessment"),
            tags$li(tags$strong("Blueback Herring"), tags$em(" (Alosa aestivalis)"), 
                    " — 238 populations; peer-reviewed in the 2024 ASMFC River Herring Benchmark Stock Assessment")
          ),
          tags$p("Rivers range from Florida, USA (St. Johns River) to Quebec, Canada (St. Lawrence drainage). 
                 Use ", tags$code("get_rivers()"), " in R to see the full list per species."),
          
          tags$hr(),
          
          # Parameters guide
          tags$h5(icon("sliders"), " Parameter Guide"),
          tags$table(
            class = "table table-sm table-bordered",
            tags$thead(tags$tr(tags$th("Parameter"), tags$th("Description"))),
            tags$tbody(
              tags$tr(tags$td(tags$strong("Number of Years")), 
                      tags$td("How many years the simulation runs. More years allow the population to stabilize. Recommended: 50+ years.")),
              tags$tr(tags$td(tags$strong("Initial Population Size")), 
                      tags$td("Starting number of spawning adults (n_init). Typically 10,000–1,000,000 depending on the river.")),
              tags$tr(tags$td(tags$strong("Sex Ratio (sr)")), 
                      tags$td("Proportion of females in the population. 0.5 means an equal 50/50 male-to-female split.")),
              tags$tr(tags$td(tags$strong("SR Parameter (b)")), 
                      tags$td("Beverton-Holt stock-recruitment parameter controlling density dependence. Lower values = stronger density dependence.")),
              tags$tr(tags$td(tags$strong("Upstream Passage (Adults)")), 
                      tags$td("Percentage of adult fish that successfully pass upstream through dams (0–100%). 100% = no dams.")),
              tags$tr(tags$td(tags$strong("Downstream Passage (Adults)")), 
                      tags$td("Percentage of adult fish surviving downstream passage through dams after spawning.")),
              tags$tr(tags$td(tags$strong("Downstream Passage (Juveniles)")), 
                      tags$td("Percentage of juvenile fish surviving their downstream migration through dams to the ocean.")),
              tags$tr(tags$td(tags$strong("Output Years")), 
                      tags$td(tags$strong("All Years:"), " returns spawner count for each year (good for trend analysis). ",
                              tags$strong("Final Year Only:"), " returns age-structured output for the last simulated year.")),
              tags$tr(tags$td(tags$strong("Sex-Specific Model")), 
                      tags$td("When enabled, the model tracks males and females separately for greater biological realism."))
            )
          ),
          
          tags$hr(),
          
          # Custom habitat
          tags$h5(icon("file-csv"), " Using a Custom Habitat CSV"),
          tags$p("You can supply your own river habitat data instead of using built-in rivers. 
                 Click ", tags$strong("Download Template"), " in the Habitat tab to get a properly 
                 formatted CSV for your selected species. Fill in the habitat area (", tags$code("Hab_sqkm"), 
                 ") and dam ordering (", tags$code("dam_order"), ") for each segment, then upload it."),
          tags$p(tags$strong("Key columns:"), " ", tags$code("river"), ", ", tags$code("region"), ", ",
                 tags$code("govt"), ", ", tags$code("dam_order"), " (number of dams from each segment to the first), ",
                 "and ", tags$code("Hab_sqkm"), " (habitat surface area in square kilometers)."),
          

        ),
        tags$div(
          class = "modal-footer",
          tags$a("View on GitHub", href = "https://github.com/danStich/anadrofish", 
                 target = "_blank", class = "btn btn-outline-secondary btn-sm"),
          tags$button(type = "button", class = "btn btn-primary btn-sm", 
                      `data-bs-dismiss` = "modal", "Close")
        )
      )
    )
  ),
  
  # Main Title row with Help button
  div(
    style = "padding: 15px 0px; display: flex; align-items: center; justify-content: space-between;",
    h2(icon("fish"), "Anadromous Fish Population Simulator"),
    tags$button(
      type = "button",
      class = "btn btn-outline-primary btn-sm",
      `data-bs-toggle` = "modal",
      `data-bs-target` = "#helpModal",
      icon("circle-info"), " Help & About"
    )
  ),
  
  fluidRow(
    
    # --- LEFT COLUMN: The Input Controls ---
    column(width = 4,
           
           # This replaces the chunky pills with a sleek underline nav bar
           navset_card_underline(
             id = "input_tabs",
             
             # TAB 1: Habitat
             nav_panel("Habitat", icon = icon("map-location-dot"),
                       br(),
                       selectInput("species_input", "Select Species:", choices = ALL_SPECIES_VECTOR),
                       radioButtons("habitat_source", "Habitat Source:",
                                    choices = c("Built-in River" = "builtin", "Custom (Upload CSV)" = "custom")),
                       
                       conditionalPanel(
                         condition = "input.habitat_source == 'builtin'",
                         selectInput("river_input", "Select River:", choices = NULL)
                       ),
                       
                       conditionalPanel(
                         condition = "input.habitat_source == 'custom'",
                         fileInput("custom_csv", "Upload Habitat CSV", accept = ".csv"),
                         downloadButton("downloadTemplate", "Download Template", class = "btn-secondary btn-sm w-100")
                       )
             ),
             
             # TAB 2: Parameters
             nav_panel("Parameters", icon = icon("sliders"),
                       br(),
                       numericInput("nyears_input", "Number of Years:", value = 50, min = 10, max = 500),
                       numericInput("n_init_input", "Initial Population Size:", value = 10000, min = 100),
                       numericInput("sr_input", "Sex ratio:", value = 0.50, min = 0.01, max = 0.99),
                       sliderInput("b_input", "SR Parameter (b):", min = 0.01, max = 0.2, value = 0.05, step = 0.01)
             ),
             
             # TAB 3: Dam Passage
             nav_panel("Passage", icon = icon("water"),
                       br(),
                       sliderInput("upstream_input", "Upstream (Adults)", min = 0, max = 100, value = 90),
                       sliderInput("downstream_input", "Downstream (Adults)", min = 0, max = 100, value = 90),
                       sliderInput("downstream_j_input", "Downstream (Juv.)", min = 0, max = 100, value = 90),
                       hr(),
                       radioButtons("output_years_input", "Output Years:",
                                    choices = c("All Years" = "all", "Final Year Only" = "last"), inline = TRUE),
                       checkboxInput("sex_specific_input", "Use Sex-Specific Model", value = TRUE)
             )
           ),
           
           # ACTION CARD: Dedicated area for running/downloading
           card(
             card_body(
               actionButton("run_button", "Run Simulation", icon = icon("play"), 
                            class = "btn-primary w-100 mb-2", style = "font-size: 16px; font-weight: bold;"),
               downloadButton("downloadData", "Download Results CSV", class = "w-100")
             )
           )
    ),
    
    # --- RIGHT COLUMN: The Output (Graph & Summary) ---
    column(width = 8,
           
           # Using the same underline nav style here so the whole app matches
           navset_card_underline(
             
             nav_panel("Plot Output", icon = icon("chart-line"),
                       br(),
                       h4(textOutput("plot_title"), align = "center"),
                       uiOutput("plot_or_empty")
             ),
             
             nav_panel("Summary Statistics", icon = icon("list"),
                       br(),
                       h3("Simulation Results Summary"),
                       hr(),
                       withSpinner(uiOutput("summary_stats"))
             )
           )
    )
  )
)

# ----------------------------------------------------
# PART 2: The Server (Unchanged from our last fix)
# ----------------------------------------------------
server <- function(input, output, session) {
  
  observeEvent(input$species_input, {
    rivers_for_dropdown <- get_rivers(input$species_input)
    updateSelectInput(session = session, inputId = "river_input",
                      choices = rivers_for_dropdown, selected = rivers_for_dropdown[1])
  })
  
  output$downloadTemplate <- downloadHandler(
    filename = function() { paste0("habitat_template_", input$species_input, ".csv") },
    content = function(file) {
      template_df <- anadrofish::custom_habitat_template(
        species = input$species_input, built_in = FALSE, river = "MyCustomRiver" 
      )
      write.csv(template_df, file, row.names = FALSE)
    }
  )
  
  simulation_from_model <- eventReactive(input$run_button, {
    req(input$upstream_input, input$downstream_input, input$downstream_j_input)
    
    up_passage <- input$upstream_input / 100
    down_passage <- input$downstream_input / 100
    down_j_passage <- input$downstream_j_input / 100
    
    results <- NULL
    
    if (input$habitat_source == "builtin") {
      req(input$river_input)
      results <- anadrofish::sim_pop(
        species = input$species_input, river = input$river_input,
        nyears = input$nyears_input, n_init = input$n_init_input,
        sr = input$sr_input, b = input$b_input,
        upstream = up_passage, downstream = down_passage, downstream_j = down_j_passage,
        output_years = input$output_years_input, sex_specific = input$sex_specific_input
      )
      
    } else {
      req(input$custom_csv) 
      custom_habitat_df <- read.csv(input$custom_csv$datapath)
      custom_habitat_df <- custom_habitat_df[!is.na(custom_habitat_df$river) & custom_habitat_df$river != "", ]
      if (nrow(custom_habitat_df) == 0) stop("Uploaded CSV is empty")
      
      uploaded_river_name <- custom_habitat_df$river[1] 
      
      results <- anadrofish::sim_pop(
        species = input$species_input,
        river = uploaded_river_name, 
        custom_habitat = custom_habitat_df, 
        nyears = input$nyears_input, n_init = input$n_init_input,
        sr = input$sr_input, b = input$b_input,
        upstream = up_passage, downstream = down_passage, downstream_j = down_j_passage,
        output_years = input$output_years_input, sex_specific = input$sex_specific_input
      )
    }
    return(results)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      river_label <- if(input$habitat_source == "builtin") input$river_input else "custom_river"
      paste0("anadrofish_data_", input$species_input, "_", river_label, "_", Sys.Date(), ".csv")
    },
    content = function(file) { write.csv(simulation_from_model(), file, row.names = FALSE) }
  )
  
  output$plot_title <- renderText({
    simulation_from_model()
    species_name <- names(ALL_SPECIES_VECTOR[ALL_SPECIES_VECTOR == input$species_input])
    if (input$habitat_source == "builtin") {
      req(input$river_input)
      paste("Simulation for", species_name, "in the", input$river_input)
    } else {
      paste("Simulation for", species_name, "in Custom Habitat Configuration")
    }
  })

  # Renders either the plot or a friendly empty state before first run
  output$plot_or_empty <- renderUI({
    if (input$run_button == 0) {
      div(class = "empty-state",
        icon("chart-line"),
        p("Configure your habitat, parameters, and passage rates on the left, then click",
          strong(" Run Simulation"), " to see results here.")
      )
    } else {
      withSpinner(plotOutput("results_plot", height = "500px"))
    }
  })

  output$results_plot <- renderPlot({
    the_plot_data <- simulation_from_model()
    req(the_plot_data)
    
    if (input$output_years_input == "all") {
      ggplot(data=the_plot_data, aes(x = year, y = spawners)) +
        geom_line(color = "#003366", linewidth = 1.2) + # Changed to match the theme color
        labs(subtitle = "Total spawner abundance over time", x = "Year", y = "Number of Spawners") +
        theme_minimal(base_size = 16)
    } else {
      ggplot(data=the_plot_data, aes(x = as.factor(age), y = spawners)) +
        geom_col(fill = "#003366", alpha = 0.8) +
        labs(subtitle = "Spawner abundance by age (Final Year)", x = "Age Class", y = "Number of Spawners") +
        theme_minimal(base_size = 16)
    }
  })
  
  output$summary_stats <- renderUI({
    df <- simulation_from_model()
    req(df)
    
    yearly_totals <- aggregate(spawners ~ year, data = df, sum)
    init_pop <- head(yearly_totals$spawners, 1)
    final_pop <- tail(yearly_totals$spawners, 1)
    
    trend_text <- if(final_pop > init_pop) "Increasing" else "Decreasing"
    trend_color <- if(final_pop > init_pop) "#198754" else "#dc3545"
    trend_icon <- if(final_pop > init_pop) icon("arrow-trend-up") else icon("arrow-trend-down")

    # Metric tiles for key stats
    tagList(
      fluidRow(
        column(3, div(class = "metric-tile",
          div(class = "metric-label", "Initial Population"),
          div(class = "metric-value", format(round(init_pop), big.mark = ","))
        )),
        column(3, div(class = "metric-tile",
          div(class = "metric-label", "Final Population"),
          div(class = "metric-value", format(round(final_pop), big.mark = ","))
        )),
        column(3, div(class = "metric-tile",
          div(class = "metric-label", "Mean Population"),
          div(class = "metric-value", format(round(mean(yearly_totals$spawners)), big.mark = ","))
        )),
        column(3, div(class = "metric-tile",
          div(class = "metric-label", "Peak Population"),
          div(class = "metric-value", format(round(max(yearly_totals$spawners)), big.mark = ","))
        ))
      ),
      div(class = "trend-banner",
        trend_icon,
        p(class = "trend-label", "Population Trend:"),
        p(class = "trend-value", style = paste0("color:", trend_color, ";"), trend_text)
      )
    )
  })
}

shinyApp(ui = ui, server = server)
