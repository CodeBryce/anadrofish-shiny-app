# My Intern Project App
# Author: B.Davis
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
  
  # Main Title
  div(style = "padding: 15px 0px;", h2(icon("fish"), "Anadromous Fish Population Simulator")),
  
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
                       withSpinner(plotOutput("results_plot", height = "500px"))
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
    trend_color <- if(final_pop > init_pop) "green" else "red"
    trend_icon <- if(final_pop > init_pop) icon("arrow-trend-up") else icon("arrow-trend-down")
    
    tagList(
      p(strong("Initial Population: "), format(round(init_pop), big.mark=",")),
      p(strong("Final Population: "), format(round(final_pop), big.mark=",")),
      p(strong("Mean Population: "), format(round(mean(yearly_totals$spawners)), big.mark=",")),
      p(strong("Peak Population: "), format(round(max(yearly_totals$spawners)), big.mark=",")),
      hr(),
      h4("Population Trend:", span(trend_icon, trend_text, style = paste0("color:", trend_color, ";")))
    )
  })
}

shinyApp(ui = ui, server = server)
