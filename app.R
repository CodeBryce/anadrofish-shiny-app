# My Intern Project App
# Author: B.Davis
# Date: 9/27/2025
# Purpose: Create a UI for the anadrofish package

# First, I need to load all the packages the app will use
library(shiny)
library(anadrofish)
library(ggplot2)
library(shinycssloaders)
library(bslib)

# this is a global variable. holds all the species names for the dropdown
ALL_SPECIES_VECTOR <- c("Alewife" = "ALE", "American Shad" = "AMS", "Blueback Herring" = "BBH")


# ----------------------------------------------------
# PART 1: The User Interface (UI)
# ----------------------------------------------------
ui <- fluidPage(
  
  # Title for the whole page
  titlePanel("Anadromous Fish Population Simulator"),
  
  # using a sidebar layout
  sidebarLayout(
    
    # this is the sidebar where all the buttons and sliders go
    sidebarPanel(
      h4("Simulation Controls"),
      
      # Dropdown for species
      selectInput(inputId = "species_input", 
                  label = "Select Species:", 
                  choices = ALL_SPECIES_VECTOR),
      
      # --- NEW: Habitat Source Toggle ---
      radioButtons(inputId = "habitat_source", "Habitat Source:",
                   choices = c("Built-in River" = "builtin", 
                               "Custom (Upload CSV)" = "custom")),
      
      # --- OPTION A: Built-in River (Classic Mode) ---
      conditionalPanel(
        condition = "input.habitat_source == 'builtin'",
        selectInput(inputId = "river_input", 
                    label = "Select River:", 
                    choices = NULL) # It starts empty
      ),
      
      # --- OPTION B: Custom Mode ---
      conditionalPanel(
        condition = "input.habitat_source == 'custom'",
        helpText("Upload a CSV file formatted for anadrofish."),
        
        fileInput(inputId = "custom_csv", label = "Upload Habitat CSV", 
                  accept = ".csv"),
        
        # Button to download a blank template for the user to fill out
        downloadButton("downloadTemplate", "Download Template CSV", 
                       class = "btn-secondary btn-sm w-100 mb-3")
      ),
      
      hr(),
      
      # inputs for the simulation time and starting fish number
      numericInput(inputId = "nyears_input", label = "Number of Years:", value = 50, min = 10, max = 500),
      numericInput(inputId = "n_init_input", label = "Initial Population Size:", value = 10000, min = 100),
      
      # Sex Ratio Model Controls
      numericInput(inputId = "sr_input", "Sex ratio:", 
                   value = 0.50, min = 0.01, max = 0.99),
      
      sliderInput(inputId = "b_input", "SR Parameter (b):", min = 0.01, max = 0.2, value = 0.05, step = 0.01),
      
      # Sliders for dam passage
      h5("Dam Passage Survival (%)"),
      sliderInput(inputId = "upstream_input", label = "Upstream (Adults)", min = 0, max = 100, value = 90),
      sliderInput(inputId = "downstream_input", label = "Downstream (Adults)", min = 0, max = 100, value = 90),
      sliderInput(inputId = "downstream_j_input", label = "Downstream (Juveniles)", min = 0, max = 100, value = 90),
      
      # other settings
      radioButtons(inputId = "output_years_input", "Output Years:",
                   choices = c("All Years" = "all", "Final Year Only" = "last"), 
                   selected = "all"),
      
      checkboxInput(inputId = "sex_specific_input", "Use Sex-Specific Model", value = TRUE),
      
      # The main button to make the simulation run
      actionButton(inputId = "run_button", "Run Simulation", icon = icon("fish"),
                   class = "btn-primary w-100"),
      
      # Download Button
      div(style = "margin-top: 20px;", h5("Download Results")),
      downloadButton("downloadData", "Download Results", icon = icon("download"), class = "w-100")
    ),
    
    # this is the main part of the page
    mainPanel(
      # We use tabsetPanel to create the tabs
      tabsetPanel(
        type = "tabs",
        
        # TAB 1: The Plot
        tabPanel("Plot", icon = icon("chart-line"),
                 br(),
                 h3(textOutput("plot_title")),
                 withSpinner(plotOutput(outputId = "results_plot"))
        ),
        
        # TAB 2: The Summary Stats
        tabPanel("Summary", icon = icon("list"),
                 br(),
                 h3("Simulation Results Summary"),
                 hr(),
                 withSpinner(uiOutput("summary_stats")) # Using uiOutput to render formatted text
        )
      )
    )
  )
)

# ----------------------------------------------------
# PART 2: The Server - Where the R code runs
# ----------------------------------------------------
server <- function(input, output, session) 
{
  
  # This part watches the species input. When it changes, this code runs.
  observeEvent(input$species_input, {
    
    # get the new list of rivers for the selected species
    rivers_for_dropdown <- get_rivers(input$species_input)
    
    # now, update the existing river dropdown with the new list
    updateSelectInput(session = session, 
                      inputId = "river_input",
                      label = "Select River:",
                      choices = rivers_for_dropdown,
                      selected = rivers_for_dropdown[1]) # Select the first river by default
  })
  
  # --- NEW: Handler for Downloading the CSV Template ---
  output$downloadTemplate <- downloadHandler(
    filename = function() {
      paste0("habitat_template_", input$species_input, ".csv")
    },
    content = function(file) {
      # Use anadrofish function to generate a template dataframe
      # built_in = FALSE ensures we get a blank template structure
      template_df <- anadrofish::custom_habitat_template(
        species = input$species_input, 
        built_in = FALSE, 
        river = "MyCustomRiver" 
      )
      write.csv(template_df, file, row.names = FALSE)
    }
  )
  
  
  # This section only runs when the "Run Simulation" button is clicked
  simulation_from_model <- eventReactive(input$run_button, {
    
    # make sure all the inputs are loaded before trying to run the model
    req(input$upstream_input, input$downstream_input, input$downstream_j_input)
    
    # need to convert passage from 0-100 to 0-1 for the model
    up_passage <- input$upstream_input / 100
    down_passage <- input$downstream_input / 100
    down_j_passage <- input$downstream_j_input / 100
    
    results <- NULL
    
    # --- LOGIC BRANCHING based on Habitat Source ---
    if (input$habitat_source == "builtin") {
      # CASE A: Built-in River
      req(input$river_input)
      
      results <- anadrofish::sim_pop(
        species = input$species_input, 
        river = input$river_input,
        nyears = input$nyears_input, n_init = input$n_init_input,
        sr = input$sr_input, b = input$b_input,
        upstream = up_passage, downstream = down_passage, downstream_j = down_j_passage,
        output_years = input$output_years_input, sex_specific = input$sex_specific_input
      )
      
    } else {
      # CASE B: Custom Habitat (CSV Upload)
      req(input$custom_csv) # Ensure file is uploaded
      
      # Read the uploaded CSV file
      custom_habitat_df <- read.csv(input$custom_csv$datapath)
      
      # Validate CSV isn't empty
      if (nrow(custom_habitat_df) == 0) {
        stop("Uploaded CSV is empty")
      }
      
      results <- anadrofish::sim_pop(
        species = input$species_input,
        river = "Custom River", # Generic label
        custom_habitat = custom_habitat_df, # <<< Pass the uploaded dataframe here
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
      # Handle name generation for custom rivers
      river_label <- if(input$habitat_source == "builtin") input$river_input else "custom_river"
      paste0("anadrofish_data_", input$species_input, "_", river_label, "_", Sys.Date(), ".csv")
    },
    contentType = "text/csv",
    content = function(file) {
      # Writes the simulation data "simulation_from_model()" to the CSV file
      write.csv(simulation_from_model(), file, row.names = FALSE)
    }
  )
  
  
  # this makes the title above the plot
  output$plot_title <- renderText({
    # Waits for button press
    simulation_from_model()
    
    species_name <- names(ALL_SPECIES_VECTOR[ALL_SPECIES_VECTOR == input$species_input])
    
    if (input$habitat_source == "builtin") {
      req(input$river_input)
      paste("Simulation for", species_name, "in the", input$river_input)
    } else {
      paste("Simulation for", species_name, "in Custom Habitat Configuration")
    }
  })
  
  # this function makes the plot
  output$results_plot <- renderPlot({
    the_plot_data <- simulation_from_model()
    
    #req() prevents the plot from rendering until simulation_from_model() has ran
    req(the_plot_data)
    
    if (input$output_years_input == "all") {
      # Time series plot
      ggplot(data=the_plot_data, aes(x = year, y = spawners)) +
        geom_line(color = "steelblue", linewidth = 1.2) +
        labs(subtitle = "Total spawner abundance over time", x = "Year", y = "Number of Spawners") +
        theme_minimal(base_size = 16)
    } else {
      # Bar chart for the final year
      ggplot(data=the_plot_data, aes(x = as.factor(age), y = spawners)) +
        geom_col(fill = "steelblue", alpha = 0.8) +
        labs(subtitle = "Spawner abundance by age in the final simulation year", x = "Age Class", y = "Number of Spawners") +
        theme_minimal(base_size = 16)
    }
  })
  
  # --- Logic for the Summary Tab ---
  output$summary_stats <- renderUI({
    # Get the data
    df <- simulation_from_model()
    req(df)
    
    # Calculate Statistics
    # We aggregate by year just in case the data has age-structure rows per year
    yearly_totals <- aggregate(spawners ~ year, data = df, sum)
    
    init_pop <- head(yearly_totals$spawners, 1)
    final_pop <- tail(yearly_totals$spawners, 1)
    mean_pop <- mean(yearly_totals$spawners)
    peak_pop <- max(yearly_totals$spawners)
    min_pop <- min(yearly_totals$spawners)
    
    # Trend Determindation
    trend_text <- if(final_pop > init_pop) "Increasing" else "Decreasing"
    trend_color <- if(final_pop > init_pop) "green" else "red"
    trend_icon <- if(final_pop > init_pop) icon("arrow-trend-up") else icon("arrow-trend-down")
    
    # The HTML layout
    tagList(
      p(strong("Initial Population: "), format(round(init_pop), big.mark=",")),
      p(strong("Final Population: "), format(round(final_pop), big.mark=",")),
      p(strong("Mean Population: "), format(round(mean_pop), big.mark=",")),
      p(strong("Peak Population: "), format(round(peak_pop), big.mark=",")),
      p(strong("Minimum Population: "), format(round(min_pop), big.mark=",")),
      hr(),
      h4("Population Trend:", span(trend_icon, trend_text, style = paste0("color:", trend_color, ";")))
    )
  })
}

# ----------------------------------------------------
# PART 3: Run the App
# ----------------------------------------------------
shinyApp(ui = ui, server = server)