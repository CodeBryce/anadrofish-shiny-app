# anadrofish-shiny-app
A user-friendly R Shiny app for simulating anadromous fish populations. This tool allows fisheries managers and biologists to simulate population dynamics of anadromous fish (American Shad, Alewife, Blueback Herring) without needing advanced programming skills.

🚀 Features
* No-Code Interface: Replaces command-line R scripts with an intuitive GUI using bslib.
* Reactive Simulations: Instantly visualize how changes in dam passage efficiency (upstream/downstream) affect population abundance over 50+ years.
* Custom Habitat Module: Allows researchers to upload their own CSV datasets to test theoretical restoration scenarios on custom river systems.
Automated Reporting: Generates instant statistical summaries (Mean, Peak, Trends) and exportable CSV results.

🛠️ Built With
* R - Core programming language
* Shiny - Web application framework
* anadrofish - Underlying statistical model (Stich et al.)
* ggplot2 - Data visualization
* bslib - Modern UI theming

📦 How to Run
1. Install R and RStudio.

2. Install required packages by running this command in the R console:
     *install.packages(c("shiny", "ggplot2", "shinycssloaders", "bslib", "remotes"))
     *remotes::install_github("dstich/anadrofish")

4. Run the App:
Open "app.R" in RStudio and click "Run App", or run the command:
shiny::runApp("app.R")



📊 Usage
* Select Location: Choose a species and a built-in river, or select "Custom" to upload your own habitat data.
* Adjust Parameters: Use the sliders to set upstream/downstream passage survival rates (0-100%).
* Simulate: Click "Run Simulation" to generate population projections.
* Analyze: Switch between the "Plot" tab for visual trends and the "Summary" tab for key metrics.


👨‍💻 Author
* Bryce Davis
* Data Science Intern, SUNY Oneonta
* [Linkedln](https://www.linkedin.com/in/bryce-davis/)
* Developed for the Biological Sciences Department at SUNY Oneonta.


