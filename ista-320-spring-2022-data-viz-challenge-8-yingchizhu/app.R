#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# load libraries
library(lubridate)
library(usmap)
library(shiny)
library(tidyverse)


# the US data files all start with the same prefix
us_files <- "COVID-19_Vaccine_Distribution_Allocations_by_Jurisdiction"

# create list of files to read
files_to_read <- list.files(path = "data",
                            pattern = us_files,
                            full.names = TRUE)

# read files
covid_vaccines_us <- sapply(files_to_read, read_csv, simplify = TRUE) %>%
    bind_rows(.id = "filename")

# create manufacturer column based on filename
covid_vaccines_us <- covid_vaccines_us %>%
    mutate(manufacturer = gsub(
        "data\\/COVID\\-19_Vaccine_Distribution_Allocations_by_Jurisdiction_\\-_|\\.csv", 
        "", filename))

# pivot dose columns and remove allocation that is NA
covid_vaccines_us_tidy <- covid_vaccines_us %>%
    pivot_longer(cols = `1st Dose Allocations`:`2nd Dose Allocations`,
                 names_to = "dose_type",
                 values_to = "allocation") %>%
    filter(!is.na(allocation))

# parse week of allocations as date
covid_vaccines_us_tidy <- covid_vaccines_us_tidy %>%
    mutate(date = parse_date(`Week of Allocations`, "%m/%d/%Y"))

# list of jurisdictions
region_options <- covid_vaccines_us_tidy %>%
    distinct(Jurisdiction) %>%
    arrange(Jurisdiction)
    
# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("US COVID-19 Vaccine Dashboard"),

    # sidebar with info 
    sidebarLayout(
        sidebarPanel(
            h3("Total Vaccine Dose Allocation Per State"),
            p("Click on tabs to choose different visualizations of the same data."),
            HTML('Data from the <a href="https://data.cdc.gov/browse?category=Vaccinations">CDC data catalog</a>')
        ),

        # show different plots
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Timeline", 
                                 selectInput("region_to_highlight",
                                             "Select Region to Highlight:",
                                             choices = region_options),
                                 plotOutput("timeline_plot")),
                        tabPanel("Map", plotOutput("map_plot")),
                        tabPanel("Bar", plotOutput("bar_plot")))
        )
    )
)

# Define server logic
server <- function(input, output) {

    output$timeline_plot <- renderPlot({
        
        data_to_plot <- covid_vaccines_us_tidy %>%
            group_by(Jurisdiction, date) %>%
            summarize(total_allocation = sum(allocation))
        
        highlighted_region <- data_to_plot %>%
            filter(Jurisdiction == input$region_to_highlight)
        
        data_to_plot %>%
            ggplot(aes(y = total_allocation,
                   x = date,
                   group = Jurisdiction)) +
            geom_line(color = "grey") +
            geom_point(data = highlighted_region,
                       color = "red") +
            geom_line(data = highlighted_region,
                      color = "red") +
            theme_linedraw() +
            labs(x = "Allocation of Vaccine Doses",
                 y = "")
            
    })
    
    
    output$map_plot <- renderPlot({
        
        data_to_plot <- covid_vaccines_us_tidy %>%
            group_by(state = Jurisdiction, date) %>%
            summarize(total_allocation = sum(allocation))
        
        data_to_plot %>%
            plot_usmap(data = .,
                       values = "total_allocation") +
            theme(legend.position = "right") +
            scale_fill_continuous(name = "Total Dose Allocation",
                                  low = "gray72",
                                  high = "gray23")
        
    })
    output$bar_plot <- renderPlot({
      
      data_to_plot <- covid_vaccines_us_tidy %>%
        group_by(state = Jurisdiction, date) %>%
        summarize(total_allocation = sum(allocation))
      
      data_to_plot %>%
        ggplot(aes(y = total_allocation,
                   x = date)) +
        geom_col(position = "dodge")
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
