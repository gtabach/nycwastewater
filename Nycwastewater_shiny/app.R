#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)

wastewater_df <- readRDS("cdc_wastewater_cleaned.Rdata")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NYC Covid-19 Wastewater Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("dates",
                        "Dates:",
                        min = min(wastewater_df$date_end),
                        max = max(wastewater_df$date_end),
                        value = c(min(wastewater_df$date_end), max(wastewater_df$date_end))),
            checkboxGroupInput("counties",
                               "Boroughs to Include:",
                               c("Manhattan" = "New York",
                                 "Brooklyn" = "Kings",
                                 "Bronx" = "Bronx",
                                 "Queens" = "Queens",
                                 "Staten Island" = "Richmond"),
                               selected = c("New York", "Kings", "Bronx", "Queens", "Richmond"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("wastewater_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$wastewater_plot <- renderPlot({
      df = wastewater_df %>%
        filter(county_names %in% input$counties,
               date_end >= input$dates[1],
               date_end <= input$dates[2])
      
      ggplot(data = df, aes(x = date_end, y = percentile)) +
        geom_point(aes(size = population_served/1000,
                       col = county_names),
                       alpha = 0.3) +
        # geom_smooth(aes(weight = population_served)) +
        # facet_grid(county_names ~ .) +
        scale_size_area(max_size = 8) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
        labs(title = "Weekly COVID-19 wastewater rates",
             subtitle = "for testing sites in NYC",
             x = "Date",
             y = "% of peak rate", 
             size = "Population served by\nwastewater site\n(thousands)",
             col = "Borough") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              text = element_text(size = 18)) +#,
              #plot.title = element_text(hjust = 0.5, size = 30)) +
        scale_color_viridis_d()
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
