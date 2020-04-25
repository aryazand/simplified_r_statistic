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
library(ggpubr)

# ****************
# Load Data
# ****************

NYT_countydata <- read_csv("./data/NYT_us-counties.csv", col_types = "Dcccnn")

# *****************
# Transform Data
# *****************

# Create a unique idea for each county based on names
NYT_countydata <- NYT_countydata %>% 
    mutate(countystate = paste(county, state, sep=", "))

# Calculate new cases
NYT_countydata <- NYT_countydata %>%
    group_by(county, state, fips) %>%
    arrange(date) %>%
    mutate(new.cases = cases - c(0, cases[-length(cases)])) %>%
    ungroup()

# ***************
# Create labels for ui from data
# ***************

county.states <- paste(NYT_countydata$county, NYT_countydata$state, sep=", ") %>% unique()

# *************
# User Interface 
# *************

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Covid-19 R statistic"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "county",
                        label = "Search & Select County (multiple selection allowed):",
                        choices = c("Choose one" = "", county.states),
                        multiple = T
                        ),
            
            dateRangeInput(inputId = "dateRange",
                           label = "select dates",
                           start = "2020-01-21",
                           end = Sys.Date()),
            
            sliderInput(inputId = "reference_date",
                        label = "Select a reference day for calculating Rs:",
                        min = -5, max = 0, value = -4, step = 1),
            
            radioButtons(inputId = "smoothing_kernal",
                         label = "Select a smoothing kernal for smoothing new cases per each day:",
                         choices = c("box", "normal"),
                         selected = "box"),
            
            sliderInput(inputId = "smoothing_bandwidth",
                        label = "Smoothing Bandwidth:",
                        min = 1, max = 5, value = 3, step = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("Plot_TotalCases"),
           plotOutput("Plot_NewCases"), 
           plotOutput("Plot_Rstat")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Subset data to only the county of interest
    data = reactive({
        NYT_countydata %>%
            arrange(date) %>%
            group_by(county, state, fips) %>%
            mutate(new.cases_smoothed = ksmooth(date, new.cases, kernel = input$smoothing_kernal, bandwidth = input$smoothing_bandwidth, n.points = n())$y) %>%
            mutate(denominator = head(c(rep(NA, input$reference_date*-1), new.cases_smoothed), input$reference_date)) %>%
            mutate(denominator = replace(denominator, denominator == 0, 1)) %>%
            ungroup() %>%
            mutate(Rs = new.cases_smoothed/denominator) %>%
            filter(countystate %in% input$county, date >= as.Date(input$dateRange[1]), date <= as.Date(input$dateRange[2]))
    })
    
    output$Plot_TotalCases <- renderPlot({
        ggplot(data(), aes(date, cases, group = countystate)) + 
            geom_line(aes(color = countystate)) +
            labs(title = "Total Cases") + 
            theme_pubr() + 
            theme(legend.title = element_blank())
    })
    
    output$Plot_NewCases <- renderPlot({
        ggplot(data()) + 
            geom_line(aes(date, new.cases, group = countystate, color = countystate), alpha=0.75) +
            geom_line(aes(date, new.cases_smoothed, group = countystate, color = countystate), alpha=1, size = 1.5) + 
            labs(title = "New Cases") + 
            theme_pubr() + 
            theme(legend.title = element_blank())
    })
    
    output$Plot_Rstat <- renderPlot({
        ggplot(data(), aes(date, Rs, group = countystate)) + 
            geom_line(aes(color = countystate)) +
            labs(title = "Simplified R estimate") +
            theme_pubr() + 
            theme(legend.title = element_blank())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
