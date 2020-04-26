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

NYT_countydata <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv", col_types = "Dcccnn")
NYT_statedata <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv", col_types = "Dccnn")
countrydata <- read_csv("https://covid.ourworldindata.org/data/ecdc/full_data.csv", col_types = "Dcnnnn")

# *****************
# Transform Data
# *****************

# Create a unique label for each region in each dataset
NYT_countydata <- NYT_countydata %>% 
    mutate(region = paste(county, state, sep=", "))

NYT_statedata <- NYT_statedata %>% rename(region = state)

countrydata <- countrydata %>% 
    rename(region = location, new.cases = new_cases, cases = total_cases)

# Calculate new cases for NYT data
# country data already has new cases is original dataset

newcases_helperfunction = function(data) {
    data <- data %>%
        group_by_at(setdiff(names(data), c("date", "cases", "deaths"))) %>%
        arrange(date) %>%
        mutate(new.cases = cases - c(0, cases[-length(cases)])) %>%
        ungroup()
    return(data)
}

NYT_countydata <- newcases_helperfunction(NYT_countydata)
NYT_statedata <- newcases_helperfunction(NYT_statedata)

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
            radioButtons(inputId = "datasource",
                         label = "Select type geographic level: ",
                         choices = c("Country", "US State", "US County"),
                         inline = T,
                         selected = "Country"),
            
            selectInput(inputId = "geographic_location",
                        label = "Search & Select Geographic Location (multiple selection allowed):",
                        choices = c("Select regions" = "", countrydata$region),
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
                         inline = T),
            
            sliderInput(inputId = "smoothing_bandwidth",
                        label = "Smoothing Bandwidth:",
                        min = 1, max = 5, value = 3, step = 1)
        ),

        # Plots
        mainPanel(
           plotOutput("Plot_TotalCases"),
           plotOutput("Plot_NewCases"), 
           plotOutput("Plot_Rstat")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, clientData, session) {
    
    observe({
        if(input$datasource == "Country") {
            data <- countrydata
        } else if (input$datasource == "US State") {
            data <- NYT_statedata 
        } else {
            data <- NYT_countydata
        }

        updateSelectInput(session, "geographic_location",
                           label = "Search & Select Geographic Location (multiple selection allowed):",
                           choices = c("Select regions" = "", unique(data$region)))
    })
    
    # Subset data to only the county of interest
    data = reactive({
        if(input$datasource == "Country") {
            data <- countrydata
        } else if (input$datasource == "US State") {
            data <- NYT_statedata 
        } else {
            data <- NYT_countydata
        }
        
       x = data %>%
            arrange(date) %>%
            group_by_at(setdiff(names(data), c("date", "cases", "deaths", "new.cases", "new_deaths", "total_deaths"))) %>%
            mutate(new.cases_smoothed = ksmooth(date, new.cases, kernel = input$smoothing_kernal, bandwidth = input$smoothing_bandwidth, n.points = n())$y) %>%
            mutate(denominator = head(c(rep(NA, input$reference_date*-1), new.cases_smoothed), input$reference_date)) %>%
            mutate(denominator = replace(denominator, denominator %in% c(0, NaN, NA), 1)) %>%
            ungroup() %>%
            mutate(Rs = new.cases_smoothed/denominator) %>%
            filter(region %in% input$geographic_location, date >= as.Date(input$dateRange[1]), date <= as.Date(input$dateRange[2]))
    })
    
    output$Plot_TotalCases <- renderPlot({
        ggplot(data(), aes(date, cases, group = region)) + 
            geom_line(aes(color = region)) +
            labs(title = "Total Cases") + 
            theme_pubr() + 
            theme(legend.title = element_blank())
    })
    
    output$Plot_NewCases <- renderPlot({
        ggplot(data()) + 
            geom_line(aes(date, new.cases, group = region, color = region), alpha=0.75) +
            geom_line(aes(date, new.cases_smoothed, group = region, color = region), alpha=1, size = 1.5) + 
            labs(title = "New Cases") + 
            theme_pubr() + 
            theme(legend.title = element_blank())
    })
    
    output$Plot_Rstat <- renderPlot({
        ggplot(data(), aes(date, Rs, group = region)) + 
            geom_line(aes(color = region)) +
            labs(title = "Simplified R estimate") +
            theme_pubr() + 
            theme(legend.title = element_blank())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
