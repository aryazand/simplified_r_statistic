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
library(RcppRoll)

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
    rename(region = location, cases = total_cases)

# Calculate new cases for NYT data
# country data already has new cases is original dataset

newcases_helperfunction = function(data) {
    data <- data %>%
        group_by_at(setdiff(names(data), c("date", "cases", "deaths"))) %>%
        arrange(date) %>%
        mutate(new_cases = cases - c(0, cases[-length(cases)])) %>%
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
                        min = -7, max = 0, value = -4, step = 1),
            
            radioButtons(inputId = "smoothing_kernal_align",
                         label = "Select alignment of day within smoothing window:",
                         choices = c("center", "right"), 
                         selected = "right"), 
            
            # I had an idea to weighted vs unweighted smoothing             
            radioButtons(inputId = "smoothing_kernal_shape",
                         label = "Weighted or unweighted smoothing",
                         choices = c("unweighted" = "unweighted", "gaussian (sd = 1)" = "gaussian"),
                         selected = "unweighted"),
                        
            sliderInput(inputId = "smoothing_window",
                        label = "Smoothing window (number of days to consider when smoothing):",
                        min = 1, max = 10, value = 7, step = 1)
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
        
        smoothing_function <- function(x, n = input$smoothing_window, align = input$smoothing_kernal_align, shape = input$smoothing_kernal_shape) {
            
            if(shape == "unweighted") {
                weights = NULL
            } else if (shape == "gaussian" & align == "right") {
                weights = dnorm((-(n-1):0))
            } else if (shape == "gaussian" & align == "center") {
                if(n%%2 == 0) {
                    weights = dnorm(((-(n/2)+1):(n/2)))    
                } else {
                    weights = dnorm(((-(n-1)/2):((n-1)/2)))  
                }
            }
            
            x.smooth = roll_mean(x, n = n, align = align, weights = weights, fill=c(NA,NA,NA))
            
            return(x.smooth)
        }
        
        data %>%
            filter(region %in% input$geographic_location, date >= as.Date(input$dateRange[1]), date <= as.Date(input$dateRange[2])) %>%
            arrange(date) %>%
            group_by(region) %>%
            mutate(new_cases.smoothed = smoothing_function(x = new_cases)) %>%
            mutate(denominator = head(c(rep(NA, input$reference_date*-1), new_cases.smoothed), input$reference_date)) %>%
            mutate(denominator = replace(denominator, denominator %in% c(0, NaN, NA), 1)) %>%
            ungroup() %>%
            mutate(Rs = new_cases.smoothed/denominator)
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
            geom_line(aes(date, new_cases, group = region, color = region), alpha=0.75) +
            geom_line(aes(date, new_cases.smoothed, group = region, color = region), alpha=1, size = 1.5) + 
            labs(title = "New Cases") + 
            theme_pubr() + 
            theme(legend.title = element_blank())
    })
    
    output$Plot_Rstat <- renderPlot({
        ggplot(data(), aes(date, Rs, group = region)) + 
            geom_line(aes(color = region)) +
            geom_hline(yintercept = 1, linetype = 2) + 
            labs(title = "Simplified R estimate") +
            scale_y_log10(labels = function(x) format(x, scientific = F)) + 
            theme_pubr() + 
            theme(legend.title = element_blank())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
