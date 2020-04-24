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
# Calculate Stats
# *****************

# Calculate new cases
NYT_countydata <- NYT_countydata %>%
    group_by(county, state, fips) %>%
    arrange(date) %>%
    mutate(new.cases = cases - c(0, cases[-length(cases)])) %>%
    ungroup()

#Calculate the R statistic
NYT_countydata <- NYT_countydata %>%
    group_by(county, state, fips) %>%
    arrange(date) %>%
    mutate(denominator = head(c(NA,NA,NA,NA, new.cases),-4)) %>%
    mutate(Rs = new.cases/denominator) %>%
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
                        label = "Select County :",
                        choices = c("Choose one" = "", county.states)
                        )
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
    NYT_countydata.subset = reactive({
        # Split the string from the "county" input textbox into county and state
        county.state.split = str_split(string = input$county, pattern = ", ")[[1]]
        
        #Subset
        NYT_countydata %>%
            filter(county == county.state.split[1], state == county.state.split[2])
    })
    
    output$Plot_TotalCases <- renderPlot({
        NYT_countydata.subset <- NYT_countydata.subset()
        ggplot(NYT_countydata.subset, aes(date, cases)) + 
            geom_line() +
            labs(title = "Total Cases") + 
            theme_pubr()
    })
    
    output$Plot_NewCases <- renderPlot({
        NYT_countydata.subset <- NYT_countydata.subset()
        ggplot(NYT_countydata.subset, aes(date, new.cases)) + 
            geom_line() + 
            labs(title = "New Cases") + 
            theme_pubr()
    })
    
    output$Plot_Rstat <- renderPlot({
        NYT_countydata.subset <- NYT_countydata.subset()
        ggplot(NYT_countydata.subset, aes(date, Rs)) +
            geom_line() +
            labs(title = "Simplified R estimate") +
            theme_pubr()
    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
