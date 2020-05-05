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
library(EpiEstim)

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
    rename(region = location)

# Calculate new cases for NYT data
# country data already has new cases is original dataset

newcases_helperfunction = function(data) {
    data <- data %>%
        # Total Cases is Active Cases + Deaths. However this ignores recovered??
        mutate(total_cases = cases + deaths) %>%
        group_by_at(setdiff(names(data), c("date", "cases", "total_cases", "deaths"))) %>%
        arrange(date) %>%
        mutate(new_cases = total_cases - c(0, total_cases[-length(total_cases)])) %>%
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
    titlePanel("R Estimator"),
    
    # *************
    # Plots
    # *************
    
    fluidRow(
        # Have a tab for each plot
        tabsetPanel(type="tabs",
                    tabPanel("Total Cases", 
                             column(10, plotOutput("Plot_TotalCases", hover = hoverOpts(id ="TotalCases.hover", delay=50))),
                             column(2, htmlOutput("TotalCases.info"))
                             ),
                    tabPanel("New Cases", 
                             column(10, plotOutput("Plot_NewCases", hover = hoverOpts(id ="NewCases.hover", delay=50))),
                             column(2, htmlOutput("NewCases.info"))
                             ), 
                    tabPanel("R", 
                             column(10, plotOutput("Plot_Rstat", hover = hoverOpts(id ="R.hover", delay = 50))),
                             column(2, htmlOutput("R.info"))
                             )    
        )
        
    ),
    
    # *************
    # Controllers
    # *************
    
    fluidRow(
        
        # *************
        # Controllers: Geography and Dates
        # *************
        
        column(4,
            wellPanel(
                tags$h3("Geography & Dates"),
                
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
                               end = Sys.Date())
            )
        ),
        
        # *************
        # Controllers: Simplified R Calculations
        # *************
        
        column(4, 
            wellPanel(
                
                tags$h3("Simplified R Calculation Parameters"),
                
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
            )
       ),

       
       # *************
       # Controllers: Simplified R Calculations
       # *************
       
       column(4, 
              wellPanel(
                  tags$h3("EpiEstim R Calculation Parameters"),
                  
                  sliderInput(inputId = "Epiestim.mean_si",
                              label = "Mean days between onset of symptoms and positive test",
                              min = 1, max = 10, value = 4, step = 1),
                  
                  sliderInput(inputId = "Epiestim.std_si",
                              label = "Standard Deviation in days between onset of symptoms and positive test",
                              min = 1, max = 10, value = 1, step = 1),
                  
                  sliderInput(inputId = "Epiestim.window_size",
                              label = "Window Size",
                              min = 2, max = 14, value = 7, step = 1),
              )
       )
       
    )
)

# *************
# Server
# *************

server <- function(input, output, clientData, session) {
    
    # *************
    # Reactivity and Data Transformation
    # *************
    
    observe({
        if(input$datasource == "Country") {
            data <- countrydata
        } else if (input$datasource == "US State") {
            data <- NYT_statedata 
        } else {
            data <- NYT_countydata
        }
        
        if(input$datasource == "Country") {
            updateSelectInput(session, "geographic_location",
                              label = "Search & Select Geographic Location (multiple selection allowed):",
                              choices = c("Select regions" = "", unique(data$region)),
                              selected = "United States")
        } else {
            updateSelectInput(session, "geographic_location",
                              label = "Search & Select Geographic Location (multiple selection allowed):",
                              choices = c("Select regions" = "", unique(data$region)))
        }

    })
    
    # Subset data to only the region and dates of interest
    data_subset = reactive({
        if(input$datasource == "Country") {
            data <- countrydata
        } else if (input$datasource == "US State") {
            data <- NYT_statedata 
        } else {
            data <- NYT_countydata
        }
        
        data <- data %>% filter(region %in% input$geographic_location)
    }) 
    
    # ***************
    # Calculate simplified R
    # ***************
    
    data =  reactive({
        
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
        
        data <- data_subset() %>%
            arrange(date) %>%
            group_by(region) %>%
            mutate(new_cases.smoothed = smoothing_function(x = new_cases)) %>%
            mutate(denominator = head(c(rep(NA, input$reference_date*-1), new_cases.smoothed), input$reference_date)) %>%
            mutate(denominator = replace(denominator, denominator %in% c(0, NaN, NA), 1)) %>%
            ungroup() %>%
            mutate(Rs = new_cases.smoothed/denominator)
    }) 
        
        
    # ***************
    # Calculate Epiestim
    # ***************
    
    data.epiestim = reactive({
        
        data.epiestim = data_subset() %>%
            arrange(date) %>%
            group_by(region) %>%
            nest(CD = -region) %>%
            mutate(EpiEstim = map(CD, function(CD_)
                estimate_R(CD_$new_cases, 
                           method = "parametric_si", 
                           config = make_config(list(mean_si = input$Epiestim.mean_si, 
                                                     std_si = input$Epiestim.std_si,
                                                     t_start = seq(2, nrow(CD_) - input$Epiestim.window_size + 1),
                                                     t_end = seq(input$Epiestim.window_size + 1, nrow(CD_)))
                           )
                
                )
            )) %>%
            mutate(EpiEstim = map(EpiEstim, function(i) i$R)) %>%
            mutate(date = map(CD, function(i) i$date)) %>%
            mutate(date = map(date, function(i) tail(i, -(input$Epiestim.window_size)))) %>%
            mutate(EpiEstim = map2(EpiEstim, date, function(i,j) mutate(i, date = j))) %>%
            select(region, EpiEstim) %>%
            unnest(EpiEstim)
    })
    
    # *************
    # Create Plots
    # *************
    
    custom_plot_theme <- function() { 
        theme_bw() + 
        theme(legend.title = element_blank(),
              axis.text = element_text(size = 15),
              axis.title = element_text(size = 15, face="bold"),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line(color="black")
        )
    }
    
    # Total Cases
    output$Plot_TotalCases <- renderPlot({
        dummyTable = data() %>% filter(date >= as.Date(input$dateRange[1]) & date <= as.Date(input$dateRange[2]))
        max_y_value = max(dummyTable$total_cases, na.rm = T)*1.1
        
        ggplot(data(), aes(date, total_cases, group = region)) +
            geom_line(aes(color = region)) +
            labs(y = 'Total Cases') +
            scale_x_date(date_breaks = '1 week', date_labels = '%b-%d',
                         limits=c(as.Date(input$dateRange[1]),  as.Date(input$dateRange[2]))) +
            scale_y_continuous(labels = function(x) format(x, scientific = F),
                              limits = c(0, max_y_value)) + 
            custom_plot_theme()
    })

    # New Cases
    output$Plot_NewCases <- renderPlot({
        dummyTable = data() %>% filter(date >= as.Date(input$dateRange[1]) & date <= as.Date(input$dateRange[2]))
        max_y_value = max(dummyTable$new_cases, na.rm = T)*1.1
        
        ggplot(data()) + 
            geom_line(aes(date, new_cases, group = region, color = region), alpha=0.75) +
            geom_line(aes(date, new_cases.smoothed, group = region, color = region), alpha=1, size = 1.5) + 
            labs(y = "New Cases") +
            scale_x_date(date_breaks = '1 week', date_labels = '%b-%d',
                         limits=c( as.Date(input$dateRange[1]),  as.Date(input$dateRange[2]))) +
            ylim(0, max_y_value) + 
            custom_plot_theme() 
    })
    
    # R estimate
    
    output$Plot_Rstat <- renderPlot({
        dummyTable = data() %>% filter(date >= as.Date(input$dateRange[1]) & date <= as.Date(input$dateRange[2]))
        dummyTable2 = data.epiestim() %>% filter(date >= as.Date(input$dateRange[1]) & date <= as.Date(input$dateRange[2]))
        max_y_value = max(dummyTable$Rs, dummyTable2$`Quantile.0.975(R)`,na.rm=T)*1.1
        
        ggplot(data()) + 
            geom_line(aes(x = date, y = Rs, color = region, group = region)) +
            geom_ribbon(data = data.epiestim(), aes(x = date,  ymin=`Quantile.0.025(R)`,ymax=`Quantile.0.975(R)`, group = region, fill = region), alpha=0.15)+
            geom_line(data = data.epiestim(), aes(x = date, y=`Mean(R)`, group = region, color = region), linetype = 2) +
            geom_line(data = data.epiestim(), aes(x = date, y=`Quantile.0.025(R)`, group = region, color = region), linetype = 2) +
            geom_line(data = data.epiestim(), aes(x = date, y=`Quantile.0.975(R)`, group = region, color = region), linetype = 2) +
            geom_hline(yintercept = 1, linetype = 2) + 
            labs(subtitle = "dotted lines give R estimation from EpiEstim with a 95% CI") +
            labs(y = "R estimate") +
            scale_x_date(date_breaks = '1 week', date_labels = '%b-%d',
                         limits=c(as.Date(input$dateRange[1]),  as.Date(input$dateRange[2]))) +
            ylim(0, max_y_value) + 
            custom_plot_theme()
    })
    
    
    # *************
    # Hover Info
    # based on: https://stackoverflow.com/questions/34370780/r-interactive-plot-show-vertical-line-on-hover
    # *************
    
    output$TotalCases.info <- renderUI({
        data = data()

        if(nrow(data) > 1 & !is.null(input$TotalCases.hover)) {
            max_y_value = data %>% 
                filter(date >= as.Date(input$dateRange[1]) & date <= as.Date(input$dateRange[2])) %>%
                .$total_cases %>%
                max(na.rm=T)
            
            dates = seq(as.Date(input$dateRange[1])-5, as.Date(input$dateRange[2])+5, "days")
            cases = seq(-1, max_y_value*1.1, length.out = 1000)


            dummytable = data.frame(
                date = rep(dates, 1000),
                total_cases = rep(cases, each=length(dates))
            )

            nearPoint <- nearPoints(dummytable, input$TotalCases.hover, threshold = 10, maxpoints = 1)

            closedata = data() %>% filter(date == nearPoint$date) %>% select(date, region, total_cases)
            
            date_title = paste0("Date ", closedata$date[1], "<br/>")
            parameters = c("Total Cases")
            create_str_func = function(df.row) {
                title = df.row["region"]
                df.row = as.character(df.row[!(names(df.row) %in% c("region", "date"))])
                info = paste0(paste0(parameters, ".....", df.row, "<br/>"), collapse="")
                info = paste0("<strong>", title, "</strong>", "<br/>", info, "<br/>")
                return(info)
            }
            
            output_strings = apply(closedata, 1, create_str_func)
            
            HTML(paste0(c(date_title, output_strings), collapse=""))
            
        } else {
         NULL
        }
    })
    
    output$NewCases.info <- renderUI({
        data = data()
        
        if(nrow(data) > 1 & !is.null(input$NewCases.hover)) {
            max_y_value = data %>% 
                filter(date >= as.Date(input$dateRange[1]) & date <= as.Date(input$dateRange[2])) %>%
                .$new_cases %>%
                max(na.rm=T)
            
            dates = seq(as.Date(input$dateRange[1])-5, as.Date(input$dateRange[2])+5, "days")
            cases = seq(-1, max_y_value*1.1, length.out = 1000)
            
            
            dummytable = data.frame(
                date = rep(dates, 1000),
                new_cases = rep(cases, each=length(dates))
            )
            
            nearPoint <- nearPoints(dummytable, input$NewCases.hover, threshold = 10, maxpoints = 1)
            
            closedata = data() %>% filter(date == nearPoint$date) %>% select(date, region, new_cases, new_cases.smoothed)
            
            date_title = paste0("Date ", closedata$date[1], "<br/>")
            parameters = c("Raw New Cases", "Smoothed New Cases")
            create_str_func = function(df.row) {
                title = df.row["region"]
                df.row = as.character(df.row[!(names(df.row) %in% c("region", "date"))])
                info = paste0(paste0(parameters, ".....", df.row, "<br/>"), collapse="")
                info = paste0("<strong>", title, "</strong>", "<br/>", info, "<br/>")
                return(info)
            }
            
            output_strings = apply(closedata, 1, create_str_func)
            
            HTML(paste0(c(date_title, output_strings), collapse=""))
        } else {
            NULL
        }
    })
    
    output$R.info <- renderUI({
        data = data()
        data.epiestim = data.epiestim()
        
        if(nrow(data) > 1 & !is.null(input$R.hover)) {
            max_y_value_1 = data %>% 
                filter(date >= as.Date(input$dateRange[1]) & date <= as.Date(input$dateRange[2])) %>%
                .$Rs %>%
                max(na.rm=T)
            
            max_y_value_2 = data.epiestim %>% 
                filter(date >= as.Date(input$dateRange[1]) & date <= as.Date(input$dateRange[2])) %>%
                .$`Quantile.0.975(R)` %>%
                max(na.rm=T)
            
            max_y_value = max(max_y_value_1, max_y_value_2, na.rm=T)
            
            dates = seq(as.Date(input$dateRange[1])-5, as.Date(input$dateRange[2])+5, "days")
            cases = seq(-1, max_y_value*1.1, length.out = 1000)
            
            
            dummytable = data.frame(
                date = rep(dates, 1000),
                Rs = rep(cases, each=length(dates))
            )
            
            nearPoint <- nearPoints(dummytable, input$R.hover, threshold = 10, maxpoints = 1)
            

            closedata = data %>% filter(date == nearPoint$date) %>% 
                        select(date, region, Rs) %>% 
                        mutate(Rs = round(Rs, 3))
            closedata.epiestim = data.epiestim %>% filter(date == nearPoint$date) %>% 
                select(c(date, region, `Mean(R)`, `Quantile.0.025(R)`, `Quantile.0.975(R)`)) %>% 
                mutate(`Mean(R)` = round(`Mean(R)`, 3),
                       `Quantile.0.025(R)` = round(`Quantile.0.025(R)`,3), 
                       `Quantile.0.975(R)` = round(`Quantile.0.975(R)`,3))
            closedata = full_join(closedata, closedata.epiestim, by=c("date", "region"))# %>% unite("pastestring", -date, sep=", ")
            
            date_title = paste0("Date ", closedata$date[1], "<br/>")
            parameters = c("Rs", "EpiEstim_R", "EpiEstim_R0.025", "EpiEstim_R0.975")
            create_str_func = function(df.row) {
                title = df.row["region"]
                df.row = as.character(df.row[!(names(df.row) %in% c("region", "date"))])
                info = paste0(paste0(parameters, ".....", df.row, "<br/>"), collapse="")
                info = paste0("<strong>", title, "</strong>", "<br/>", info, "<br/>")
                return(info)
            }
            
            output_strings = apply(closedata, 1, create_str_func)
            
            HTML(paste0(c(date_title, output_strings), collapse=""))
        } else {
            NULL   
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
