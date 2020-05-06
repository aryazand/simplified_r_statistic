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
        # 3 Tabs, one for total cases, new cases, and R calculations 
        # Each tab has a plot and an information bar to the right
        
        tabsetPanel(type="tabs",
                    tabPanel("Total Cases", 
                             column(10, plotOutput("Plot_TotalCases", 
                                                   hover = hoverOpts(id ="TotalCases.hover", delay=50),
                                                   dblclick = "TotalCases.dblclk",
                                                   brush = brushOpts(id = "TotalCases.brush", resetOnNew = TRUE)
                                    )),
                             column(2, htmlOutput("TotalCases.info"))
                             ),
                    tabPanel("New Cases", 
                             column(10, plotOutput("Plot_NewCases", 
                                                   hover = hoverOpts(id ="NewCases.hover", delay=50),
                                                   dblclick = "NewCases.dblclk",
                                                   brush = brushOpts(id = "NewCases.brush", resetOnNew = TRUE)
                             )),
                             column(2, htmlOutput("NewCases.info"))
                             ), 
                    tabPanel("R", 
                             column(10, plotOutput("Plot_Rstat", 
                                                   hover = hoverOpts(id ="R.hover", delay = 50),
                                                   dblclick = "R.dblclk",
                                                   brush = brushOpts(id = "R.brush", resetOnNew = TRUE)
                             )),
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
    
    function_to_zoom <- function(brush, ranges) {
        
        # Code from: https://shiny.rstudio.com/gallery/plot-interaction-zoom.html
        
        if (!is.null(brush)) {
            ranges$x <- as.Date(c(brush$xmin, brush$xmax), origin="1970-01-01")
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- c(as.Date(input$dateRange[1]),  as.Date(input$dateRange[2]))
            ranges$y <- NULL
        }
    }
    
    # Total Cases
    TotalCases.ranges <- reactiveValues(x = NULL, y = NULL)
    Plot_TotalCases <- reactive({
        ggplot(data(), aes(date, total_cases, group = region)) +
            geom_line(aes(color = region)) +
            labs(y = 'Total Cases') +
            scale_x_date(date_breaks = '1 week', date_labels = '%b-%d') +
            scale_y_continuous(labels = function(x) format(x, scientific = F)) +
            coord_cartesian(xlim = TotalCases.ranges$x, ylim = TotalCases.ranges$y, expand = FALSE) + 
            custom_plot_theme()
    })
    output$Plot_TotalCases <- renderPlot({Plot_TotalCases()})
    observeEvent(input$TotalCases.dblclk, {
        function_to_zoom(input$TotalCases.brush, TotalCases.ranges)
    })
    
    # New Cases
    NewCases.ranges <- reactiveValues(x = NULL, y = NULL)
    Plot_NewCases <- reactive({(
        ggplot(data()) + 
            geom_line(aes(date, new_cases, group = region, color = region), alpha=0.75) +
            geom_line(aes(date, new_cases.smoothed, group = region, color = region), alpha=1, size = 1.5) + 
            labs(y = "New Cases") +
            scale_x_date(date_breaks = '1 week', date_labels = '%b-%d') + 
            coord_cartesian(xlim = NewCases.ranges$x, ylim = NewCases.ranges$y, expand = FALSE) + 
            custom_plot_theme()
    )})
    output$Plot_NewCases <- renderPlot({Plot_NewCases()})
    observeEvent(input$NewCases.dblclk, {
        function_to_zoom(input$NewCases.brush, NewCases.ranges)
    })
    
    # R estimate
    R.ranges <- reactiveValues(x = NULL, y = NULL)
    Plot_Rstat <- reactive({
        ggplot(data()) + 
            geom_line(aes(x = date, y = Rs, color = region, group = region)) +
            geom_ribbon(data = data.epiestim(), aes(x = date,  ymin=`Quantile.0.025(R)`,ymax=`Quantile.0.975(R)`, group = region, fill = region), alpha=0.15)+
            geom_line(data = data.epiestim(), aes(x = date, y=`Mean(R)`, group = region, color = region), linetype = 2) +
            geom_line(data = data.epiestim(), aes(x = date, y=`Quantile.0.025(R)`, group = region, color = region), linetype = 2) +
            geom_line(data = data.epiestim(), aes(x = date, y=`Quantile.0.975(R)`, group = region, color = region), linetype = 2) +
            geom_hline(yintercept = 1, linetype = 2) + 
            labs(subtitle = "dotted lines give R estimation from EpiEstim with a 95% CI") +
            labs(y = "R estimate") +
            scale_x_date(date_breaks = '1 week', date_labels = '%b-%d') + 
            coord_cartesian(xlim = R.ranges$x, ylim = R.ranges$y, expand = FALSE) + 
            custom_plot_theme()
    })
    output$Plot_Rstat <- renderPlot({
        Plot_Rstat()
    })
    observeEvent(input$R.dblclk, {
        function_to_zoom(input$R.brush, R.ranges)
    })
    
    # *************
    # Hover Info
    # based on: https://stackoverflow.com/questions/34370780/r-interactive-plot-show-vertical-line-on-hover
    # *************
    
    info_on_hover_func <- function(plot_obj, hover_obj, ranges, threshold = 20, maxpoints = 1) {
        
        # Create a dummy table based on the x & y ranges of the plot
        # Using nearPoints with this table will allow us to find the x & y values of the
        # mouse no matter where the mouse is on the plot
        
        data = plot_obj$data
        
        # Get mapped variables
        # based on https://cran.r-project.org/web/packages/gginnards/vignettes/user-guide-2.html
        if(length(plot_obj$layers) == 1) {
            mapped.vars <- gsub("[~*\\%^]", " ", as.character(plot_obj$mapping)) %>%
                str_split(boundary("word")) %>%
                unlist() %>% 
                list()
 
        } else {
            mapped.vars <- map(plot_obj$layers, function(x) gsub("[~*\\%^]", "", as.character(x$mapping))) 
        }
    
        n = 1000
        
        if(!is.null(ranges$x) & !is.null(ranges$y)) { 
            dates = seq(as.Date(ranges$x[1]), as.Date(ranges$x[2]), length.out = n)
            y_vals = seq(ranges$y[1], ranges$y[2], length.out = n)
        } else {
            dates = seq(as.Date(input$dateRange[1]), as.Date(input$dateRange[2]), length.out = n)
            
            mapped.vars.y = map_chr(mapped.vars, function(x) x[2])
            mapped.vars.y_max = map_dbl(mapped.vars.y, function(x) max(data[[x]], na.rm=T))
            max_y = max(mapped.vars.y_max, na.rm=T)
            y_vals = seq(-1, max_y, length.out = n)
        }
        
        dummytable = data.frame(
            date = rep(dates, n),
            y_vals = rep(y_vals, each=length(dates))
        )
        colnames(dummytable)[2] = mapped.vars[[1]][2]
        
       
        
        nearPoint <- nearPoints(dummytable, hover_obj, threshold = threshold, maxpoints = maxpoints)

        return(nearPoint)
    }
    
    info_display_func <- function(data, parameters_to_display) {
        
        # HTML to render in to a information about data
        # helper function to display where mouse is hovering
        date_title = paste0("Date ", data$date[1], "<br/>")
        create_str_func = function(df.row) {
            title = df.row["region"]
            df.row = as.character(df.row[!(names(df.row) %in% c("region", "date"))])
            info = paste0(paste0(parameters_to_display, ".....", df.row, "<br/>"), collapse="")
            info = paste0("<strong>", title, "</strong>", "<br/>", info, "<br/>")
            return(info)
        }
        
        output_strings = apply(data, 1, create_str_func)
        output_strings = paste0(c(date_title, output_strings), collapse="")
        return(output_strings)
    }
    
    output$TotalCases.info <- renderUI({
        data = data()

        if(nrow(data) > 1 & !is.null(input$TotalCases.hover)) {
            nearPoint <- info_on_hover_func(Plot_TotalCases(), input$TotalCases.hover, TotalCases.ranges)
            nearPoint$date = as.character(nearPoint$date)
            data.subset = data %>% mutate(date = as.character(date)) %>% filter(date == nearPoint$date) %>% select(date, region, total_cases) 
            output_string = info_display_func(data.subset, c("Total Cases"))
            HTML(output_string)
        } else {
         NULL
        }
    })
    
    output$NewCases.info <- renderUI({
        data = data()
        
        if(nrow(data) > 1 & !is.null(input$NewCases.hover)) {
                nearPoint <- info_on_hover_func(Plot_NewCases(), input$NewCases.hover, NewCases.ranges)
                nearPoint$date = as.character(nearPoint$date)
                data.subset = data %>% mutate(date = as.character(date)) %>% 
                    filter(date == nearPoint$date) %>% select(date, region, new_cases, new_cases.smoothed)
                output_string = info_display_func(data.subset, c("New Cases Raw", "New Cases Smoothed"))
                HTML(output_string)
        } else {
            NULL
        }
    })
    
    output$R.info <- renderUI({
        data = data()
        data.epiestim = data.epiestim()
        
        if(nrow(data) > 1 & !is.null(input$R.hover)) {
            nearPoint <- info_on_hover_func(Plot_Rstat(), input$R.hover, R.ranges)
            nearPoint$date = as.character(nearPoint$date)
            data1 = data %>% mutate(date = as.character(date)) %>% 
                        filter(date == nearPoint$date) %>%
                        select(date, region, Rs) %>%
                        mutate(Rs = round(Rs, 3))
            data2 = data.epiestim %>% mutate(date = as.character(date)) %>% 
                filter(date == nearPoint$date) %>%
                select(c(date, region, `Mean(R)`, `Quantile.0.025(R)`, `Quantile.0.975(R)`)) %>%
                mutate(`Mean(R)` = round(`Mean(R)`, 3),
                       `Quantile.0.025(R)` = round(`Quantile.0.025(R)`,3),
                       `Quantile.0.975(R)` = round(`Quantile.0.975(R)`,3))
            data = full_join(data1, data2, by=c("date", "region"))# %>% unite("pastestring", -date, sep=", ")
            output_string = info_display_func(data, c("Rs", "EpiEstim_R", "EpiEstim_R0.025", "EpiEstim_R0.975"))
            HTML(output_string)
            

        } else {
            NULL   
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
