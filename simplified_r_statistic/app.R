#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Load Packages ========================

library(shiny)
library(tidyverse)
library(ggpubr)
library(RcppRoll)
library(EpiEstim)
library(R0)
library(shinyjs)
library(shinycssloaders)

# Load Data ========================

source("./Estimate_R_Functions.R")
DATA = read_csv("./case_data.csv", col_types = "ccccDddld")
initial_data = read_csv("./initial_data.csv", col_types = "ccccDddlddcddddddd")

# Javascript Functions ===================

download_node_js = "var elem = document.getElementById('download-data');
                     var topPos = elem.offsetTop;
                     document.getElementById('md_file').scrollTop = topPos;"

standardize_node_js = "var elem = document.getElementById('standardize-data');
                       var topPos = elem.offsetTop;
                       document.getElementById('md_file').scrollTop = topPos;"

bind_node_js = "var elem = document.getElementById('bind_data');
                var topPos = elem.offsetTop;
                document.getElementById('md_file').scrollTop = topPos;"

clean_main_js = paste("var elem = document.getElementById('clean-data');
                       var topPos = elem.offsetTop;
                       document.getElementById('md_file').scrollTop = topPos;", sep="")

clean_node_js = paste("var elem = document.getElementById('clean_", 1:3, "');
                       var topPos = elem.offsetTop;
                       document.getElementById('md_file').scrollTop = topPos;", sep="")

geo_node_js = "var elem = document.getElementById('select-region-to-plot');
               var topPos = elem.offsetTop;
               document.getElementById('md_file').scrollTop = topPos;"
 
smooth_node_js = "var elem = document.getElementById('smooth-data');
                  var topPos = elem.offsetTop;
                  document.getElementById('md_file').scrollTop = topPos;"

estimateR_mainnode_js = "var elem = document.getElementById('estimate-r_t');
                         var topPos = elem.offsetTop;
                         document.getElementById('md_file').scrollTop = topPos;"

simpleR_node_js = "var elem = document.getElementById('simple-ratio-method');
                   var topPos = elem.offsetTop;
                   document.getElementById('md_file').scrollTop = topPos;"

coriR_node_js = "var elem = document.getElementById('cori-et-al-2013-r_t-estimation');
                   var topPos = elem.offsetTop;
                   document.getElementById('md_file').scrollTop = topPos;"

wtR_node_js = "var elem = document.getElementById('wallinga-teunis-2004');
                   var topPos = elem.offsetTop;
                   document.getElementById('md_file').scrollTop = topPos;"

wlR_node_js = "var elem = document.getElementById('wallinga-lipsitch-2007');
                   var topPos = elem.offsetTop;
                   document.getElementById('md_file').scrollTop = topPos;"

# UI ========================

ui <- fluidPage(
    
    navbarPage(title = "Rt Estimator",
    
        tabPanel("Main", fluid = TRUE,
            
            ## * Location & Date ============================
            
            fluidRow(
              column(4, 
                wellPanel(
                   selectInput(inputId = "region",
                               label = "Search Region (Country, US State, or US County):",
                               choices = c("Select regions" = "", unique(DATA$region)),
                               multiple = T,
                               selected = "World")
              )),
              column(4,
                wellPanel(
                    dateRangeInput(inputId = "dateRange",
                                    label = "Select Date Range:",
                                    start = max(DATA$date) - 56,
                                    end = max(DATA$date))
              )),
              
              ## * Main Description ============================
              
              column(4, 
                tags$span("This app demonstrates a simple ratio method for estimating the instataneous reproduction rate (Rt) of SARS-CoV-2 virus in any country, 
                          US State, and US County. In the app, you can compare this estimation of Rt to previously published methods. The Rt in the average number of people who will be infected by an individual 
                          who has the virus. This number is determined by both biological characteristics of the virus as well as human 
                          behavior, therefore it changes over time. For the pandemic to end, we must see the Rt value stay below 1.")
              )
              
            ),
            
            ## * Controls ============================
            
            fluidRow(style="font-size: 0.85em",
            column(3,
        
                wellPanel(
                  tags$p("Control How Rt is Estimated:", style="font-size:20px; font-weight:bold"),
                  tags$b("1. Select method for estimating Rt"),
                  tags$p("Intro page explains each method", style="margin-bottom:1.5em"),
                  checkboxGroupInput(inputId = "Method", 
                                     label = NULL,
                                     choiceNames = c("Simple Ratio", "Cori et al (2013)", "Wallinga & Teunis (2004)", "Wallinga & Lipsitch (2007)"),
                                     choiceValues = c("simple", "cori", "WT", "WL"),
                                     selected = "simple"),
                  
                    tags$b("2. Smoothing Window"),
                    tags$p("We smooth new cases per day with a rolling mean prior to Rt estimation to reduce noise. Select the size of the window for the rolling mean:"),
                    sliderInput(inputId = "smoothing_window",
                                label = NULL,
                                min = 1, max = 14, value = 1, step = 1),
                    
                    tags$b("3. Define Generation Interval"),
                    tags$p("Generation Inteval is the time difference between when one person is infected to when they cause an infection in another person. 
                           All methods of estimating Rt are dependent on this interval"),
                    
                    sliderInput(inputId = "GT_mean",
                                label = "Mean Generation Interval (days)",
                                min = 1, max = 10, value = 4, step = 1),
                    
                    sliderInput(inputId = "GT_SD",
                                label = "Standard Deviation of Generation Interval (days)",
                                min = 1, max = 10, value = 3, step = 1),
                    
                    tags$b("4. Period Size"),
                    tags$p("For most methods of estimating Rt (except for Wallinga & Teunis Method), we compare the sum of new cases that occur
                            over two time periods (instead of just comparing new cases for two different days). 
                           Select the size (in days) of this period:"),
                    sliderInput(inputId = "tau",
                                label = NULL,
                                min = 1, max = 14, value = 7, step = 1)
                )
            ),
        
            ## * Plots ============================
            
            column(6,
              tags$p("Hover near lines in plots for more information"),
              
              div( 
                style = "position:relative",
                plotOutput("plot_R", height="400px", 
                           hover = hoverOpts("plot_hover_1", delay = 100, delayType = "debounce"),
                           dblclick = dblclickOpts("plot_dblclick_1")) %>% withSpinner(color="#0dc5c1"),
                uiOutput("hover_info_1")
              ),
              
              fluidRow(              
                    tags$b("Select data to display on secondary plot"),
                    splitLayout(cellWidths = c("15%", "15%"),
                        actionLink("button_CumulativeCases", "Cumulative Cases"),
                        actionLink("button_CumulativeDeaths", "Cumulative Deaths"),
                        actionLink("button_NewCases", "New Cases")
                    )),
                
              div( 
                style = "position:relative",
                plotOutput("secondary_plot", height="400px", 
                           hover = hoverOpts("plot_hover_2", delay = 100, delayType = "debounce")) %>% withSpinner(color="#0dc5c1"),
                uiOutput("hover_info_2")
              ),
              
            ),
            
            ## * Left Panel ============================
            
            column(3, 
               
               ## ** Score Cards ============================
               fixedRow(
                   tags$h4("Weekly Report Card:", style="font-size:20px; font-weight:bold"),
                   HTML("<p> Below is a weekly report for the last 3 weeks of data available. For each week displayed, 
                        <span style='font-weight: bold; background-color: limegreen'>green</span> 
                        indicates that on average the upper bound of the Rt was below 1. 
                        <span style='font-weight: bold; background-color: yellow'>Yellow</span> 
                        indicates that on average Rt esimate was below 1, but the upper bound of the estimate was above 1. 
                        <span style='font-weight: bold; background-color: orange'>Orange</span> 
                        indicates that on average the Rt esimate was above 1, but the lower bound of the estimate was below 1. 
                        <span style='font-weight: bold; background-color: red'>Red</span> 
                        indicates that on average the lower bound of the Rt estimate was above 1."),
                   htmlOutput("Score_Card")
               ),
               
               tags$br(),
               
               ## ** Update Info ============================
               fixedRow(
                   uiOutput("Last_Update")
               )
            )
        )), 
        
        # Other Tabs ===================
        
        tabPanel("Introduction", fluid=TRUE, withMathJax(includeMarkdown("Introduction.md"))),
        tabPanel("Methods", fluid = TRUE,
                 tags$head(tags$style(HTML(".node:hover polygon {fill: red;} .node {cursor:pointer}"))),
                 tags$head(tags$style(HTML("#clust2:hover text {fill: red} #clust2 {cursor:pointer}"))),
                 tags$head(tags$style(HTML("#clust3:hover text {fill: red} #clust3 {cursor:pointer}"))),
                 useShinyjs(),
                 column(6, uiOutput('flow_chart') %>% withSpinner(color="#0dc5c1")),
                 column(6, uiOutput("md_file", style = "overflow-y:scroll; max-height: 600px") %>% withSpinner(color="#0dc5c1"))
        ), 
        tabPanel("About", fluid = TRUE, includeMarkdown("About.md"))
    )
)   

# Server =====================

server <- function(input, output, clientData, session) {
  
    # Update Data ================= 
    
    data_holder <- reactiveValues(data = initial_data)
    
    
    # ** Update on Rt ================= 
    
    R_estimation_func <- function(data, var_D, var_D_sd, var_tau, method) {
      
      data <- data %>%
        arrange(date) %>%
        group_by(region, region_type, regionID, regionID_type) %>%
        nest(CD = -c(region, region_type, regionID, regionID_type))
      
      parameters_to_input = 
      data <- data %>% 
        mutate(R_estimate = map(CD, function(CD_) { 
          parameters_to_input = list(date = CD_$date, Is = CD_$new_cases_smoothed, si_mean = var_D, si_sd = var_D_sd, tau = var_tau)
          return(do.call(method, parameters_to_input))}
               )) %>%
        mutate(CD = map2(CD, R_estimate, left_join, by="date")) %>%
        dplyr::select(-R_estimate)
      
      data = data %>% unnest(CD) %>% ungroup()
      
      data = data %>% pivot_longer(cols=contains(".R_"), 
                                   names_to = c("Method", ".value"),
                                   names_sep=".R_")
      
      return(data)
    }
    
    observeEvent({
      c(input$region,
        input$Method,
        input$tau,
        input$GT_mean,
        input$GT_SD,
        input$smoothing_window)
    },
    {
      validate(
        need(input$region != "", "Please select a region")
      )
      
      validate(
        need(input$Method != "", "Please select a method for estimating R")
      )
      
      
      previous_parameters = data_holder$data %>%
        dplyr::select(smoothing_window, tau, GT_mean, GT_SD, Method, region) %>%
        unique()
      
      parameters_to_display = list(GT_mean = input$GT_mean, 
                                   GT_SD = input$GT_SD, 
                                   smoothing_window = input$smoothing_window, 
                                   tau = input$tau, 
                                   region = input$region, 
                                   Method = input$Method)

      parameters_to_display = parameters_to_display %>% cross(.) %>% transpose() %>% map(unlist) %>% as.tibble()
      
      parameters_to_calculate = anti_join(parameters_to_display, previous_parameters) 
     
      
      if(nrow(parameters_to_calculate) > 0) {
        
        # Select Geography
        new_data <- DATA %>% filter(region %in% input$region)
        
        # Smooth data
        new_data <- smooth_new_cases(new_data, input$smoothing_window)
        
        # Calculate Rt
        
        new_data = parameters_to_calculate %>% as.list() %>% transpose() %>% map(unlist) %>%
          map(., function(x) R_estimation_func(data = new_data, 
                                               var_D = as.numeric(x[[1]]),
                                               var_D_sd = as.numeric(x[[2]]),
                                               var_tau = as.numeric(x[[4]]),
                                               method = paste0("EstimateR.", x[6])))
        
        # name data
        names(new_data) = parameters_to_calculate %>% 
          dplyr::select(c("smoothing_window", "GT_mean", "GT_SD", "tau")) %>% 
          unite(col="parameters", sep=",") %>%
          unlist()
        
        new_data = bind_rows(new_data, .id = "parameters")
        new_data = new_data %>% separate(col = parameters, into = c("smoothing_window", "GT_mean", "GT_SD", "tau"))
        new_data = new_data %>% mutate(
                                 smoothing_window = as.numeric(smoothing_window), 
                                 GT_mean = as.numeric(GT_mean),
                                 GT_SD = as.numeric(GT_SD), 
                                 tau = as.numeric(tau))
        
        # add it back to data holder
        data_holder$data <- bind_rows(data_holder$data, new_data)
      }
    })
    
    ## Render Plotting Info ===================
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
    
    data_to_plot <- reactive({
      
      validate(
        need(input$region != "", "Please select a region")
      )
      
      validate(
        need(input$Method != "", "Please select a method for estimating R")
      )
      
      
      data = data_holder$data %>% filter(smoothing_window == input$smoothing_window,
                                         GT_mean == input$GT_mean,
                                         GT_SD == input$GT_SD,
                                         tau == input$tau, 
                                         Method %in% input$Method,
                                         region %in% input$region)
      
      data = data %>% filter(date >= input$dateRange[1] & date <= input$dateRange[2])
      
      data$Method = factor(data$Method, levels = c("simple", "cori", "WT", "WL"))
      data$Method = data$Method %>% fct_recode(`Simple Ratio` = "simple",
                                               Cori = "cori",
                                               `Walinga & Teunis` = "WT", 
                                               `Walinga & Lipsitch` = "WL")
      
      
      data$region = factor(data$region, levels = input$region)
      
      return(data)
    })
    
    ## Render R Estimation Plot =====================
    
    show_full_plot <- reactiveValues(p = F)
    
    observeEvent(input$plot_dblclick_1, {
      if (show_full_plot$p) {
        show_full_plot$p = F
      } else {
        show_full_plot$p = T
      }
    })
    
    # reset when region or dates change
    observeEvent(c(input$region, input$dateRange), {show_full_plot$p = F})
    
    plot_R = reactive({
      
      validate(
        need(input$region != "", "Please select a region")
      )
      
      validate(
        need(input$Method != "", "Please select a method for estimating R")
      )
         
      # Initialize Data for Plot
      data = data_to_plot()
    
      min_val = min(data$Quantile_025, na.rm=T)
      ymin = ifelse(min_val < 0.6, min_val*0.8, 0.5)
      
      max_data = max(data$Quantile_975, na.rm=T)
      
      if(show_full_plot$p) {
        ymax = ifelse(max_data > (1.5/1.1), max_data*1.1, 1.5)  
      } else {
        ymax = ifelse(max_data > (1.5/1.1), max_data*1.1, 1.5)
        ymax = ifelse(ymax > 5, 5, ymax)
      }
      
      missing_data_warning = ifelse(ymax < (max_data*1.1), "data beyond bounds of plot\ndouble-click to see full data", "")
     
      p = ggplot(data = data) +
          geom_ribbon(aes(x=date, ymin=Quantile_025, ymax=Quantile_975, fill=region, group=interaction(Method, region)), alpha=0.2) +
          geom_line(aes(x=date, y=mean, color=region, linetype=Method), size=1) +
          geom_hline(yintercept = 1, linetype = 2) +
          annotate("text",  x=input$dateRange[1], y = ymax, label = missing_data_warning, hjust=0, color="red") +
          scale_x_date(date_breaks = '1 week', date_labels = '%b-%d') +
          scale_linetype_manual(values = c(1,2,3,4), labels = levels(data$Method), drop=F) + 
          ylim(ymin, ymax) + 
          labs(x="Date", y="Rt Estimate") +
          custom_plot_theme() +
          theme(axis.text.x = element_text(angle=45, hjust=1),
                legend.position = "bottom",
                legend.key.size = unit(1.5, "line"),
                legend.text = element_text(size=15),
                legend.box="vertical", 
                legend.margin=margin())

      return(p)
    })
    

    
    output$plot_R = renderPlot({plot_R()})
    
    output$hover_info_1 <- renderUI({
      hover <- input$plot_hover_1
      
      # Initialize Data
      data = data_to_plot()
      
      point <- nearPoints(data, hover, threshold = 20, maxpoints = 1, yvar="mean", addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_px + 2, "px; top:", top_px + 2, "px;")

      # actual tooltip created as wellPanel
      wellPanel(
        style = style,
        p(HTML(paste0("<b> Date: </b>", point$date, "<br/>",
                      "<b> Region: </b>", point$region, "<br/>",
                      "<b> Method: </b>", point$Method, "<br/>",
                      "<b> Rt Estimate: </b>", round(point$mean,2), " (", round(point$Quantile_025,2), "-", round(point$Quantile_975,2), ")")))
      )
    })
    
    
    ## Render Secondary Plot =========================
    
    # Ref: https://5harad.com/mse125/r/visualization_code.html
    addUnits <- function(n) {
        labels <- ifelse(n < 1000, n,  # less than thousands
                         ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                                ifelse(n < 1e9, paste0(round(n/1e6), 'M'),  # in millions
                                       ifelse(n < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                              'too big!'
                                       ))))
        return(labels)
    }
    
    secondary_plot <- reactiveValues(p = "cumulative_cases")

    observeEvent(input$button_CumulativeCases, {
        secondary_plot$p <- "cumulative_cases"
    })

    observeEvent(input$button_NewCases, {
        secondary_plot$p <- "new_cases"
    })
    
    observeEvent(input$button_CumulativeDeaths, {
      secondary_plot$p <- "cumulative_deaths"
    })

    output$secondary_plot = renderPlot({
      data = data_to_plot()
      
        if(secondary_plot$p == "cumulative_cases") {
         
          p = ggplot(data) +
            geom_line(aes(x=date, y=cumulative_cases, color = region), size = 1) +
            labs(y = "Cumulative Cases") +
            scale_y_continuous(labels = addUnits) +
            scale_x_date(date_breaks = '1 week', date_labels = '%b-%d') +
            labs(x = "Date") +
            custom_plot_theme() +
            theme(axis.text.x = element_text(angle=45, hjust=1),
                  legend.position = "none")
          
        } else if (secondary_plot$p == "new_cases") {
          
          p = ggplot(data = data) +
            geom_line(aes(x = date, y = new_cases, group = region, color = region), linetype=2) +
            geom_line(aes(x = date, y = new_cases_smoothed, group = region, color = region), linetype=1, size=1) +
            labs(y = "New Cases") +
            scale_y_continuous(labels = addUnits, limits = c(0, NA)) +
            scale_x_date(date_breaks = '1 week', date_labels = '%b-%d') +
            labs(x = "Date") +
            custom_plot_theme() +
            theme(axis.text.x = element_text(angle=45, hjust=1),
                  legend.position = "none")

        } else if (secondary_plot$p == "cumulative_deaths") {
          
          p = ggplot(data = data) +
            geom_line(aes(x=date, y=cumulative_deaths, color = region), size = 1) +            
            labs(y = "Cumulative Deaths") +
            scale_y_continuous(labels = addUnits, limits = c(0, NA)) +
            scale_x_date(date_breaks = '1 week', date_labels = '%b-%d') +
            labs(x = "Date") +
            custom_plot_theme() +
            theme(axis.text.x = element_text(angle=45, hjust=1),
                  legend.position = "none")
        } 
      
      return(p)
    })
    
     
    output$hover_info_2 <- renderUI({
      # Taken from: https://gitlab.com/snippets/16220
      hover <- input$plot_hover_2
      
      data = data_to_plot()
      point <- nearPoints(data, hover, threshold = 20, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)

      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_px + 2, "px; top:", top_px + 2, "px;")

      # actual tooltip created as wellPanel
      if (secondary_plot$p == "cumulative_cases") {
        wellPanel(
          style = style,
          p(HTML(paste0("<b> Date: </b>", point$date, "<br/>",
                        "<b> Region: </b>", point$region, "<br/>",
                        "<b> Cumulative Cases: </b>", round(point$cumulative_cases), "<br/>")))
        )
      } else if (secondary_plot$p == "new_cases") {
        wellPanel(
          style = style,
          p(HTML(paste0("<b> Date: </b>", point$date, "<br/>",
                        "<b> Region: </b>", point$region, "<br/>",
                        "<b> New Cases: </b>", round(point$new_cases), "<br/>",
                        "<b> Smoothed New Cases: </b>", round(point$new_cases_smoothed), "<br/>")))
        )
      } else if (secondary_plot$p == "cumulative_deaths") {
        wellPanel(        
          style = style,
          p(HTML(paste0("<b> Date: </b>", point$date, "<br/>",
                        "<b> Region: </b>", point$region, "<br/>",
                        "<b> Cumulative Deaths: </b>", round(point$cumulative_deaths), "<br/>")))
        )
      }
    })
    

    # Render Score Card ==========
  
    output$Score_Card <- renderUI({

        data = data_holder$data %>% filter(smoothing_window == input$smoothing_window,
                                           GT_mean == input$GT_mean,
                                           GT_SD == input$GT_SD,
                                           tau == input$tau, 
                                           Method %in% input$Method,
                                           region %in% input$region)

        selected_regions = input$region
        if(is.null(selected_regions)){selected_regions = "World"}

        week_start = c(14,7,0)
        score_cards = vector(mode="list", length=length(selected_regions))

        for(i in 1:length(selected_regions)) {
            data_subset = data %>% filter(region == selected_regions[i])

            score_cards_j = vector(mode="list", length=length(week_start))

            for (j in seq_along(week_start)) {

                start_date = max(data_subset$date) - (week_start[j]+7)
                end_date = max(data_subset$date) - week_start[j]

                data_subset.2 = data_subset %>% filter(date >= start_date & date < end_date)
                data_subset.2 = data_subset.2 %>% filter(Method %in% input$Method)

                if(nrow(data_subset.2) == 0){
                    next
                }

                # Get mean lower and upper bounds of R estimates
                Rmax = data_subset.2$Quantile_975 %>% mean(na.rm=T) %>% round(2)
                Rmin = data_subset.2$Quantile_025 %>% mean(na.rm=T) %>% round(2)
                Rmean = data_subset.2$mean %>% mean(na.rm=T) %>% round(2)
            
                # Dates to put on score cards
                score_card_dates = paste(format(start_date, "%b-%d"), "to",format(end_date, "%b-%d"))

                # Title to put on score cards
                relative_start_date = as.numeric(Sys.Date() - start_date)/7
                relative_end_date = as.numeric(Sys.Date() - end_date)/7
                relative_week = (relative_start_date + relative_end_date)/2

                if(relative_week < 1) {
                  score_card_week = "This Past Week"
                } else if (relative_week < 2 & relative_week >= 1) {
                  score_card_week = "Last Week"
                } else if (relative_week >= 2) {
                  score_card_week = paste(floor(relative_week), "Weeks Ago")
                }
                
                if(sum(is.na(data_subset.2$mean)) == length(data_subset.2$mean)) {
                  score_cards_j[j] =
                    
                    paste("<svg height='120' width='110'>",
                          "<rect width='110' height='120' rx='15' fill='", "rgb(190,190,190)", "' />",
                          "<text fill='#000000' font-size='10' font-family='Verdana' font-weight='bold' x='50%' y='20', text-anchor='middle'>",score_card_week,"</text>",
                          "<text fill='#000000' font-size='8' font-family='Verdana' x='50%' y='40', text-anchor='middle'>",score_card_dates,"</text>",
                          "<text fill='#000000' font-size='10' font-family='Verdana' x='50%' y='60', text-anchor='middle'>Average R Range:</text>",
                          "<text fill='#000000' font-size='10' font-family='Verdana' font-weight='bold' x='50%' y='77', text-anchor='middle'> not enough data </text>",
                          "Sorry, your browser does not support inline SVG.",
                          "</svg>", sep="")
                  next
                } else if(Rmean < 1 & Rmax <= 1) {
                  color_assingment = "rgb(0,255,0)"
                } else if (Rmean < 1 & Rmax > 1) {
                  color_assingment = "rgb(255,255,0)"
                } else if (Rmean >= 1 & Rmin < 1) {
                  color_assingment = "rgb(255,165,0)"
                } else if (Rmean >= 1 & Rmin >= 1) {
                  color_assingment = "rgb(255,0,0)"
                }
                
                score_cards_j[j] =

                    paste("<svg height='120' width='110'>",
                            "<rect width='110' height='120' rx='15' fill='", color_assingment, "' />",
                            "<text fill='#000000' font-size='10' font-family='Verdana' font-weight='bold' x='50%' y='20', text-anchor='middle'>",score_card_week,"</text>",
                            "<text fill='#000000' font-size='8' font-family='Verdana' x='50%' y='40', text-anchor='middle'>",score_card_dates,"</text>",
                            "<text fill='#000000' font-size='10' font-family='Verdana' x='50%' y='60', text-anchor='middle'>Average R Range:</text>",
                            "<text fill='#000000' font-size='10' font-family='Verdana' font-weight='bold' x='50%' y='77', text-anchor='middle'> ",Rmin,"-", Rmax, "</text>",
                            "<text fill='#000000' font-size='10' font-family='Verdana' x='50%' y='95', text-anchor='middle'> based on </text>",
                            "<text fill='#000000' font-size='10' font-family='Verdana' x='50%' y='110', text-anchor='middle'>",sum(!is.na(data_subset.2$mean )), " day(s) of data</text>",
                            "Sorry, your browser does not support inline SVG.",
                        "</svg>", sep="")
            }

            score_cards[i] = paste(score_cards_j, collapse=" ")
            score_cards[i] = paste("<p><b>",input$region[i],"</b></p>", score_cards[i])

         }

         score_cards = paste(score_cards, collapse=" ")
         HTML(score_cards)
    })
    
    # Code Page==========
    
    # Load html
    output$md_file <- renderUI({withMathJax(includeHTML("Code_Explanation.html"))})
    
    # Load graphviz flowchart
    output$flow_chart <- renderUI({includeHTML("Process_Flow_Chart.html")}) 
    
    # Click functions on graphviz nodes
    observe({shinyjs::onclick("node1", runjs(download_node_js))})
    observe({shinyjs::onclick("node3", runjs(download_node_js))})
    observe({shinyjs::onclick("node5", runjs(download_node_js))})
    observe({shinyjs::onclick("node2", runjs(standardize_node_js))})
    observe({shinyjs::onclick("node4", runjs(standardize_node_js))})
    observe({shinyjs::onclick("node6", runjs(standardize_node_js))})
    observe({shinyjs::onclick("node7", runjs(bind_node_js))})
    observe({shinyjs::onclick("node8", runjs(clean_node_js[1]))})
    observe({shinyjs::onclick("node9", runjs(clean_node_js[2]))})
    observe({shinyjs::onclick("node10", runjs(clean_node_js[3]))})
    observe({shinyjs::onclick("node11", runjs(geo_node_js))})
    observe({shinyjs::onclick("node12", runjs(smooth_node_js))})
    observe({shinyjs::onclick("node13", runjs(simpleR_node_js))})
    observe({shinyjs::onclick("node14", runjs(coriR_node_js))})
    observe({shinyjs::onclick("node15", runjs(wtR_node_js))})
    observe({shinyjs::onclick("node16", runjs(wlR_node_js))})
    
    observe({shinyjs::onclick("clust2", runjs(clean_main_js))})
    observe({shinyjs::onclick("clust3", runjs(estimateR_mainnode_js))})

    # Update Info===============
    
    output$Last_Update <- renderUI({
        includeHTML("Update.html")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
