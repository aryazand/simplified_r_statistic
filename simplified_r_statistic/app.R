#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Load Packages ========================

library(plotly)
library(shiny)
library(tidyverse)
library(ggpubr)
library(RcppRoll)
library(EpiEstim)
library(R0)
library(shinyjs)
library(shinycssloaders)
library(grid)

# Load Data ========================

source("./Estimate_R_Functions.R")
DATA = read_csv("./case_data.csv", col_types = "ccccDddl")
initial_data = read_csv("./initial_data.csv")

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

simpleR_node_js = "var elem = document.getElementById('simple-r-calcuation');
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
    
    navbarPage(HTML("Rt Estimator"),
    
        tabPanel("Main", fluid = TRUE,
            
            ## * Location & Date ============================
            
            fluidRow(
              column(4, 
                wellPanel(style = "height:100px",
                   selectInput(inputId = "geographic_location",
                               label = "Search Region (Country, US State, or US County):",
                               choices = c("Select regions" = "", unique(DATA$region)),
                               multiple = T,
                               selected = "World")
              )),
              column(4,
                wellPanel(style = "height:100px",
                    dateRangeInput(inputId = "dateRange",
                                    label = "Select Date Range:",
                                    start = max(data$date) - 56,
                                    end = max(data$date))
              )),
              column(4, 
                tags$span("This app allows you to view the instataneous reproduction rate (Rt) of SARS-CoV-2 virus over time in
                          all countries, US States, and US Counties using 4 different methods. More information on how to use this
                          app and the how to interpret the graphs can be found in the Inroduction page.")      
              )
              
            ),
            
            ## * Controls ============================
            
            fluidRow(style="font-size: 0.85em",
            column(3,
        
                wellPanel(
                  tags$p("Controls:", style="font-size:20px; font-weight:bold"),
                  tags$b("1. Select method for estimating Rt"),
                  tags$p("Intro page explains each method", style="margin-bottom:1.5em"),
                  checkboxGroupInput(inputId = "R_type", 
                                     label = NULL,
                                     choiceNames = c("Simple Ratio", "Cori et al (2013)", "Wallinga & Teunis (2004)", "Wallinga & Lipsitch (2007)"),
                                     choiceValues = c("Simple Ratio", "Cori", "WT", "WL"),
                                     selected = "Simple Ratio"),
                  
                    tags$b("2. Smoothing Window"),
                    tags$p("We smooth new cases per day with a rolling mean to reduce noise. Select the size of the window for the rolling mean:"),
                    sliderInput(inputId = "smoothing_window",
                                label = NULL,
                                min = 1, max = 14, value = 7, step = 1),
                    
                    tags$b("3. Define Generation Interval"),
                    tags$p("Generation Inteval is the time different between when one person is infected to when they cause an infection in another person. 
                           All methods of estimating Rt are dependent on this interval"),
                    
                    sliderInput(inputId = "var.si_mean",
                                label = "Mean Generation Interval (days)",
                                min = 1, max = 10, value = 4, step = 1),
                    
                    sliderInput(inputId = "var.si_sd",
                                label = "Standard Deviation of Generation Interval (days)",
                                min = 1, max = 10, value = 3, step = 1),
                    
                    tags$b("4. Period Size"),
                    tags$p("For most methods of estimating Rt (except for Wallinga & Teunis Method), we analyze the number of new cases
                           over a period of days. Select the size of this period:"),
                    sliderInput(inputId = "var.window_size",
                                label = NULL,
                                min = 1, max = 14, value = 7, step = 1)
                )
            ),
        
            ## * Plots ============================
            
            column(6,
              
              div( 
                style = "position:relative",
                plotOutput("plot_R", height="400px", 
                           hover = hoverOpts("plot_hover_1", delay = 100, delayType = "debounce")) %>% withSpinner(color="#0dc5c1"),
                uiOutput("hover_info_1")
              ),
              
              fluidRow(              
                    tags$b("Select data to display on secondary plot"),
                    splitLayout(cellWidths = c("15%", "15%"),
                        actionLink("button_TotalCases", "Total Cases"),
                        actionLink("button_NewCases", "New Cases")
                    )),
                
              div( 
                style = "position:relative",
                plotOutput("secondary_plot", height="400px", 
                           hover = hoverOpts("plot_hover_2", delay = 100, delayType = "debounce")),
                uiOutput("hover_info_2")
              ),
              
            ),
            
            column(3, 
                   
               fixedRow(
                   tags$h4("Weekly Score Card:", style="font-size:20px; font-weight:bold"),
                   htmlOutput("Score_Card")
               ),
               
               tags$br(),
               
               fixedRow(
                   uiOutput("Last_Update")
               )
            )
        )), 
        
        tabPanel("Introduction", fluid=TRUE, withMathJax(includeMarkdown("Introduction.md"))),
        tabPanel("Methods", fluid = TRUE,
                 tags$head(tags$style(HTML(".node:hover polygon {fill: red;} .node {cursor:pointer}"))),
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
    
    data_holder <- reactiveValues(data = initial_data, 
                                  data2 = initial_data %>%
                                      pivot_longer(cols=contains(".R_"), names_to = c("Method", ".value"), names_sep=".R_") %>%
                                      filter(!is.na(mean) & !is.na(Quantile_025) & !is.na(Quantile_975)) %>%
                                      mutate(Method = factor(Method, levels=c("Simple Ratio", "Cori", "WT", "WL"))))
    
    # ** Update on Parameter Change ================= 
    
    observeEvent({
      c(input$geographic_location,
      input$smoothing_window,
      input$var.window_size,
      input$var.si_mean,
      input$var.si_sd,
      input$var.tau)
    },{
      print("doing an update (1)")
      
      # Validate basic have been selected
      validate(
        need(input$geographic_location != "", "Please select a region")
      )
      
      validate(
        need(input$R_type != "", "Please select a method for estimating R")
      )
      
        data <- DATA %>% 
            filter(region %in% input$geographic_location) %>%
            mutate(region = factor(region, levels=c(input$geographic_location)))
        
        if(nrow(data) == 0) {
            data <- DATA %>% filter(region == "World")
        }
        

        # Establish Parameters
        var_tau = input$var.window_size # Window size
        var_D = input$var.si_mean  # Mean generation interval 
        var_D_sd = input$var.si_sd # SD in generation interval
        
        # Smooth Data
        data = smooth_new_cases(data, smoothing_window = input$smoothing_window)

        # Estimate R

        # Calculate Simple R by default
        data <- data %>%
          arrange(date) %>%
          group_by(region, region_type, regionID, regionID_type) %>%
          nest(CD = -c(region, region_type, regionID, regionID_type)) %>% 
          mutate(estimateR = map(CD, function(CD_) EstimateR.simple(date = CD_$date, Is = CD_$new_cases_smoothed, si_mean = var_D, tau = var_tau))) %>%
          mutate(CD = map2(CD, estimateR, left_join, by="date")) %>% 
          dplyr::select(-estimateR)
        
        if("Cori" %in% input$R_type) {
          data <- data %>% 
            mutate(estimateR.cori = map(CD, function(CD_) EstimateR.cori(date = CD_$date, Is = CD_$new_cases_smoothed, si_mean = var_D, si_sd = var_D_sd, tau = var_tau))) %>%
            mutate(CD = map2(CD, estimateR.cori, left_join, by="date")) %>%
            dplyr::select(-estimateR.cori)
        }
        
        if("WT" %in% input$R_type) {
          data <- data %>% 
            mutate(estimateR.TD = map(CD, function(CD_) EstimateR.WT(date = CD_$date, Is = CD_$new_cases_smoothed, si_mean = var_D, si_sd = var_D_sd))) %>%
            mutate(CD = map2(CD, estimateR.TD, left_join, by="date")) %>%
            dplyr::select(-estimateR.TD)
        }
        
        if("WL" %in% input$R_type) {
          data <- data %>% 
            mutate(estimateR.WL = map(CD, function(CD_) EstimateR.WL(date = CD_$date, Is = CD_$new_cases_smoothed, si_mean = var_D, si_sd = var_D_sd, tau = var_tau))) %>%
            mutate(CD = map2(CD, estimateR.WL, left_join, by="date")) %>%
            dplyr::select(-estimateR.WL)
        }
        
        data = data %>% unnest(CD) %>% ungroup()
        
        data_holder$data <- data
    }, ignoreInit = T)
    
    # ** Update on R_type Change ================= 
    
    observeEvent(input$R_type,
      {
        print("doing an update (2)")
        validate(
          need(input$geographic_location != "", "Please select a region")
        )
        
        validate(
          need(input$R_type != "", "Please select a method for estimating R")
        )
        
        
        data = data_holder$data 

        # Establish Parameters
        var_tau = input$var.window_size # Window size
        var_D = input$var.si_mean  # Mean generation interval
        var_D_sd = input$var.si_sd # SD in generation interval

        # Estimate R by published methods
        data <- data %>%
            arrange(date) %>%
            group_by(region, region_type, regionID, regionID_type) %>%
            nest(CD = -c(region, region_type, regionID, regionID_type))
      
        
        if(("Cori" %in% input$R_type) & !any(grepl("Cori", colnames(data$CD[[1]])))) {
            
            data <- data %>% 
                mutate(estimateR.cori = map(CD, function(CD_) EstimateR.cori(date = CD_$date, Is = CD_$new_cases_smoothed, si_mean = var_D, si_sd = var_D_sd, tau = var_tau))) %>%
                mutate(CD = map2(CD, estimateR.cori, left_join, by="date")) %>%
                dplyr::select(-estimateR.cori)
        }

        if("WT" %in% input$R_type & !any(grepl("^WT", colnames(data$CD[[1]])))) {
            data <- data %>% 
                mutate(estimateR.TD = map(CD, function(CD_) EstimateR.WT(date = CD_$date, Is = CD_$new_cases_smoothed, si_mean = var_D, si_sd = var_D_sd))) %>%
                mutate(CD = map2(CD, estimateR.TD, left_join, by="date")) %>%
                dplyr::select(-estimateR.TD)
        }

        if("WL" %in% input$R_type & !any(grepl("^WL", colnames(data$CD[[1]])))) {
            data <- data %>% 
                mutate(estimateR.WL = map(CD, function(CD_) EstimateR.WL(date = CD_$date, Is = CD_$new_cases_smoothed, si_mean = var_D, si_sd = var_D_sd, tau = var_tau))) %>%
                mutate(CD = map2(CD, estimateR.WL, left_join, by="date")) %>%
                dplyr::select(-estimateR.WL)
        }

        data = data %>% unnest(CD) %>% ungroup()
        
        data_holder$data <- data
    }, ignoreInit = T)
    
    # ** Convert to long format ================= 
    
    observeEvent(data_holder$data, {
      print("doing an update (3)")
      data = data_holder$data
      
      data = data %>% pivot_longer(cols=contains(".R_"), 
                                   names_to = c("Method", ".value"),
                                   names_sep=".R_")
      
      data = data %>% mutate(region = factor(region, levels=input$geographic_location),
                             Method = factor(Method, levels=c("Simple Ratio", "Cori", "WT", "WL")))
      
      levels(data$Method)[3:4] =c("Walinga & Teunis", "Walinga & Lipsitch")
      
      data_holder$data2 = data     
    }, ignoreInit = T)
    
    ## Render R Estimation Plot =====================
    
    # Cutom ggplot theme
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
    
    plot_R = reactive({
      
      validate(
        need(input$geographic_location != "", "Please select a region")
      )
      
      validate(
        need(input$R_type != "", "Please select a method for estimating R")
      )
         
      # Initialize Data for Plot
      data = data_holder$data2
      data = data %>% filter(date >= input$dateRange[1] & date <= input$dateRange[2])
      data$region = factor(data$region, levels=input$geographic_location)
      
      ymin = min(data$Quantile_025, na.rm=T)
      ymin = ifelse(ymin < 0.6, ymin*0.8, 0.5)
      ymax = max(data$Quantile_975, na.rm=T)
      ymax = ifelse(ymax > (1.5/1.1), ymax*1.1, 1.5)
      
      p = ggplot(data = data) +
          geom_ribbon(aes(x=date, ymin=Quantile_025, ymax=Quantile_975, fill=region, group=interaction(Method, region)), alpha=0.2) +
          geom_line(aes(x=date, y=mean, color=region, linetype=Method)) +
          geom_hline(yintercept = 1, linetype = 2) +
          scale_x_date(date_breaks = '1 week', date_labels = '%b-%d') +
          ylim(ymin, ymax) + 
          labs(x="Date", y="Rt Estimate") +
          custom_plot_theme() +
          theme(axis.text.x = element_text(angle=45, hjust=1),
                legend.position = "bottom")
      return(p)
    })
    
    output$plot_R = renderPlot({plot_R()})
    
    output$hover_info_1 <- renderUI({
      hover <- input$plot_hover_1
      point <- nearPoints(data_holder$data2, hover, threshold = 10, maxpoints = 1, yvar="mean", addDist = TRUE)
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
    
    ## ** Create Total_Cases Plot =========================
    
    plot_TotalCases <- reactive({

        data = data_holder$data %>% filter(date >= input$dateRange[1] & date <= input$dateRange[2])

        p = ggplot(data) +
            geom_line(aes(x=date, y=total_cases, color = region), size = 1) +
            labs(y = "Total Cases") +
            scale_y_continuous(labels = addUnits) +
            scale_x_date(date_breaks = '1 week', date_labels = '%b-%d') +
            labs(x = "Date") +
            custom_plot_theme() +
            theme(axis.text.x = element_text(angle=45, hjust=1),
                  legend.position = "none")

        return(p)
    })

    ## ** Create New_Cases Plot =========================

    plot_NewCases <- reactive({

        data = data_holder$data %>% filter(date >= input$dateRange[1] & date <= input$dateRange[2])

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

        return(p)
    })


    ## ** Display Secondary Plot =========================

    secondary_plot <- reactiveValues(p = "total_cases")

    observeEvent(input$button_TotalCases, {
        secondary_plot$p <- "total_cases"
    })

    observeEvent(input$button_NewCases, {
        secondary_plot$p <- "new_cases"
    })

    output$secondary_plot = renderPlot({

        if(secondary_plot$p == "total_cases") {
            plot_TotalCases()
        } else {
            plot_NewCases()
        }

    })
    
    output$hover_info_2 <- renderUI({
      # Taken from: https://gitlab.com/snippets/16220
      hover <- input$plot_hover_2
      point <- nearPoints(data_holder$data, hover, threshold = 10, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      print(hover)
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      if(secondary_plot$p == "total_cases") {
        wellPanel(
          style = style,
          p(HTML(paste0("<b> Date: </b>", point$date, "<br/>",
                        "<b> Region: </b>", point$region, "<br/>",
                        "<b> Total Cases: </b>", round(point$total_cases), "<br/>")))
        )
      } else {
        wellPanel(
          style = style,
          p(HTML(paste0("<b> Date: </b>", point$date, "<br/>",
                        "<b> Region: </b>", point$region, "<br/>",
                        "<b> New Cases: </b>", round(point$new_cases), "<br/>",
                        "<b> Smoothed New Cases: </b>", round(point$new_cases_smoothed), "<br/>")))
        )
      }
    })
    

    # Render Score Card ==========
    
    output$Score_Card <- renderUI({

        data = data_holder$data

        selected_regions = input$geographic_location
        if(is.null(selected_regions)){selected_regions = "World"}

        week_start = c(21,14,7)
        score_cards = vector(mode="list", length=length(selected_regions))

        for(i in 1:length(selected_regions)) {
            data_subset = data %>% filter(region == selected_regions[i])

            score_cards_j = vector(mode="list", length=length(week_start))

            for (j in seq_along(week_start)) {

                data_subset.2 = data_subset %>% filter(date >= (Sys.Date() - (week_start[j]+7)) & date < (Sys.Date() - week_start[j]))
                data_subset.2 = data_subset.2 %>% dplyr::select(starts_with(input$R_type))

                if(nrow(data_subset.2) == 0){
                    next
                }

                # Get mean lower and upper bounds of R estimates
                Rmax = data_subset.2 %>%
                    dplyr::select(contains("R_Quantile_975")) %>%
                    unlist() %>% mean(na.rm=T) %>% round(2)

                Rmin = data_subset.2 %>%
                    dplyr::select(contains("R_Quantile_025")) %>%
                    unlist() %>% mean(na.rm=T) %>% round(2)

                Rmean = data_subset.2 %>%
                    dplyr::select(contains("R_mean")) %>%
                    unlist() %>% mean(na.rm=T) %>% round(2)

                if(Rmean < 1 & Rmax <= 1) {
                    color_assingment = "rgb(0,255,0)"
                } else if (Rmean < 1 & Rmax > 1) {
                    color_assingment = "rgb(255,255,0)"
                } else if (Rmean >= 1 & Rmin < 1) {
                    color_assingment = "rgb(255,165,0)"
                } else if (Rmean >= 1 & Rmin >= 1) {
                    color_assingment = "rgb(255,0,0)"
                }

                score_card_dates = paste(format(Sys.Date() - (week_start[j]+7), "%b-%d"), "to",format(Sys.Date() - week_start[j], "%b-%d"))

                score_cards_j[j] =

                    paste("<svg height='100' width='100'>",
                            "<rect width='100' height='100' rx='15' fill='", color_assingment, "' />",
                            "<text fill='#000000' font-size='10' font-family='Verdana' x='50%' y='30', text-anchor='middle'>Average R Range:</text>",
                            "<text fill='#000000' font-size='10' font-family='Verdana' x='50%' y='50', text-anchor='middle'>",score_card_dates,"</text>",
                            "<text fill='#000000' font-size='10' font-family='Verdana' font-weight='bold' x='50%' y='70', text-anchor='middle'> ",Rmin,"-", Rmax, "</text>",
                            "Sorry, your browser does not support inline SVG.",
                        "</svg>", sep="")
            }

            score_cards[i] = paste(score_cards_j, collapse=" ")
            score_cards[i] = paste("<p><b>",input$geographic_location[i],"</b></p>", score_cards[i])

         }

         score_cards = paste(score_cards, collapse=" ")
         HTML(score_cards)
    })
    
    #-----------
    # Code Page
    #------------
    
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
    observe({shinyjs::onclick("node17", runjs(estimateR_mainnode_js))})
    
    #-----------
    # Update Info
    # -----------
    
    output$Last_Update <- renderUI({
        includeHTML("Update.html")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
