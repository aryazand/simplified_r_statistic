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
library(R0)
library(shinyjs)

# ****************
# Load Data
# ****************

DATA = read_csv("./case_data.csv", col_types = "ccccDddl")

# ****************
# Javascript Functions
# ****************

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

# *************
# User Interface 
# *************

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # *************
    # Title and Description
    # *************
    
    # Application title
    navbarPage("R Estimator",
        tabPanel("Main", fluid = TRUE,
            fluidRow(includeMarkdown("Introduction.md")),
            
            # ---------------
            # Controls
            # ---------------
            
            column(3,
                wellPanel(
                    
                    selectInput(inputId = "geographic_location",
                                label = "Search & Select Geographic Location (multiple selection allowed):",
                                choices = c("Select regions" = "", unique(DATA$region)),
                                multiple = T,
                                selected = "World"),
                    
                    dateRangeInput(inputId = "dateRange",
                                   label = "select dates",
                                   start = Sys.Date() - 56,
                                   end = Sys.Date())
                ), 
        
                wellPanel(
                    
                    tags$h4("Smooth Daily New Cases"),
                    
                    sliderInput(inputId = "smoothing_window",
                                label = "Smoothing Window Size",
                                min = 1, max = 14, value = 7, step = 1),
                    
                    tags$h4("Parameters for R calculation"),
                    
                    tags$head(
                        tags$style(
                            HTML(".checkbox-inline {margin-left: 0px; margin-right: 10px;}
                                  .checkbox-inline+.checkbox-inline {margin-left: 0px; margin-right: 10px;}"
                                 )
                        ) 
                    ),
                    
                    checkboxGroupInput(inputId = "R_type", 
                                       label = "Select Method of R Calculation",
                                       choiceNames = c("Simple", "Cori et al (2013)", "Wallinga & Teunis (2004)", "Wallinga & Lipsitch (2007)"),
                                       choiceValues = c("KN", "Cori", "TD", "WL"),
                                       inline = T,
                                       selected = "KN"),
                    
                    sliderInput(inputId = "var.si_mean",
                                label = "Mean Serial Interval (days)",
                                min = 1, max = 10, value = 4, step = 1),
                    
                    sliderInput(inputId = "var.si_sd",
                                label = "Standard Deviation of Serial Interval (days)",
                                min = 1, max = 10, value = 3, step = 1),
                    
                    sliderInput(inputId = "var.window_size",
                                label = "Window Size",
                                min = 2, max = 14, value = 7, step = 1)
                )
            ),
        
            # -------------
            # Plots
            # -------------
            column(6,
                
                plotOutput("plot_R", height="400px",
                     hover = hoverOpts(id ="plot_R.hover", delay=40, delayType = "debounce", ),
                     dblclick = "plot_R.dblclk",
                     brush = brushOpts(id = "plot_R.brush", resetOnNew = TRUE)
                ),
        
                fluidRow(
                    tags$b("Select data to display on secondary plot"),
                    splitLayout(cellWidths = c("15%", "15%"),
                        actionLink("button_TotalCases", "Total Cases"),
                        actionLink("button_NewCases", "New Cases")
                    )
                ),
                
                plotOutput("secondary_plot", height="400px", 
                           hover = hoverOpts(id ="secondary_plot.hover", delay=40, delayType = "debounce"),
                           dblclick = "secondary_plot.dblclk",
                           brush = brushOpts(id = "secondary_plot.brush", resetOnNew = TRUE))
            ),
            
            column(3, 
                   
               fixedRow(
                   htmlOutput("Score_Card")
               ),
               
               fixedRow(
                   verbatimTextOutput("Plot_Info")
               ),
               
               fixedRow(
                   uiOutput("Last_Update")
               )
            )
        ), 
        tabPanel("Code", fluid = TRUE,
                 tags$head(tags$style(HTML(".node:hover polygon {fill: red;} .node {cursor:pointer}"))),
                 useShinyjs(),
                 column(6, uiOutput('flow_chart')),
                 column(6, uiOutput("md_file", style = "overflow-y:scroll; max-height: 600px"))
        ), 
        tabPanel("About", fluid = TRUE, includeMarkdown("About.md"))
    )
)   

# --------------
# Server
# --------------

server <- function(input, output, clientData, session) {
    
    # ----------
    # Update Data
    # ----------
    
    data <- reactive({
        data <- DATA %>% 
            filter(region %in% input$geographic_location) %>%
            mutate(region = factor(region, levels=c(input$geographic_location))) #%>%
            #filter(!is.na(start_date))
        
        if(nrow(data) == 0) {
            data <- DATA %>% filter(region == "World")
        }
        #---------------------
        # Establish Parameters
        #----------------------
        var_tau = input$var.window_size # Window size
        var_D = input$var.si_mean  # Mean serial interval 
        var_D_sd = input$var.si_sd # SD in serial interval
        
        #---------------------
        # Smooth Data
        #----------------------
        
        data = data %>%
            group_by(region, region_type, regionID, regionID_type) %>%
            mutate(new_cases_smoothed = roll_mean(new_cases, n = input$smoothing_window, align="center", fill = c(NA, NA, NA), na.rm=T)) %>%
            mutate(new_cases_smoothed = replace(new_cases_smoothed, is.na(new_cases_smoothed), new_cases[is.na(new_cases_smoothed)])) %>%
            ungroup()
        
        data$new_cases_smoothed[is.na(data$new_cases_smoothed)] = data$new_cases[is.na(data$new_cases_smoothed)]
        
        data = data %>% 
          filter(!is.na(new_cases_smoothed)) %>%
          group_by(region, region_type, regionID, regionID_type) %>%
          mutate(reference_date = date[min(which((new_cases_smoothed > 0)))]) %>%
          filter(date >= reference_date) %>%
          dplyr::select(-reference_date) %>%
          ungroup()
        
        #---------------------------------
        # Calculate R by published methods
        #---------------------------------
        
        source("./Estimate_R_Functions.R")
    
        data <- data %>%
            arrange(date) %>%
            group_by(region, region_type, regionID, regionID_type) %>%
            nest(CD = -c(region, region_type, regionID, regionID_type)) %>%
            mutate(estimateR = map(CD, function(CD_) EstimateR.simple(date = CD_$date, Is = CD_$new_cases_smoothed, si_mean = var_D, tau = var_tau))) %>%
            mutate(CD = map2(CD, estimateR, left_join, by="date")) %>%
            mutate(estimateR = map(CD, function(CD_) EstimateR.cori(date = CD_$date, Is = CD_$new_cases_smoothed, si_mean = var_D, si_sd = var_D_sd, tau = var_tau))) %>%
            mutate(CD = map2(CD, estimateR, left_join, by="date")) %>%
            mutate(estimateR = map(CD, function(CD_) EstimateR.WT(date = CD_$date, Is = CD_$new_cases_smoothed, si_mean = var_D, si_sd = var_D_sd))) %>%
            mutate(CD = map2(CD, estimateR, left_join, by="date")) %>%
            mutate(estimateR = map(CD, function(CD_) EstimateR.WL(date = CD_$date, Is = CD_$new_cases_smoothed, si_mean = var_D, si_sd = var_D_sd, tau = var_tau))) %>%
            mutate(CD = map2(CD, estimateR, left_join, by="date")) %>%
            dplyr::select(region, region_type, regionID, regionID_type, CD) %>%
            unnest(CD) %>% 
            ungroup()
    })
    
    # ------------
    # Render R Estimation Plot
    # ------------
    
    # custom function to zoom in plot
    function_to_zoom <- function(brush, ranges) {
        
        # Code from: https://shiny.rstudio.com/gallery/plot-interaction-zoom.html
        
        if (!is.null(brush)) {
            ranges$x <- as.Date(c(brush$xmin, brush$xmax), origin="1970-01-01")
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- as.Date(c(input$dateRange[1], input$dateRange[2]), origin="1970-01-01")
            ranges$y <- NULL
        }
    }

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
    
    #Define plot ranges
    plot_R.ranges <- reactiveValues(x = NULL, y = NULL)
    
    observeEvent(input$plot_R.dblclk, {
        function_to_zoom(input$plot_R.brush, plot_R.ranges)
    })
    
    
    plot_R <- reactive({
        data = data()
        
        data = data %>% filter(date >= input$dateRange[1] & date <= input$dateRange[2])
        
        # If regions are selected
        if(nrow(data) < 1) {
            warning("select geographic location")
        } else {
            
            if(is.null(plot_R.ranges$y[2])) {
                ymax = data %>% dplyr::select(contains(input$R_type)) %>%
                    map_dbl(., max, na.rm=T) %>% max(., na.rm=T)
                ymax = ymax*1.1
                
                if (ymax < 1.5) {
                    ymax = 1.5
                }
            } else {
                ymax = plot_R.ranges$y[2]
            }

            p = ggplot(data = data) +
                geom_hline(yintercept = 1, linetype = 2) +
                labs(subtitle = "Shaded Region gives 95% CI for each R estimate", y = "R estimate") +
                scale_x_date(date_breaks = '1 week', date_labels = '%b-%d') +
                coord_cartesian(xlim = plot_R.ranges$x, ylim = c(0.5, ymax), expand = FALSE) +
                custom_plot_theme() +
                theme(axis.text.x = element_text(angle=45, hjust=1))    
            
            for(R_type in input$R_type) {
                
                y = paste(R_type, "R_mean", sep=".")
                ymin = paste(R_type, "R_Quantile_025", sep=".")
                ymax = paste(R_type, "R_Quantile_975", sep=".")
                
                linetypes = c(KN = 1, Cori = 2, TD = 3, WL = 4)
                l = linetypes[R_type == names(linetypes)]

                p = p +
                    geom_ribbon(aes_string(x = "date",  ymin=ymin,ymax=ymax, group = "region", fill = "region"), alpha=0.15) +
                    geom_line(aes_string(x = "date", y = y, color = "region", group = "region"), linetype = l, size =1.25)
            }

            return(p)
        }
    })
    
    output$plot_R = renderPlot({plot_R()})
    
    # ------------
    # Render Total Cases Plot
    # ------------
    
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
    
    plot_TotalCases <- reactive({
        
        data = data() %>% filter(date >= input$dateRange[1] & date <= input$dateRange[2])
        
        data = data %>% split(f = data$INTERPROLATED)
        
        #Identify breaks in data
        ID_breaks_in_data <- function(df) {
            # from https://stackoverflow.com/questions/14821064/line-break-when-no-data-in-ggplot2
            idx <- c(1, diff(df$date))
            i2 <- c(1,which(idx != 1), nrow(df)+1)
            df$date_grps <- rep(1:length(diff(i2)), diff(i2))
            return(df)
        }
        
        data = map(data, ID_breaks_in_data) %>% bind_rows()
        
        p = ggplot() +
            geom_line(data = data %>% filter(INTERPROLATED == T), 
                      aes(x = date, y = total_cases, group = date_grps, color = region), size=1.5, linetype=3) +
            geom_line(data = data %>% filter(INTERPROLATED == F),
                      aes(x = date, y = total_cases, group = date_grps, color = region), size=1.5, linetype=1) +
            labs(y = "Total Cases") + 
            scale_y_continuous(labels = addUnits, limits = c(0, NA)) + 
            scale_x_date(date_breaks = '1 week', date_labels = '%b-%d') +
            custom_plot_theme() +
            theme(axis.text.x = element_text(angle=45, hjust=1))    
        
        return(p)
    })
    
    # ------------
    # Render New Cases Plot
    # ------------
    
    plot_NewCases <- reactive({
        
        data = data() %>% filter(date >= input$dateRange[1] & date <= input$dateRange[2])
        
        p = ggplot(data = data) +
            geom_line(aes(x = date, y = new_cases, group = region, color = region), linetype=2) +
            geom_line(aes(x = date, y = new_cases_smoothed, group = region, color = region), linetype=1, size=1.5) +
            labs(y = "New Cases") + 
            scale_y_continuous(labels = addUnits, limits = c(0, NA)) + 
            scale_x_date(date_breaks = '1 week', date_labels = '%b-%d') +
            custom_plot_theme() +
            theme(axis.text.x = element_text(angle=45, hjust=1))    
        
        return(p)
    })
    
    
    #--------------
    # Show Secondary Plot
    #--------------
    
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
    
    # ----------------
    # Render Score Card
    # -----------------
    
    output$Score_Card <- renderUI({
        
        data = data()
        
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
    
    # ------------------------
    # Render Plot Information
    # ------------------------
    
    # *************
    # Hover Info
    # *************
    
    output$Plot_Info <- renderPrint({
        data = data()
        
        print("Hover mouse over plot for more info")
        print("Click-and-Drag to select a plot area.")
        print("Double-click to zoom in and out.")
        
        if(any(!(input$geographic_location %in% unique(data$region)))) {
            regions_without_data = input$geographic_location[!(input$geographic_location %in% unique(data$region))]
            regions_without_data = paste(regions_without_data, sep=", ")
            warning_annotation = paste("WARNING: The following selected region(s) do not have enough data:", regions_without_data)
            print("***********************************")
            print("*             WARNING             *")
            print("***********************************")
            print(warning_annotation)
        }
    })
    
    observeEvent(input$plot_R.hover, ignoreNULL = F, {
        
        data = data()
        hover_info = input$plot_R.hover
        
        if(!is.null(hover_info)) {
          
          # Get date associated with hover location
          hover.date = as.Date(hover_info$x, origin="1970-01-01")
          
          # Mak a vertical line in the other plot
          output$secondary_plot = renderPlot({
            if(secondary_plot$p == "total_cases") {
              plot_TotalCases() + geom_vline(xintercept = hover.date)
            } else {
              plot_NewCases() + geom_vline(xintercept = hover.date)
            }    
          })
          
          # If there is associated data then print in information window
          if(nrow(data) > 0) {
            
            data = data %>% filter(date == as.character(hover.date)) %>%
              dplyr::select(date, region, contains(input$R_type)) %>%
              mutate_if(is.numeric, round, digits=2)
            data = data %>% pivot_longer(-c(1:2), names_to = c("R_type", ".value"), names_sep = "\\.")
            colnames(data) = gsub("R_", "", colnames(data))
            colnames(data)[3] = "R Estimation"
            data = data %>% unite(5:6, col="95% CI", sep="-")    
            
            data$`R Estimation` = factor(data$`R Estimation`)
            
            data$`R Estimation` = fct_recode(data$`R Estimation`, Simple = "KN", 
                                             `Cori et al (2013)` = "Cori", 
                                             `Wallinga & Teunis (2004)` = "TD",
                                             `Wallinga & Lipsitch (2007)`  = "WT")
            
            data = as.data.frame(data)
            
            output$Plot_Info <- renderPrint({ 
              print("Hover mouse over plot for more info")
              print("Click-and-Drag to select a plot area.")
              print("Double-click to zoom in and out.")
              
              if(any(!(input$geographic_location %in% unique(data$region)))) {
                regions_without_data = input$geographic_location[!(input$geographic_location %in% unique(data$region))]
                regions_without_data = paste(regions_without_data, sep=", ")
                warning_annotation = paste("WARNING: The following selected region(s) do not have enough data:", regions_without_data)
                print("***********************************")
                print("*             WARNING             *")
                print("***********************************")
                print(warning_annotation)
              }
              
              print("***********************************")
              print("* INFROMATION FROM MOUSE POSITION *")
              print("***********************************")
              
              print(paste("Date:", data$date[1]))
              
              data = split(data, f=data$region)
              data = map(data, function(x) x %>% dplyr::select(`R Estimation`, mean, `95% CI`))
              for(i in seq_along(data)) {
                print(names(data)[i])
                print(data[[i]])
              }
            })
          }
        } else { 
            
            output$secondary_plot = renderPlot({
              if(secondary_plot$p == "total_cases") {
                plot_TotalCases()
              } else {
                plot_NewCases()
              }    
            })
            
            output$Plot_Info <- renderPrint({ 
                print("Hover mouse over plot for more info")
                print("Click-and-Drag to select a plot area.")
                print("Double-click to zoom in and out.")
                
                if(any(!(input$geographic_location %in% unique(data$region)))) {
                    regions_without_data = input$geographic_location[!(input$geographic_location %in% unique(data$region))]
                    regions_without_data = paste(regions_without_data, sep=", ")
                    warning_annotation = paste("WARNING: The following selected region(s) do not have enough data:", regions_without_data)
                    print("***********************************")
                    print("*             WARNING             *")
                    print("***********************************")
                    print(warning_annotation)
                }
            })
        }
    })
    
    observeEvent(input$secondary_plot.hover, ignoreNULL = F, {
        
        data = data()
        hover_info = input$secondary_plot.hover
        
        if(!is.null(hover_info)) {
          hover.date = as.Date(hover_info$x, origin="1970-01-01")
          
          output$plot_R = renderPlot({
            plot_R() + geom_vline(xintercept = hover.date)
          })
          
          if(nrow(data) > 0) {
            
            data = data %>% filter(date == as.character(hover.date)) %>%
              dplyr::select(region, total_cases, new_cases) %>%
              mutate_if(is.numeric, round, digits=2)
            
            data = as.data.frame(data)
            
            output$Plot_Info <- renderPrint({ 
              print("Hover mouse over plot for more info")
              print("Click-and-Drag to select a plot area.")
              print("Double-click to zoom in and out.")
              
              if(any(!(input$geographic_location %in% unique(data$region)))) {
                regions_without_data = input$geographic_location[!(input$geographic_location %in% unique(data$region))]
                regions_without_data = paste(regions_without_data, sep=", ")
                warning_annotation = paste("WARNING: The following selected region(s) do not have enough data:", regions_without_data)
                print("***********************************")
                print("*             WARNING             *")
                print("***********************************")
                print(warning_annotation)
              }
              
              print("***********************************")
              print("* INFROMATION FROM MOUSE POSITION *")
              print("***********************************")
              
              print(paste("Date:", hover.date))
              print(data)
            })
          }
        } else {
          
            output$plot_R = renderPlot({plot_R()})
          
            output$Plot_Info <- renderPrint({ 
                print("Hover mouse over plot for more info")
                print("Click-and-Drag to select a plot area.")
                print("Double-click to zoom in and out.")
                
                if(any(!(input$geographic_location %in% unique(data$region)))) {
                    regions_without_data = input$geographic_location[!(input$geographic_location %in% unique(data$region))]
                    regions_without_data = paste(regions_without_data, sep=", ")
                    warning_annotation = paste("WARNING: The following selected region(s) do not have enough data:", regions_without_data)
                    print("***********************************")
                    print("*             WARNING             *")
                    print("***********************************")
                    print(warning_annotation)
                }
            })
        }
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
    observe({shinyjs::onclick("node16", runjs(estimateR_mainnode_js))})
    
    #-----------
    # Update Info
    # -----------
    
    output$Last_Update <- renderUI({
        includeHTML("Update.html")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
