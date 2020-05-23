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

# ****************
# Load Data
# ****************

DATA = read_csv("./case_data.csv", col_types = "ccccDddl")

# Remove lead trail of 0s 
DATA = DATA %>% 
    group_by(region, region_type, regionID, regionID_type) %>%
    arrange(date) %>%
    mutate(filtering = new_cases > 0) %>%
    mutate(filtering = roll_sum(filtering, n=7, align="left", fill=c(NA,NA,NA), na.rm=T)) %>%
    mutate(start_date = ifelse(length(which(filtering > 1))>1, date[min(which(filtering > 1))], NA)) %>%
    filter(date >= start_date | is.na(start_date)) %>%
    dplyr::select(-filtering) %>%
    ungroup()

# *************
# User Interface 
# *************

# Define UI for application that draws a histogram
ui <- fluidPage(

    # *************
    # Title and Description
    # *************
    
    # Application title
    titlePanel("R Estimator"),
    
    fluidRow(
        tags$span("Here we show multiple methods of estimating the effective reproduction number (R)"),
        tags$b("Note: We have made assumptions in calculating this R values. 
               Neither those assumptions or the code for this tool have been peer-reviewed"),
        tags$span("We will be adding information on the code and how these R estimations are calculated soon."),
        tags$br(),
        tags$br()
        
    ),
    
    # ---------------
    # Controls
    # ---------------
    fluidRow(
        
    column(3,
        wellPanel(
            
            tags$h3("Simple Controls"),
            
            selectInput(inputId = "geographic_location",
                        label = "Search & Select Geographic Location (multiple selection allowed):",
                        choices = c("Select regions" = "", unique(DATA$region)),
                        multiple = T,
                        selected = "World"),
            
            dateRangeInput(inputId = "dateRange",
                           label = "select dates",
                           start = "2020-02-01",
                           end = Sys.Date())
        ), 
        
        fixedRow(
            verbatimTextOutput("Plot_Info")
        ),
    
    ), 

    
    # -------------
    # Plots
    # -------------
    

    column(6,
        fixedRow(
            plotOutput("plot_R",
                 hover = hoverOpts(id ="plot_R.hover", delay=50, delayType = "debounce"),
                 dblclick = "plot_R.dblclk",
                 brush = brushOpts(id = "plot_R.brush", resetOnNew = TRUE)
            )
        ),
        
        fixedRow(
            htmlOutput("Score_Card")
        )
    ),
    
    column(3, 
           wellPanel(
               tags$h3("Advanced Controls"),
               
               tags$h4("Smooth Daily New Cases"),
               
               sliderInput(inputId = "smoothing_window",
                           label = "Smoothing Window Size",
                           min = 4, max = 14, value = 7, step = 1),
               
               tags$h4("Parameters for R calculation"),
               
               checkboxGroupInput(inputId = "R_type", 
                                  label = "Select Method of R Calculation",
                                  choiceNames = c("Simple", "Cori et al (2013)", "Wallinga & Teunis (2004)", "RKI (2020)"),
                                  choiceValues = c("KN", "Cori", "TD", "RKI"),
                                  inline = F,
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
    )
    
    )
    
)   

# --------------
# Server
# --------------

server <- function(input, output, clientData, session) {
    
    # ----------
    # Update Data
    # ----------
    
    data_subset <- reactive({
        data_subset <- DATA %>% 
            filter(region %in% input$geographic_location) %>%
            mutate(region = factor(region, levels=c(input$geographic_location))) %>%
            filter(!is.na(start_date))
        
        if(nrow(data_subset) == 0) {
            data_subset <- DATA %>% filter(region == "World")
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
        
        data_subset = data_subset %>%
            group_by(region, region_type, regionID, regionID_type) %>%
            mutate(new_cases_smoothed = roll_mean(new_cases, n = input$smoothing_window, align="center", fill = c(NA, NA, NA), na.rm=T)) %>%
            ungroup()
        
        data_subset$new_cases_smoothed[is.na(data_subset$new_cases_smoothed) & data_subset$new_cases > 0] = data_subset$new_cases[is.na(data_subset$new_cases_smoothed) & data_subset$new_cases > 0]
        
        data_subset = data_subset %>% filter(!is.na(new_cases_smoothed))
        #---------------------------------
        # Calculate R by published methods
        #---------------------------------
        
        estimate.R_helper = function(dates, new_cases, nsim=1000, method=c("KN", "Cori", "TD", "RKI")) {
            
            if(method  == "TD") {
                mGT = generation.time("gamma", c(var_D, var_D_sd))
                names(new_cases) = dates
                r_estimates = estimate.R(new_cases, GT = mGT, begin=1, end=as.numeric(length(new_cases)), 
                                         methods=method, nsim=nsim)
                
                df = data.frame(
                    date = seq.Date(as.Date(r_estimates$estimates[[1]]$begin, origin="1970-01-01"), 
                                    length.out = length(r_estimates$estimates[[1]]$R), by = 1),
                    R_mean = r_estimates$estimates[[1]]$R,
                    R_Quantile_025 = r_estimates$estimates[[1]]$conf.int[[1]],
                    R_Quantile_975 = r_estimates$estimates[[1]]$conf.int[[2]]
                ) 
                
                # for some reason last R estimate from TD always ends up as 0. So we'll remove that
                df = df[-c(1, nrow(df)),]
                
            } else if(method == "Cori") {
                r_estimates = estimate_R(new_cases, method = "parametric_si", config = make_config(list(mean_si = var_D, 
                                                                 std_si = var_D_sd,
                                                                 t_start = seq(2, length(new_cases) - var_tau + 1),
                                                                 t_end = seq(var_tau+1, length(new_cases))))
                                )
                df = data.frame(
                    date = tail(dates, -var_tau) - var_tau,
                    R_mean = r_estimates$R$`Mean(R)`,
                    R_Quantile_025 = r_estimates$R$`Quantile.0.025(R)`,
                    R_Quantile_975 = r_estimates$R$`Quantile.0.975(R)`
                )  
            } else if(method == "KN") {
                weekly_sum = roll_sum(new_cases, n=var_tau, align="right", fill = c(NA, NA, NA))
                denominator = head(c(rep(NA, var_D), weekly_sum), var_D*-1)
                denominator = replace(denominator, which(denominator == 0), NA)
                R_mean = weekly_sum/denominator
                R_sd = sqrt(R_mean/denominator)
                R_Quantile_025 = qnorm(0.025, mean = R_mean, sd = R_sd)
                R_Quantile_975 = qnorm(0.975, mean = R_mean, sd = R_sd)
                
                df = data.frame(
                    date = dates,
                    R_mean,
                    R_Quantile_025,
                    R_Quantile_975
                )
            } else if (method == "RKI") {
                generation_time_sum = roll_sum(new_cases, n=var_D, align="right", fill = c(NA, NA, NA))
                denominator = head(c(rep(NA, var_D), generation_time_sum), var_D*-1)
                denominator = replace(denominator, which(denominator == 0), NA)
                R_mean = generation_time_sum/denominator
                R_sd = sqrt(R_mean/denominator)
                R_Quantile_025 = qnorm(0.025, mean = R_mean, sd = R_sd)
                R_Quantile_975 = qnorm(0.975, mean = R_mean, sd = R_sd)
                
                df = data.frame(
                    date = dates - var_tau,
                    R_mean,
                    R_Quantile_025,
                    R_Quantile_975
                )
            }
            
            else {
              warning("method must be one of Cori, TD, or SB")  
            }
        
            colnames(df)[2:4] = paste(method, colnames(df)[2:4], sep=".")
            
            return(df)
        }
    
        data_subset <- data_subset %>%
            arrange(date) %>%
            group_by(region, region_type, regionID, regionID_type) %>%
            nest(CD = -c(region, region_type, regionID, regionID_type)) %>%
            mutate(estimateR = map(CD, function(CD_) estimate.R_helper(CD_$date, CD_$new_cases_smoothed, method="KN"))) %>%
            mutate(CD = map2(CD, estimateR, left_join, by="date")) %>%
            mutate(estimateR = map(CD, function(CD_) estimate.R_helper(CD_$date, CD_$new_cases_smoothed, method="RKI"))) %>%
            mutate(CD = map2(CD, estimateR, left_join, by="date")) %>%
            mutate(estimateR = map(CD, function(CD_) estimate.R_helper(CD_$date, CD_$new_cases_smoothed, method="Cori"))) %>%
            mutate(CD = map2(CD, estimateR, left_join, by="date")) %>%
            mutate(estimateR = map(CD, function(CD_) estimate.R_helper(CD_$date, CD_$new_cases_smoothed, method="TD"))) %>%
            mutate(CD = map2(CD, estimateR, left_join, by="date")) %>%
            dplyr::select(region, region_type, regionID, regionID_type, CD) %>%
            unnest(CD) %>% 
            ungroup()
    })
    
    # ------------
    # Render Plot
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
        data = data_subset()
        
        data = data %>% filter(date >= input$dateRange[1] & date <= input$dateRange[2])
        
        # If regions are selected
        if(nrow(data) < 1) {
            warning("select geographic location")
        } else {

            p = ggplot(data = data) +
                geom_hline(yintercept = 1, linetype = 2) +
                labs(subtitle = "Shaded Region gives 95% CI for each R estimate", y = "R estimate") +
                scale_x_date(date_breaks = '1 week', date_labels = '%b-%d') +
                coord_cartesian(xlim = plot_R.ranges$x, ylim = plot_R.ranges$y, expand = FALSE) +
                custom_plot_theme() +
                theme(axis.text.x = element_text(angle=45, hjust=1))    
            
            for(R_type in input$R_type) {
                
                y = paste(R_type, "R_mean", sep=".")
                ymin = paste(R_type, "R_Quantile_025", sep=".")
                ymax = paste(R_type, "R_Quantile_975", sep=".")
                
                linetypes = c(KN = 1, Cori = 2, TD = 3, RKI = 4)
                l = linetypes[R_type == names(linetypes)]

                p = p +
                    geom_ribbon(aes_string(x = "date",  ymin=ymin,ymax=ymax, group = "region", fill = "region"), alpha=0.15) +
                    geom_line(aes_string(x = "date", y = y, color = "region", group = "region"), linetype = l)
            }

            return(p)
        }
    })
        
    output$plot_R = renderPlot({plot_R()})
    

    # ----------------
    # Render Score Card
    # -----------------
    
    output$Score_Card <- renderUI({
        
        data = data_subset()
        data = data %>% filter(date >= Sys.Date() - 14 & date < Sys.Date() - 7)
        
        selected_regions = input$geographic_location
        if(is.null(selected_regions)){selected_regions = "World"}
        
        score_cards = vector(mode="list", length=length(selected_regions))
        
        for(i in 1:length(selected_regions)) {
            region = selected_regions[i]
            
            data_subset = data %>% filter(region == selected_regions[i]) 
            
            # Get min and max R estimates
            Rmax = data_subset %>%
                dplyr::select(contains("R_Quantile_975")) %>% 
                map_dbl(., mean, na.rm=T) %>% mean(., na.rm=T) %>%
                round(.,2)
            
            Rmin = data_subset %>%
                dplyr::select(contains("R_Quantile_025")) %>% 
                map_dbl(., mean, na.rm=T) %>% mean(., na.rm=T) %>%
                round(.,2)
            
            print(unique(data_subset$region))
            #normalize to between 0 and 1
            # ymax = Rmax
            # ymin = Rmin
            # if(ymax > 2) {ymax = 1.5} else if (ymax < 0.5) {ymax = 0.5}
            # if(ymin > 2) {ymin = 1.5} else if (ymin < 0.5) {ymin = 0.5}
            # 
            # ymax = (ymax-0.5)/(1.5-0.5)
            # ymin = (ymin-0.5)/(1.5-0.5)
            # 
            # color_limits = colorRamp(c("green", "red"))(c(ymax,ymin)) %>% apply(., 1, paste, collapse=",")
            # 
            # # Get mean R estimate
            # # R_mean = data %>% filter(region == region) %>% select(contains(".R_mean")) %>%
            # #             summarise_all(mean, na.rm=T) %>% .[1,] %>% mean(.,na.rm=T)
            # #     
            
            if(Rmin > 1) {
                min_color = "255,0,0"
            } else if (Rmin < 0.95) {
                min_color = "0,255,0"
            } else {
                min_color = "255,255,0"
            }
            
            if(Rmax > 1) {
                max_color = "255,0,0"
            } else if (Rmax < 0.95) {
                max_color = "0,255,0"
            } else {
                max_color = "255,255,0"
            }
            
            score_cards[i] = 

                paste("<svg height='100' width='100'>",
                        "<defs>",
                            "<linearGradient id='grad", i,"' gradientTransform='rotate(90)'>",
                              "<stop offset='0%' style='stop-color:rgb(", max_color, ");stop-opacity:1' />",
                              "<stop offset='100%' style='stop-color:rgb(", min_color,");stop-opacity:1' />",
                            "</linearGradient>",
                        "</defs>",
                        "<rect width='100' height='100' fill='url(#grad",i,")' />",
                        "<text fill='#000000' font-size='10' font-family='Verdana' x='50%' y='20', text-anchor='middle'>", input$geographic_location[i] ,"</text>",
                        "<text fill='#000000' font-size='10' font-family='Verdana' x='50%' y='40', text-anchor='middle'>Last week's</text>",
                        "<text fill='#000000' font-size='10' font-family='Verdana' x='50%' y='60', text-anchor='middle'>Average R Range</text>",
                        "<text fill='#000000' font-size='10' font-family='Verdana' x='50%' y='80', text-anchor='middle'> ",Rmin,"-", Rmax, "</text>",
                        "Sorry, your browser does not support inline SVG.",
                    "</svg>", sep="")
         }
         
         score_cards = paste(score_cards, collapse=" ")
         print(score_cards)
         HTML(score_cards)
    })
    
    # ------------------------
    # Render Plot Information
    # ------------------------
    
    # *************
    # Hover Info
    # *************

    output$Plot_Info <- renderPrint({
        data = data_subset()
        
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
        if(nrow(data) > 0 & !is.null(input$plot_R.hover)) {
            hover_info = input$plot_R.hover
            hover.date = as.Date(hover_info$x, origin="1970-01-01")

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
                                                                 `RKI (2020)` = "RKI")
            
            print("***********************************")
            print("* INFROMATION FROM MOUSE POSITION *")
            print("***********************************")
            print(as.data.frame(data))

        } 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
