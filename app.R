
# packages --------------------------------------------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

source("./fit_funs.R")


# ui --------------------------------------------------------------------------------------------------------------

ui <- dashboardPage(
    skin = "green",
    dashboardHeader(title = "Shiny Stopped Flow"),

    # Sidebar
    dashboardSidebar(
        "Jannik Buhr",
        hr(),

        # Inputs
        menuItem("Data", icon = icon("flask"), startExpanded = T,
                 fileInput(inputId = "file", label = "Raw input data",
                           accept = c(
                               "text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv"))
        ),

        menuItem("Options", icon = icon("cogs"), startExpanded = T,
                 numericInput(inputId = "filter", label = "Filter one value",
                              value = 8),
                 numericInput(inputId = "bin", label = "Bin Size",
                              value = 10, min = 1, max = 100, step = 1),

                 # Input for graphing and fitting
                 dateInput(inputId = "date", label = "date",
                           value = Sys.time()),
                 textInput(inputId = "experiment", label = "Name of your experiment",
                           value = "Stopped Flow: H450-1"),
                 textInput(inputId = "run", label = "Run or numbering",
                           value = "run: 1")
        )

    ),

    dashboardBody(

        tabsetPanel(

            tabPanel("Tables",
                     # Outputs
                     # Tables
                     fluidRow(
                         box(
                             title = "Raw Data",
                             dataTableOutput(outputId = "raw")
                         ),
                         box(
                             title = "Processed Data",
                             dataTableOutput(outputId = "data"),
                             p("The data has been grouped into chunks of the specified size (bin size)
                        and averaged over these groups"),
                             downloadButton(outputId = "download", label = "Download processed data")
                         )
                     )
            ),

            # Plots
            tabPanel("Plots",
                     fluidRow(
                         # Raw Data
                         box(
                             title = "Raw Data",
                             plotOutput(outputId = "raw_plot")
                         ),
                         box(
                             title = "Processed Data",
                             plotOutput(outputId = "processed_plot")
                         )
                     )
            ),
            tabPanel("Fits",
                     fluidRow(
                         h1("1 Phase exponential dacay"),
                         box(title = "Starting Settings",
                             numericInput("1Y0", "Y0", value = 9.48),
                             numericInput("1Plateau", "Plateau", value = 8.61),
                             numericInput("1KFast", "KFast", value = 40),
                             actionButton("1_go", "Go!")
                         ),
                         plotOutput(outputId = "1p_plot"),
                         plotOutput(outputId = "1p_res"),
                         box(title = "Fit",
                             textOutput(outputId = "1p_text"),
                             textOutput(outputId = "1p_gof")
                         )
                     ),
                     fluidRow(
                         h1("2 Phase exponential dacay"),
                         box(title = "Starting Settings",
                             numericInput("2Y0", "Y0", value = 9.48),
                             numericInput("2Plateau", "Plateau", value = 8.61),
                             numericInput("2KFast", "KFast", value = 40),
                             numericInput("2KSlow", "KSlow", value = 5),
                             numericInput("2PercentFast", "PercentFast", value = 90),
                             actionButton("2_go", "Go!")
                         ),
                         plotOutput(outputId = "2p_plot"),
                         plotOutput(outputId = "2p_res"),
                         box(
                             textOutput(outputId = "2p_text"),
                             textOutput(outputId = "2p_gof")
                         )
                     ),
                     fluidRow(
                         h1("3 Phase exponential dacay"),
                         box(title = "Starting Settings",
                             numericInput("3Y0", "Y0", value = 9.48),
                             numericInput("3Plateau", "Plateau", value = 8.61),
                             numericInput("3KFast", "KFast", value = 40),
                             numericInput("3Kmedium", "Kmedium", value = 10),
                             numericInput("3KSlow", "KSlow", value = 5),
                             numericInput("3PercentFast", "PercentFast", value = 90),
                             numericInput("3PercentSlow", "PercentSlow", value = 5),
                             actionButton("3_go", "Go!")
                         ),
                         plotOutput(outputId = "3p_plot"),
                         plotOutput(outputId = "1p_res"),
                         box(
                             textOutput(outputId = "3p_text"),
                             textOutput(outputId = "3p_gof")
                         )
                     )
            )
        )
    )
)








# server ----------------------------------------------------------------------------------------------------------
server <- function(input, output) {

    # Make infos available
    experiment <- reactive({
        input$experiment
    })

    run <- reactive({
        input$run
    })

    date <- reactive({
        input$date
    })

    # Function to plot the way we want
    plt <- function(df){
        ggplot(data = df) +
            aes(x = time, y = fluorescence) +
            geom_point() +
            theme_classic() +
            labs(
                title =  experiment(),
                subtitle = paste(run(), "\n"),
                x = "Time [s]",
                y = "Fluorescenc [ ]",
                caption = date()
            )
    }


    # Read Raw Data
    rawdata <- reactive({
        inFile <- input$file

        # Check if a file is selected
        if (is.null(inFile))
            return(NULL)

        df <- read_csv(inFile$datapath, skip = 30)
        df <- filter(df, !is.na(Time))
        df <- df %>% rename(fluorescence = Wavelength, time = Time)
        return(df)
    })

    # Clean Raw data
    final <- reactive({
        df <- rawdata()
        bin_size <- input$bin
        length_data <- dim(df)[1]

        # Add a variable with a group for "bin_size" measurements each
        validate(
            need(try(
                df <- df %>% mutate(
                    timestep = rep(seq(1 : (length_data / bin_size)), each = bin_size)
                )
            ), message = paste("Please choose a bin size that fits the lenght of your data, which is", length_data)
            )
        )
        # delete rows that are not unique (i.e. repeated measurement of the same value)
        df <- df %>%
            distinct(fluorescence, timestep, .keep_all = T)

        # There is always a measurement at 8.00, why? I filter it for now
        df <- df %>% filter(fluorescence > input$filter)

        # Summarize by the timestep grouping
        df <- df %>%
            group_by(timestep) %>%
            summarise(
                fluorescence = mean(fluorescence),
                time = mean(time)
            ) %>%
            filter(!is.na(fluorescence)) %>%
            select(time, fluorescence)
        return(df)
    })

    # Display Raw Data
    output$raw <- renderDataTable({
        rawdata()
    }, options = list(lengthMenu = c(5, 10, 30, 50), pageLength = 10, dom = "ltp")
    )

    # Display Processed Data
    output$data <- renderDataTable({
        final()
    }, options = list(lengthMenu = c(5, 10, 30, 50), pageLength = 10, dom = "ltp")
    )

    # Download Processed data
    output$download <- downloadHandler(
        filename = function() {
            paste(substr(input$file$name, 1, nchar(input$file)-4), "_processed", ".csv", sep = "")
        },
        content = function(con){
            write_csv(final(), con)
        }
    )


    # Output plots
    # raw
    output$raw_plot <- renderPlot({
        s = input$raw_rows_selected
        new <- rawdata()[s,]
        p <- rawdata() %>% plt()
        p + geom_point(data = new, color = "red")
    })

    # processed
    output$processed_plot <- renderPlot({
        s = input$data_rows_selected
        new <- final()[s,]
        p <- final() %>% plt()
        p + geom_point(data = new, color = "red")
    })


    # Fit and make plots
    # model1 <- eventReactive(input$`1_go`, {
    #     starting1 <- list(Y = input$Yo, PLateau = input$Plateau, KFast = input$KFast)
    #     model1 <- fit_decay(data(), 1)
    #     return(model1)
    # })
    #
    # data_model1 <- reactive({
    #     df <- extract_model(data(), model1())
    # })
    #
    #
    # td1 <- reactive({
    #     tidy(model1())
    # })
    #
    # values1 <- reactiveValues()
    #
    # values1$y_start <- td1()$estimate[which(td1()$term == "Y0")]
    # values1$y_end <- td1()$estimate[which(td1()$term == "Plateau")]
    # values1$k_fast <- td1()$estimate[which(td1()$term == "KFast")]
    # values1$k_med <- td1()$estimate[which(td1()$term == "Kmedium")]
    # values1$k_slow <- td1()$estimate[which(td1()$term == "KSlow")]
    # values1$p_slow <- td1()$estimate[which(td1()$term == "PercentSlow")]
    # values1$p_fast <- td1()$estimate[which(td1()$term == "PercentFast")]
    # values1$t_half_fast <- log(2) / values1$k_fast
    # values1$t_half_slow <- log(2) / values1$k_slow
    # values1$sigma <- glance(model1())$sigma
    #
    #

    #
    #     # Send to fitted plots output
    #     output$`1p_plot` <- renderPlot({
    #         p1()
    #     })
    #     output$`2p_plot` <- renderPlot({
    #         p2()
    #     })
    #     output$`2p_plot` <- renderPlot({
    #         p3()
    #     })


}



# Run App ---------------------------------------------------------------------------------------------------------

shinyApp(ui, server)
