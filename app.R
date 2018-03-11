
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
        fileInput(inputId = "file", label = "Raw input data", accept = c("text/csv", ".csv")),
        hr(),
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
                  value = "run: 1"),
        radioButtons(inputId = "phases", label = "type of exponential decay for the fit",
                     choices = list("1 phase" = 1, "2 phase" = 2, "3 phase" = 3))

    ),

    dashboardBody(

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
        ),
        # Plots
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
        req(inFile)

        df <- read_csv(inFile$datapath, skip = 30)
        df <- filter(df, !is.na(Time))
        df <- df %>% rename(fluorescence = Wavelength, time = Time)
        df
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
        df
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
            paste(input$file, "_processed", ".csv", sep = "")
        },
        content = function(con){
            write_csv(final(), con)
        }
    )


    # Output plots
    # raw
    output$raw_plot <- renderPlot({
        # s = input$raw_rows_selected
        # new <- rawdata()[s,]
        # p <- rawdata() %>% plt()
        # p + geom_point(data = new, color = "red")
        plot(data = rawdata(), fluorescence ~ time)
    })
    # processed
    output$processed_plot <- renderPlot({
        s = input$data_rows_selected
        new <- rawdata()[s,]
        p <- final() %>% plt()
        p + geom_point(data = new, color = "red")
    })

    # Fitted
    output$fitted <- renderPlot({})




}



# Run App ---------------------------------------------------------------------------------------------------------

shinyApp(ui, server)
