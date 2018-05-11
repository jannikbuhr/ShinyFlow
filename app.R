# Handling packaged installing and dependencies

#list of packages required
list.of.packages <-
    c("shiny",
      "shinydashboard",
      "tidyverse",
      "DT",
      "broom",
      "modelr",
      "minpack.lm")

#checking missing packages from list
new.packages <-
    list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

#install missing ones
if (length(new.packages))
    install.packages(new.packages, dependencies = TRUE)


# Load packages --------------------------------------------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(broom)
library(modelr)
library(minpack.lm)

# Source functions for fitting
source("./fit_funs.R")

# ui --------------------------------------------------------------------------------------------------------------

ui <- dashboardPage(
    skin = "green",
    dashboardHeader(title = "Shiny Stopped Flow"),

    # Sidebar
    dashboardSidebar(
        h5(
            "Jannik Buhr",
            a(href = "mailto:jannik.buhr@stud.uni-heidelberg.de",
              icon("envelope", lib = "font-awesome"))
        ),
        hr(),

        # Inputs
        menuItem(
            "Data",
            icon = icon("flask"),
            startExpanded = T,
            numericInput(
                "delete",
                "Delete first n Rows (leave only your numbers):",
                min = 0,
                value = 31,
                step = 1
            ),
            fileInput(
                inputId = "file",
                label = "Raw input data",
                accept = c("text/csv", ".csv")
            )
        ),

        menuItem(
            "Options",
            icon = icon("cogs"),
            startExpanded = T,
            numericInput(
                inputId = "filter1",
                label = "From",
                min = 1,
                max = 10000,
                value = 1
            ),
            numericInput(
                inputId = "filter2",
                label = "To",
                min = 0,
                max = 10000,
                value = 10000
            ),
            numericInput(
                inputId = "bin",
                label = "Bin Size",
                value = 1,
                min = 1,
                max = 100,
                step = 1
            ),

            # Input for graphing and fitting
            dateInput(
                inputId = "date",
                label = "date",
                value = Sys.time()
            ),
            textInput(
                inputId = "experiment",
                label = "Name of your experiment",
                value = "Stopped Flow: H450-1"
            ),
            textInput(
                inputId = "run",
                label = "Run or numbering",
                value = "run: 1"
            ),
            selectInput(
                "dev",
                label = "Save plots as:",
                choices = list("pdf", "png", "eps", "svg"),
                selected = "pdf"
            )
        )

    ),

    dashboardBody(tabsetPanel(
        tabPanel("Tables",
                 # Outputs
                 # Tables
                 fluidRow(
                     box(title = "Raw Data",
                         dataTableOutput(outputId = "raw")),
                     box(
                         title = "Processed Data",
                         dataTableOutput(outputId = "data"),
                         p(
                             "The data has been grouped into chunks of the specified size (bin size)
                             and averaged over these groups"
                         ),
                         downloadButton(outputId = "download", label = "Download processed data")
                     )
                 )),

        # Plots
        tabPanel("Plots",
                 fluidRow(
                     # Raw Data
                     box(title = "Raw Data",
                         plotOutput(outputId = "raw_plot")),
                     box(title = "Processed Data",
                         plotOutput(outputId = "processed_plot"))
                 )),

        # Fits
        tabPanel("Fits",
                 tabsetPanel(
                     tabPanel(
                         title = "1 Phase exponential dacay",
                         inputPanel(
                             h4("Starting Values"),
                             actionButton("go1", "Go!"),
                             numericInput("Y01", "Y0", 0),
                             numericInput("Plateau1", "Plateau", 1),
                             numericInput("KFast1", "KFast", 40)
                         ),
                         fluidPage(
                             box(
                                 title = "Plot of Fit",
                                 plotOutput(outputId = "p1_plot"),
                                 downloadButton("downloadPlot1")
                             ),
                             box(title = "Residuals",
                                 plotOutput(outputId = "p1_res")),
                             box(
                                 title = "Goodness of Fit",
                                 width = 12,
                                 tableOutput(outputId = "p1_tidy"),
                                 tableOutput(outputId = "p1_glance"),
                                 textOutput(outputId = "p1_summary")
                             )
                         )
                     ),
                     tabPanel(
                         title = "2 Phase exponential dacay",
                         inputPanel(
                             h4("Starting Values"),
                             actionButton("go2", "Go!"),
                             numericInput("Y02", "Y0", 0),
                             numericInput("Plateau2", "Plateau", 1),
                             numericInput("KFast2", "KFast", 40),
                             numericInput("KSlow2", "KSlow", 5),
                             numericInput("PercentFast2", "PercentFast", 90)
                         ),
                         fluidPage(
                             box(
                                 title = "Plot of Fit",
                                 plotOutput(outputId = "p2_plot"),
                                 downloadButton("downloadPlot2")
                             ),
                             box(title = "Residuals",
                                 plotOutput(outputId = "p2_res")),
                             box(
                                 title = "Goodness of Fit",
                                 width = 12,
                                 tableOutput(outputId = "p2_tidy"),
                                 tableOutput(outputId = "p2_glance"),
                                 textOutput(outputId = "p2_summary")
                             )
                         )
                     ),
                     tabPanel(
                         title = "3 Phase exponential dacay",
                         inputPanel(
                             h4("Starting Values"),
                             actionButton("go3", "Go!"),
                             numericInput("Y03", "Y0", 0),
                             numericInput("Plateau3", "Plateau", 1),
                             numericInput("KFast3", "KFast", 40),
                             numericInput("Kmedium3", "Kmedium", 5),
                             numericInput("KSlow3", "KSlow", 1),
                             numericInput("PercentFast3", "PercentFast", 90),
                             numericInput("PercentSlow3", "PercentSlow", 5)

                         ),
                         fluidPage(
                             box(
                                 title = "Plot of Fit",
                                 plotOutput(outputId = "p3_plot"),
                                 downloadButton("downloadPlot3")
                             ),
                             box(title = "Residuals",
                                 plotOutput(outputId = "p3_res")),
                             box(
                                 title = "Goodness of Fit",
                                 width = 12,
                                 tableOutput(outputId = "p3_tidy"),
                                 tableOutput(outputId = "p3_glance"),
                                 textOutput(outputId = "p3_summary")
                             )
                         )
                     )
                 ))
    ))
)


# server ----------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
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
    plt <- function(df) {
        ggplot(data = df) +
            aes(x = time, y = fluorescence) +
            # geom_errorbar(aes(ymin = fluorescence - SEM, ymax = fluorescence + SEM),
            #               alpha = .8)+
            geom_point() +
            theme_classic() +
            labs(
                title =  experiment(),
                subtitle = paste(run(), "\n"),
                x = "Time [s]",
                y = "Fluorescenc [AU]",
                caption = date()
            )
    }

    # Read Raw Data
    delete <- reactive({
        input$delete
    })

    rawdata <- reactive({
        inFile <- input$file

        # Check if a file is selected
        req(inFile)

        df <-
            read_csv(
                inFile$datapath,
                skip = delete(),
                col_names = F,
                n_max = 10000
            ) %>%
            rename(time = X1) %>%
            filter(!is.na(time)) %>%
            gather(-time, key = "rep", value = "fluor") %>%
                group_by(rep) %>%
                mutate( fluor = fluor-min(fluor)) %>%
                group_by(time) %>% summarise(
                fluorescence = mean(fluor),
                SD = sd(fluor),
                SEM = sd(fluor) / sqrt(n())
            )
        df
    })

    # Clean Raw data
    final <- reactive({
        df <- rawdata()
        bin_size <- input$bin
        length_data <- dim(df)[1]

        # Add a variable with a group for "bin_size" measurements each
        validate(need(
            try(df <- df %>% mutate(timestep = rep(seq(
                1:(length_data / bin_size)
            ), each = bin_size)))
            , message = paste(
                "Please choose a bin size that fits the lenght of your data, which is",
                length_data
            )
        ))
        # delete rows that are not unique (i.e. repeated measurement of the same value)
        df <- df %>%
            distinct(fluorescence, timestep, .keep_all = T)

        # Filter the range
        df <- df %>% slice(input$filter1:input$filter2) %>% mutate(time = time - time[1])

        # Summarize by the timestep grouping
        df <- df %>%
            group_by(timestep) %>%
            summarise(
                fluorescence = mean(fluorescence),
                time = mean(time),
                SD = max(SD),
                # grouping adjacent values will give the maximum SD for the mean to be on the safe side
                SEM = max(SEM)
            ) %>%
            filter(!is.na(fluorescence)) %>%
            select(-timestep)
        df
    })

    # Display Raw Data
    output$raw <- renderDataTable({
        rawdata()
    }, options = list(
        lengthMenu = c(5, 10, 30, 50),
        pageLength = 10,
        dom = "ltp"
    ))

    # Display Processed Data
    output$data <- renderDataTable({
        final()
    }, options = list(
        lengthMenu = c(5, 10, 30, 50),
        pageLength = 10,
        dom = "ltp"
    ))

    # Download Processed data
    output$download <- downloadHandler(
        filename = function() {
            paste(substr(input$file, 1, nchar(input$file) - 4),
                  "_processed",
                  ".csv",
                  sep = "")
        },
        content = function(con) {
            write_csv(final(), con)
        }
    )


    # Output plots
    # raw
    output$raw_plot <- renderPlot({
        s = input$raw_rows_selected
        new <- rawdata()[s, ]
        p <- rawdata() %>% plt()
        p + geom_point(data = new, color = "red")
    })
    # processed
    output$processed_plot <- renderPlot({
        s = input$data_rows_selected
        new <- rawdata()[s, ]
        p <- final() %>% plt()
        p + geom_point(data = new, color = "red")
    })


    # # Test Output for debugging
    # output$test <- renderPrint({
    #     model1()
    # })



    # Fit and make plots

    ## 1 Phase
    model1 <- eventReactive(input$go1, {
        starting1 <-
            list(
                Y0 = input$Y01,
                Plateau = input$Plateau1,
                KFast = input$KFast1
            )
        model1 <-
            fit_decay(df = final(),
                      phases = 1,
                      starting = starting1)
        return(model1)
    })


    data_model1 <- reactive({
        df <- extract_model(final(), model1())
        df
    })


    td1 <- reactive({
        tidy(model1())
    })


    # Send to fitted plots output
    p1_plot <- reactive({
        p <- data_model1() %>% plt() +
            geom_line(
                aes(y = pred),
                color = "darkorange",
                size = 1,
                alpha = 0.9
            )
        return(p)
    })

    output$p1_plot <- renderPlot(p1_plot())


    ## 2 Phase
    model2 <- eventReactive(input$go2, {
        starting2 <-
            list(
                Y0 = input$Y02,
                Plateau = input$Plateau2,
                KFast = input$KFast2,
                KSlow = input$KSlow2,
                PercentFast = input$PercentFast2
            )
        model2 <- fit_decay(df = final(), 2, starting2)
        return(model2)
    })



    data_model2 <- reactive({
        df <- extract_model(final(), model2())
        df
    })


    td2 <- reactive({
        tidy(model2())
    })


    # Send to fitted plots output
    p2_plot <- reactive({
        p <- data_model2() %>% plt() +
            geom_line(
                aes(y = pred),
                color = "darkorange",
                size = 1.2,
                alpha = 0.9
            )
        return(p)
    })

    output$p2_plot <- renderPlot(p2_plot())


    ## 3 Phase
    model3 <- eventReactive(input$go3, {
        starting3 <-
            list(
                Y0 = input$Y03,
                Plateau = input$Plateau3,
                KFast = input$KFast3,
                Kmedium = input$Kmedium3,
                KSlow = input$KSlow3,
                PercentFast = input$PercentFast3,
                PercentSlow = input$PercentSlow3
            )
        model3 <- fit_decay(df = final(), 3, starting3)
        return(model3)
    })



    data_model3 <- reactive({
        df <- extract_model(final(), model3())
        df
    })


    td3 <- reactive({
        tidy(model3())
    })

    # Send to fitted plots output
    p3_plot <- reactive({
        p <- data_model3() %>% plt() +
            geom_line(
                aes(y = pred),
                color = "darkorange",
                size = 1.2,
                alpha = 0.9
            )
        return(p)
    })

    output$p3_plot <- renderPlot(p3_plot())

    # Residuals
    # 1 Phase
    output$p1_res <- renderPlot({
        data_model1() %>% ggplot() +
            aes(x = time, y = resid) +
            theme_classic() +
            geom_point()
    })

    # 2 Phase
    output$p2_res <- renderPlot({
        data_model2() %>% ggplot() +
            aes(x = time, y = resid) +
            theme_classic() +
            geom_point()
    })

    # 3 Phase
    output$p3_res <- renderPlot({
        data_model3() %>% ggplot() +
            aes(x = time, y = resid) +
            theme_classic() +
            geom_point()
    })

    # Model summary outputs
    # 1 Phase
    output$p1_tidy <- renderTable({
        tidy(model1()) %>% left_join(confint(model1()) %>% as_tibble(rownames = "term"))
    })

    output$p1_glance <- renderTable({
        glance(model1())
    })

    output$p1_summary <- renderPrint({
        summary(model1()) %>% print()
    })

    # 2 Phase
    output$p2_tidy <- renderTable({
        tidy(model2()) %>% left_join(confint(model2()) %>% as_tibble(rownames = "term"))
    })

    output$p2_glance <- renderTable({
        glance(model2())
    })

    output$p2_summary <- renderPrint({
        summary(model2())
    })

    # 3 Phase
    output$p3_tidy <- renderTable({
        tidy(model3()) %>% left_join(confint(model2()) %>% as_tibble(rownames = "term"))
    })

    output$p3_glance <- renderTable({
        glance(model3())
    })

    output$p3_summary <- renderPrint({
        summary(model3())
    })



    # Download Buttons ------------------------------------------------------------------------------------------------
    graphic_device <- reactive(input$dev)

    output$downloadPlot1 <- downloadHandler(
        filename = function() {
            paste0(substr(input$file, 1, nchar(input$file) - 4),
                   "_fit",
                   ".",
                   as.character(graphic_device()))
        },
        content = function(file) {
            ggsave(file,
                   plot = p1_plot(),
                   device = graphic_device(),
                   dpi = 300)
        }
    )

    output$downloadPlot2 <- downloadHandler(
        filename = function() {
            paste0(substr(input$file, 1, nchar(input$file) - 4),
                   "_fit",
                   ".",
                   as.character(graphic_device()))
        },
        content = function(file) {
            ggsave(file,
                   plot = p2_plot(),
                   device = graphic_device(),
                   dpi = 300)
        }
    )

    output$downloadPlot3 <- downloadHandler(
        filename = function() {
            paste0(substr(input$file, 1, nchar(input$file) - 4),
                   "_fit",
                   ".",
                   as.character(graphic_device()))
        },
        content = function(file) {
            ggsave(file,
                   plot = p3_plot(),
                   device = graphic_device(),
                   dpi = 300)
        }
    )

    # End session on closing of browser window
    session$onSessionEnded(stopApp)
}



# Run App ---------------------------------------------------------------------------------------------------------

options(shiny.reactlog = TRUE)

shinyApp(ui, server)
