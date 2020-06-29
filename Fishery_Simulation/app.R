library(shiny)

# Population model
model.pop <- function(N_t0, catch) {
    # Set carrying capacity
    K <- 10000
    # Set intrinsic growth rate
    r <- 0.75
    # Choose random natural mortality noise
    nat_mort <- runif(1, min = 0, max = 0.25)
    
    # Calcuate deaths as the starting population minus catch and natural mortality
    N_t0 <- N_t0 - catch - nat_mort * N_t0
    if (N_t0 > 0) {
        # If positive population, calculate growth according to logistic population model
        N_t1 <- K / (1 + ((K - N_t0) / N_t0) * exp(-r))
    } else{
        N_t1 <- 0
    }
    return(N_t1)
}

# Catch model
model.catch <- function(N, effort) {
    # Set catchability quotiant
    q <- 0.1
    # Calculate catch based on effort
    catch <- N * (1 - exp(-q * effort))
    # Modify catch with some noise
    catch <- catch + catch * runif(1, min = -0.1, max = 0.1)
    
    return(catch)
}

ui <- fluidPage(
    theme = "bootswatch_journal.css",
    titlePanel("Fishery simulation"),
                
                fluidRow(
                    # Side panel
                    column(
                        4,
                        h4("Set fishing effort"),
                        wellPanel(
                            sliderInput(
                                inputId = "effort",
                                label = "Fishing effort (hours)",
                                min = 0,
                                max = 24,
                                value = 0,
                                step = 1
                            )
                        ),
                        
                        # Add simulate button
                        wellPanel(
                            conditionalPanel(
                                condition = "output.collapsed == 'no'",
                                actionButton(
                                    inputId = "simulate",
                                    label = "Simulate next season",
                                    class = "btn btn-success action-button"
                                )
                            ),
                            
                            conditionalPanel(
                                condition = "output.collapsed == 'yes'",
                                actionButton(
                                    inputId = "simulate",
                                    label = "Simulate next season",
                                    class = "btn btn-success action-button disabled"
                                )
                                
                            ),
                            
                            tags$br(),
                            tags$button(
                                "Restart",
                                id = "restart",
                                type = "button",
                                class = "btn btn-danger action-button",
                                onclick = "history.go(0)"
                            )
                        )
                    ),
                    
                    # Right panel
                    column(
                        8,
                        fluidRow(column(12,
                                        wellPanel(
                                            textOutput("text_userfeedback")
                                        ))),
                        
                        fluidRow(column(4,
                                        h4("Season")),
                                 column(4,
                                        h4("Season's Catch")),
                                 column(4,
                                        h4("Lifetime Catch"))),
                        fluidRow(
                            column(4,
                                   wellPanel (
                                       div(textOutput("text_season"), style = "font-size:125%")
                                   )),
                            column(4,
                                   wellPanel (
                                       div(textOutput("text_catch"), style = "font-size:125%")
                                   )),
                            column(4,
                                   wellPanel (
                                       div(textOutput("text_total_catch"), style = "font-size:125%")
                                   ))
                        ),
                        fluidRow(column(12,
                                        
                                        tabsetPanel(
                                            tabPanel(
                                                "Data Table",
                                                h5("Summary catch data"),
                                                downloadButton('downloadData', 'Download Data', class =
                                                                   "btn-xs btn-info"),
                                                div(tableOutput("c_data_out"), style = "font-size:80%")
                                            ),
                                            
                                            tabPanel("Graphs",
                                                     fluidRow(
                                                         column(4,
                                                                plotOutput("plot_catch", height = 300)),
                                                         column(4,
                                                                plotOutput("plot_CPUE", height = 300)),
                                                         column(4,
                                                               plotOutput("plot_total_catch", height = 300))
                                                     ))
                                        )))
                    )
                ))


server <- function(input, output) {
    # Define reactive values
    values <- reactiveValues()
    values$id <- 1
    values$season <- 0
    values$N_t0 <- 9000
    values$catch <- 0
    values$total_catch <- 0
    values$CPUE <- 0
    values$collapsed <- "no"
    values$userfeedback <-
        "Select your fishing effort level and click the 'Simulate' button"
    
    # Define fishery dataframe
    values$fishery <- data.frame(
        id = as.integer(1),
        season = as.integer(0),
        N_t0 = 9000,
        catch = 0,
        CPUE = 0,
        total_catch = 0
    )
    
    # Simulate season
    observeEvent(input$simulate, {
        if (input$simulate > 0) {
            # Calculate catch
            values$catch <-
                model.catch(N = values$N_t0, effort = input$effort)
            
            # Calculate new population
            values$N_t0 <-
                model.pop(N_t0 = values$N_t0, catch = values$catch)
            
            # Calculate CPUE
            values$fishery$CPUE <- values$catch / input$effort
            
            # Check for stock collapse
            if (values$N_t0 < 1) {
                values$collapsed <- "yes"
            } else{
                values$collapsed <- "no"
            }
            
            # Set user feedback strings
            if (values$collapsed == "yes") {
                values$userfeedback <- "Oh no! You collapsed the fishery."
            } else {
                values$userfeedback <-
                    "Review your data and select your effort for the next fishing season"
            }
            
            # Increment id and season
            values$season <- values$season + 1
            values$id <- values$id + 1
            
            # Calculate cumulative catch
            values$total_catch <- sum(values$fishery$catch)
            
            # Update fishery dataframe
            newLine <- c(
                values$id,
                values$season,
                values$N_t0,
                values$catch,
                values$CPUE,
                values$total_catch
            )
            
            values$fishery[nrow(values$fishery) + 1,] <- newLine
            values$fishery$id <- as.integer(values$fishery$id)
            values$fishery$season <-
                as.integer(values$fishery$season)
        }
    })
    
    # Generate plots
    output$plot_catch <- renderPlot({
        ggplot(
            data = values$fishery,
            aes(
                x = values$fishery$season,
                y = values$fishery$catch,
                group = 1
            )
        ) +
            geom_line(color = "steelblue", size = 2) +
            xlab("Season number") +
            ylab("Catch (tonnes)") +
            theme_few()
    })
    
    
    output$plot_CPUE <- renderPlot({
        ggplot(
            data = values$fishery,
            aes(
                x = values$fishery$season,
                y = values$fishery$CPUE,
                group = 1
            )
        ) +
            geom_line(color = "steelblue", size = 2) +
            xlab("Season number") +
            ylab("Catch per unit effort (tonnes/hour)") +
            theme_few()
    })
    
    output$plot_total_catch <- renderPlot({
        ggplot(
            data = values$fishery,
            aes(
                x = values$fishery$season,
                y = values$fishery$total_catch,
                group = 1
            )
        ) +
            geom_line(color = "steelblue", size = 2) +
            xlab("Season number") +
            ylab("Total catch (tonnes)") +
            theme_few()
    })
    
    # Data table output and download function
    
    output$c_data_out  <-
        renderTable(values$fishery, striped = TRUE, spacing = "xs")
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste('dominobus', 'csv', sep = ".")
        },
        content = function(file) {
            # Write to a file specified by the 'file' argument
            write.csv(values$fishery, file,
                      row.names = FALSE)
        }
    )
    
    # Dashboard output
    output$text_season <- renderText(paste({
        values$fishery$season[values$id]
    }))
    output$text_catch <- renderText(paste({
        round(values$fishery$catch[values$id])
    }))
    output$text_total_catch <- renderText(paste({
        round(values$fishery$total_catch[values$id])
    }))
    output$text_userfeedback <- renderText(paste({
        values$userfeedback
    }))
    
    # Note this is used with conditional panel to deactivate simulation button once collapsed
    output$collapsed <- renderText(paste({
        values$collapsed
    }))
    
    # This line is needed as output$collapsed is not shown on the app, to prevent it being suspended
    outputOptions(output, "collapsed", suspendWhenHidden = FALSE)
}

# Run the application
shinyApp(ui = ui, server = server)
