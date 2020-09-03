#' @import shiny

dash_app <- function(...) {

  dash_data <- load_data(named_urls)
  corelogic_df <- dash_data$corelogic

  # Define UI for application that draws a histogram
  ui <- fluidPage(

    # Application title
    titlePanel("A graph"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        uiOutput("dates_slider")
      ),

      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("plot")
      )
    )
  )

  # Define server logic required to draw a histogram
  server <- function(input, output, session) {

    output$dates_slider <- renderUI({
      sliderInput("dates",
                  label = "Dates",
                  min = as.Date("1948-01-01"),
                  max = as.Date(Sys.Date()),
                  value = c(as.Date("1948-01-01"),
                            Sys.Date()))

    })

    # filtered_cpi <- reactive({
    #   cpi %>%
    #     filter(date >= input$dates[1] &
    #              date <= input$dates[2])
    # })

    output$plot <- renderPlot({
      # req(input$dates)

      viz_corelogic_shutdown(corelogic_df)

    })
  }

  # Run the application
  shinyApp(ui = ui, server = server, ...)



}
