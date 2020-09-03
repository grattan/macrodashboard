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
        uiOutput("dates_slider"),
        downloadButton("plot_download", "Download plot")
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

    raw_plot <- reactive({
      viz_corelogic_shutdown(corelogic_df)
    })

    output$plot <- renderPlot(
      {
       raw_plot() %>%
        wrap_labs("blog") %>%
        create_fullslide("blog")
        },
    width = 37.7952755906 * grattantheme:::chart_types$width[grattantheme:::chart_types$type == "blog"],
    height = 37.7952755906 * grattantheme:::chart_types$height[grattantheme:::chart_types$type == "blog"]
    )

    output$plot_download <- downloadHandler(
      filename = function() {
        "corelogic.png"
      },

      content = function(file) {
        grattantheme::grattan_save(filename = file, object = raw_plot(), type = "blog")
      }

    )
  }

  # Run the application
  shinyApp(ui = ui, server = server, ...)

}
