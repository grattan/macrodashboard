#' @import shiny
#' @import shinydashboard

sidebar <- function(...) {
  dashboardSidebar(
    sidebarMenu(
      menuItem("Housing",
                               tabName = "housing",
                               icon = icon("home")),
      menuItem("Inflation",
                               tabName = "inflation",
                               icon = icon("dollar-sign"))
  )
)
}


tab_housing <- function(...) {
  tabItem(tabName = "housing",
                          fluidRow(plotOutput("plot1")),
                          fluidRow(
                            box(title = "Download plot",
                                                downloadButton("plot_download", "Download plot"))
                          )
  )
}

tab_inflation <- function(...) {
  tabItem(tabName = "inflation",
                         h2("Widgets tab content"))
}

body <- function(...) {
  dashboardBody(
    tabItems(
      # First tab content
      tab_housing(),

      # Second tab content
      tab_inflation()

    )
  )
}


dash_ui <-   dashboardPage(
  header = dashboardHeader(title = "Macro dashboard"),
  sidebar = sidebar(),
  body = body()
)

dash_server <- function(input, output, session) {

  dash_data <- load_data(named_urls)

  output$plot1 <- renderPlot(
    {
      dash_data$corelogic %>%
        viz_corelogic_shutdown() %>%
        wrap_labs("blog") %>%
        create_fullslide("blog")
    }
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

dash_app <- function(...) {

  shinyApp(dash_ui, dash_server)

}
