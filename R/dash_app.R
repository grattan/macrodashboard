#' @import shiny
#' @import shinydashboard

sidebar <- function(...) {
  dashboardSidebar(
    sidebarMenu(
      menuItem("About",
               tabName = "about",
               icon = icon("bookmark")),
      menuItem("Unemployment",
               tabName = "unemployment",
               icon = icon("briefcase")),
      menuItem("Housing",
               tabName = "housing",
               icon = icon("home")),
      menuItem("Inflation",
               tabName = "inflation",
               icon = icon("dollar-sign"))
  )
)
}

tab_about <- function(...) {
  tabItem(tabName = "about",
          fluidRow(box(width = 12,
                       br(),
                       "intro goes here"))
          )
}

tab_unemployment <- function(...) {
  tabItem(tabName = "unemployment",
          fluidRow(box("Unemployment goes here"))
          )
}

tab_housing <- function(...) {
  tabItem(tabName = "housing",
          fluidRow(div(plotOutput("plot1", height = "500px"), style = "max-width: 765px")),
          fluidRow(
            box(title = "Download plot",
                selectInput("corelogic_type",
                            "Select plot type to download",
                            choices = grattantheme:::chart_types$type[!is.na(grattantheme:::chart_types$pptx_template)],
                            selected = "blog",
                            multiple = FALSE),
                radioButtons("corelogic_filetype",
                             "Select plot format to download",
                             choices = c("PNG", "PowerPoint"),
                             selected = "PNG"),
                downloadButton("corelogic_download", "Download plot")
                                ),
            width = 6)

  )
}

tab_inflation <- function(...) {
  tabItem(tabName = "inflation",
                         h2("Widgets tab content"))
}

body <- function(...) {
  dashboardBody(
    tabItems(
      tab_about(),
      tab_unemployment(),
      tab_housing(),
      tab_inflation()
    )
  )
}


dash_ui <-   function(...) {
  dashboardPage(
  header = dashboardHeader(title = "Macro dashboard"),
  sidebar = sidebar(),
  body = body()
  )
}

dl_button_server <- function(id, plot, type, filetype) {

  moduleServer(id, function(input, output, session) {

    downloadHandler(
      filename = function() {
        extension <- ifelse(filetype == "PNG", ".png", ".pptx")
        paste0(id, "_", type, extension)
      },

      content = function(file) {
        if (filetype == "PNG") {
          grattantheme::grattan_save(filename = file, object = plot, type = type)
        } else if (filetype == "PowerPoint") {
          grattantheme:::grattan_save_pptx(p = plot, filename = file, type = type)
        } else {
          stop("filetype must be one of: 'PNG' or 'PowerPoint'.")
        }

      }
    )
  })
}



dash_server <- function(input, output, session) {

  dash_data <- load_data(named_urls)

  raw_plot <- reactive({
    dash_data$corelogic %>%
      filter(date >= as.Date("2020-01-01")) %>%
      viz_corelogic_shutdown()
  })

  output$plot1 <- renderPlot(
    {
      raw_plot() %>%
        wrap_labs("normal")
    }
  )


  output$corelogic_download <- dl_button_server("corelogic",
                                           plot = raw_plot(),
                                           type = input$corelogic_type,
                                           filetype = input$corelogic_filetype)

}

dash_app <- function(...) {

  shinyApp(dash_ui(), dash_server)

}
