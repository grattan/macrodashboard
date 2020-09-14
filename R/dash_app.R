#' @import shiny
#' @import shinydashboard
#'

graph_ui <- function(id) {
  div(style = "padding: 30px; background-color: rgba(217,217,217,0.1); border-style: solid; border-width: thin; border-color: #D9D9D9",
  fluidRow(div(width = 12,
               plotOutput(paste0(id, "_plot"),
                          height = "500px"),
               style = "max-width: 765px")
           ),
  br(),
  fluidRow(
    div(width = 4,
        selectInput(paste0(id, "_type"),
                    "Select plot type to download",
                    choices = grattantheme:::chart_types$type[!is.na(grattantheme:::chart_types$pptx_template)],
                    selected = "blog",
                    multiple = FALSE),
        radioButtons(paste0(id, "_filetype"),
                     "Select plot format to download",
                     choices = c("PNG", "PowerPoint"),
                     selected = "PNG"),
        downloadButton(paste0(id, "_download"), "Download plot")
    )
  )
)
}

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
          graph_ui("unemp"))

}

tab_housing <- function(...) {
  tabItem(tabName = "housing",
          graph_ui("corelogic")
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

dl_button_server <- function(id,
                             plot = sym(paste0(id, "_raw_plot()")),
                             type = input[[paste0(id, "_type")]],
                             filetype = input[[paste0(id, "_filetype")]]) {

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

  dash_data <- load_data()

  # Unemployment server logic ----
  unemp_raw_plot <- reactive({
    viz_unemp_rate(dash_data$lfs_m_1)
  })

  output$unemp_plot <- renderPlot({
    wrap_labs(unemp_raw_plot(), "normal")
  })

  output$unemp_download <- dl_button_server("unemp")

  # Corelogic shutdown server logic ----
  corelogic_raw_plot <- reactive({
    viz_corelogic_shutdown(dash_data$corelogic)
  })

  output$corelogic_plot <- renderPlot({
       wrap_labs(corelogic_raw_plot(), "normal")
    })

  output$corelogic_download <- dl_button_server("corelogic")


}

dash_app <- function(...) {

  shinyApp(dash_ui(), dash_server)

}
