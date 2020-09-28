#' @import shiny
#' @import shinydashboard

download_data <- function() {
  df <- load_data()
  shinyjs::hide("loading_page")
  shinyjs::show("main_content")
  df
}

graph_ui <- function(id,
                     input_fn1 = function(id){}) {
  div(style = "padding: 30px; background-color: rgba(217,217,217,0.5); border-style: solid; border-width: thin; border-color: #D9D9D9",
      # Inputs above plot
      fluidRow(column(width = 4,
                   input_fn1(id)
                   )
               ),
      fluidRow(plotOutput(NS(id, "plot"))),
      fluidRow(download_ui(id))
  )
}

download_ui <- function(id) {
  tagList(
      selectInput(NS(id, "type"),
                  "Select plot type to download",
                  choices = c("normal", "wholecolumn", "fullpage",
                              "fullslide", "fullslide_169",
                              "blog", "blog_half"),
                  selected = "blog",
                  multiple = FALSE),
      radioButtons(NS(id, "filetype"),
                   "Select plot format to download",
                   choices = c("PNG", "PowerPoint"),
                   selected = "PNG"),
      downloadButton(NS(id, "download"), "Download plot")
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
               icon = icon("home"))
  )
)
}

tab_about <- function(...) {
  tabItem(tabName = "about",
          fluidRow(shinyjs::useShinyjs(),
                   div(
                     id = "loading_page",
                     h2(" Loading data, please wait...")
                   ),
                   shinyjs::hidden(
                     div(
                       id = "main_content",
                       " Data loaded, click away"
                     )
                   )
                   )
          )
}

tab_unemployment <- function(...) {
  tabItem(tabName = "unemployment",
          graph_ui("unemp",
                   input_fn1 = function(id) {
                     sliderInput(NS(id, "arg1"),
                                 "Choose dates to show",
                                 min = as.Date("1901-06-30"),
                                 max = Sys.Date(),
                                 value = c(Sys.Date() - lubridate::years(40),
                                           Sys.Date()))
                   }))

}

tab_housing <- function(...) {
  tabItem(tabName = "housing",
          graph_ui("corelogic",
                   input_fn1 = function(id) {}))
}


body <- function(...) {
  dashboardBody(
    tabItems(
      tab_about(),
      tab_unemployment(),
      tab_housing()
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


plot_server <- function(id, plot_function, data) {

  moduleServer(id,
               function(input, output, session) {

                 filtered_plot <- reactive(plot_function(data = data,
                                                         arg1 = input$arg1))

                 output$plot <- renderPlot({
                   wrap_labs(filtered_plot(), "normal")
                   })

                 output$download = downloadHandler(
                   filename = function() {
                     ifelse(input$filetype == "PNG",
                            paste0(id, ".png"),
                            paste0(id, ".pptx"))

                   },
                   content = function(file) {

                     if (input$filetype == "PNG") {
                       grattantheme::grattan_save(filename = file,
                                                object = filtered_plot(),
                                                type = input$type)
                     } else {
                       grattantheme::grattan_save_pptx(p = filtered_plot(),
                                                       filename = file,
                                                       type = input$type)

                     }
                   })
               })
}


dash_server <- function(input, output, session) {

  dash_data <- download_data()

  purrr::map2(
    .x = c("unemp", "corelogic"),
    .y = c(viz_unemp_rate,
           viz_corelogic_shutdown),
    .f = plot_server,
    data = dash_data
  )
}

dash_app <- function(...) {

  shinyApp(dash_ui(), dash_server)

}
