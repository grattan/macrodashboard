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
      menuItem("Labour force flows",
               tabName = "gross_flows",
               icon = icon("wind")),
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

tab_gross_flows<- function(...) {
  tabItem(tabName = "gross_flows",
          graph_ui("gross_flows"))

}

body <- function(...) {
  dashboardBody(
    tabItems(
      tab_about(),
      tab_unemployment(),
      tab_gross_flows(),
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
