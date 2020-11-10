graph_ui <- function(id,
                     title = "",
                     input_fn1 = function(id) {},
                     input_fn2 = function(id) {}) {
  div(
    style = "padding: 30px; background-color: rgba(217,217,217,0.5); border-style: solid; border-width: thin; border-color: #D9D9D9",
    # Inputs above plot
    fluidRow(
      column(
        width = 12,
        h3(strong(title))
      ),
      column(
        width = 4,
        input_fn1(id)
      ),
      column(
        width = 4,
        input_fn2(id)
      )
    ),
    fluidRow(div(
      plotOutput(NS(id, "plot"),
        height = "500px"
      ),
      style = "max-width: 765px"
    )),
    fluidRow(column(download_graph_ui(id),
      width = 9
    ))
  )
}

download_graph_ui <- function(id) {
  fluidRow(
    column(
      selectInput(NS(id, "type"),
        "Select plot type to download",
        choices = c(
          "normal", "wholecolumn", "fullpage",
          "fullslide", "fullslide_169",
          "blog", "blog_half"
        ),
        selected = "blog",
        multiple = FALSE
      ),
      width = 6
    ),
    column(
      radioButtons(NS(id, "filetype"),
        "Select plot format to download",
        choices = c("PNG", "PowerPoint"),
        selected = "PNG"
      ),
      width = 6
    ),
    column(
      downloadButton(NS(id, "download"), "Download plot"),
      width = 6
    ),
    column(
      downloadButton(NS(id, "download_data"), "Download chart data"),
      width = 6
    )
  )
}


sidebar <- function(...) {
  dashboardSidebar(
    sidebarMenu(
      # style = "position: fixed; overflow: visible;",
      menuItem("About",
        tabName = "about",
        icon = icon("bookmark")
      ),
      menuItem("Housing",
        tabName = "housing",
        icon = icon("home")
      ),
      menuItem("Unemployment",
        tabName = "unemployment",
        icon = icon("briefcase")
      ),
      menuItem("Employment + hours",
        tabName = "emp_hours",
        icon = icon("chart-area")
      ),
      menuItem("Labour force flows",
        tabName = "gross_flows",
        icon = icon("wind")
      )
    )
  )
}

tab_about <- function(...) {
  tabItem(
    tabName = "about",
    fluidRow(
      shinyjs::useShinyjs(),
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
  tabItem(
    tabName = "unemployment",
    graph_ui("unemp",
      title = "Unemployment rate",
      input_fn1 = function(id) {
        sliderInput(NS(id, "arg1"),
          "Choose dates to show",
          min = as.Date("1901-06-30"),
          max = Sys.Date(),
          value = c(
            Sys.Date() - lubridate::years(40),
            Sys.Date()
          )
        )
      }
    ),
    graph_ui("unemp_age_dots",
      title = "Unemployment and underemployment by age",
      input_fn1 = function(id) {
        sliderInput(NS(id, "arg1"),
          "Choose initial date to show",
          min = as.Date("2020-01-01"),
          max = Sys.Date(),
          value = as.Date("2020-03-01"),
          timeFormat = "%b %Y"
        )
      }
    )
  )
}

tab_emphours <- function(...) {
  tabItem(
    tabName = "emp_hours",
    graph_ui("hours_pop",
      title = "Hours per head of population (civilians aged 15+)"
    ),
    graph_ui("viz_emphours_sex_recessions",
      title = "Recessions compared by sex",
      input_fn1 = function(id) {
        selectInput(NS(id, "arg1"),
          "Select measure of interest",
          choices = c(
            "Employment" = "employment",
            "Total hours worked" = "hours",
            "Hours per head of population" = "hours_pop"
          ),
          selected = "employment"
        )
      },
      input_fn2 = function(id) {
        selectInput(NS(id, "arg2"),
          "Facet on recession or sex?",
          choices = c(
            "Recession" = "recession",
            "Sex" = "sex"
          ),
          selected = "recession"
        )
      }
    ),
    graph_ui("emp_agesex_recessions",
      title = "Recessions compared by age"
    ),
    graph_ui("payrolls_byind_bystate",
      title = "Payroll jobs by state, by industry",
      input_fn1 = function(id) {
        selectInput(NS(id, "arg1"),
                    "Select industry to highlight",
                    choices = c(
                      "Agriculture, forestry & fishing",
                      "Mining",
                      "Manufacturing",
                      "Electricity, gas, water & waste services",
                      "Construction",
                      "Wholesale trade",
                      "Retail trade",
                      "Accommodation & food services",
                      "Transport, postal & warehousing",
                      "Information media & telecommunications",
                      "Financial & insurance services",
                      "Rental, hiring & real estate services",
                      "Professional, scientific & technical services",
                      "Administrative & support services",
                      "Public administration & safety",
                      "Education & training",
                      "Health care & social assistance",
                      "Arts & recreation services",
                      "Other services"
                    ),
                    selected = "Construction"
        )
      }
    ),
    graph_ui("payrolls_byind_bar",
      title = "Change in payroll jobs by industry",
      input_fn1 = function(id) {
        selectInput(NS(id, "arg1"),
          "Select industry to highlight",
          choices = c(
            "Agriculture, forestry & fishing",
            "Mining",
            "Manufacturing",
            "Electricity, gas, water & waste services",
            "Construction",
            "Wholesale trade",
            "Retail trade",
            "Accommodation & food services",
            "Transport, postal & warehousing",
            "Information media & telecommunications",
            "Financial & insurance services",
            "Rental, hiring & real estate services",
            "Professional, scientific & technical services",
            "Administrative & support services",
            "Public administration & safety",
            "Education & training",
            "Health care & social assistance",
            "Arts & recreation services",
            "Other services"
          ),
          selected = "Construction"
        )
      }
    )
  )
}

tab_housing <- function(...) {
  tabItem(
    tabName = "housing",
    graph_ui("corelogic_shutdown_panel"),
    br(),
    graph_ui("corelogic_shutdown_lines",
      input_fn1 = function(id) {
        sliderInput(NS(id, "arg1"),
          "Choose minimum date to show",
          min = as.Date("2018-11-02"),
          max = Sys.Date() - 1,
          value = as.Date("2020-01-01")
        )
      },
      input_fn2 = function(id) {
        sliderInput(NS(id, "arg2"),
          "Choose date to base index",
          min = as.Date("2018-11-02"),
          max = Sys.Date(),
          value = as.Date("2020-03-22")
        )
      }
    )
  )
}

tab_gross_flows <- function(...) {
  tabItem(
    tabName = "gross_flows",
    graph_ui("gross_flows",
      input_fn1 = function(id) {
        sliderInput(NS(id, "arg1"),
          "Choose minimum date to show",
          min = as.Date("2000-09-01"),
          max = Sys.Date() - 90,
          value = as.Date("2018-01-01")
        )
      }
    )
  )
}

body <- function(...) {
  dashboardBody(
    tags$script(HTML("$('body').addClass('fixed');")),
    tabItems(
      tab_about(),
      tab_unemployment(),
      tab_emphours(),
      tab_gross_flows(),
      tab_housing()
    )
  )
}


dash_ui <- function(...) {
  dashboardPage(
    skin = "yellow",
    header = dashboardHeader(title = "Macro dashboard"),
    sidebar = sidebar(),
    body = body()
  )
}
