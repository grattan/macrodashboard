#' @import shiny
#' @import shinydashboard

download_data <- function() {
  df <- load_data()
  shinyjs::hide("loading_page")
  shinyjs::show("main_content")
  df
}


plot_server <- function(id, plot_function, data) {
  moduleServer(
    id,
    function(input, output, session) {
      filtered_plot <- reactive(plot_function(
        data = data,
        input$arg1,
        input$arg2
      ))

      output$plot <- renderPlot({
        wrap_labs(filtered_plot(), "normal")
      })

      output$download <- downloadHandler(
        filename = function() {
          ifelse(input$filetype == "PNG",
            paste0(id, ".png"),
            paste0(id, ".pptx")
          )
        },
        content = function(file) {
          obj <- filtered_plot()

          if (input$filetype == "PNG") {
            grattantheme::grattan_save(
              filename = file,
              object = obj,
              type = input$type
            )
          } else {
            grattantheme::grattan_save_pptx(
              p = obj + theme(text = element_text(family = "Arial")),
              filename = file,
              type = input$type
            )
          }
        }
      )

      output$download_data <- downloadHandler(
        filename = function() {
          paste0(id, ".xlsx")
        },
        content = function(file) {
          obj <- filtered_plot()

          grattantheme::save_chartdata(
            filename = file,
            object = obj
          )
        }
      )
    }
  )
}


dash_server <- function(input, output, session) {
  dash_data <- download_data()

  purrr::map2(
    .x = c(
      "unemp",
      "unemp_age_dots",
      "hours_pop",
      "viz_emphours_sex_recessions",
      "emp_agesex_recessions",
      "payrolls_bystate",
      "payrolls_byind_bystate",
      "payrolls_byind_bar",
      "corelogic_shutdown_panel",
      "corelogic_shutdown_lines",
      "gross_flows",
      "oecd_minwage",
      "forecasts_cpi_wpi",
      "wpi_bysector",
      "wpi_byind",
      "apple_mobility"
    ),
    .y = c(
      viz_unemp_rate,
      viz_unemp_age_dots,
      viz_hours_pop,
      viz_emphours_sex_recessions,
      viz_emp_agesex_recessions,
      viz_payrolls_bystate,
      viz_payrolls_byind_bystate,
      viz_payrolls_byind_bar,
      viz_corelogic_shutdown_panel,
      viz_corelogic_shutdown_lines,
      viz_lf_grossflows,
      viz_oecd_minwage,
      viz_forecasts_cpi_wpi,
      viz_wpi_bysector,
      viz_wpi_byind,
      viz_apple_mobility
    ),
    .f = plot_server,
    data = dash_data
  )
}

dash_app <- function(...) {
  shinyApp(dash_ui(), dash_server)
}
