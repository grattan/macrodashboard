#' @import shiny
#' @import shinydashboard

download_data <- function() {
  df <- load_data()
  shinyjs::hide("loading_page")
  shinyjs::show("main_content")
  df
}


plot_server <- function(id, plot_function, data) {

  moduleServer(id,
               function(input, output, session) {

                 filtered_plot <- reactive(plot_function(data = data,
                                                         arg1 = input$arg1,
                                                         arg2 = input$arg2))

                 output$plot <- renderPlot({
                   wrap_labs(filtered_plot(), "normal")
                   })

                 output$download <- downloadHandler(
                   filename = function() {
                     ifelse(input$filetype == "PNG",
                            paste0(id, ".png"),
                            paste0(id, ".pptx"))

                   },
                   content = function(file) {

                     if (input$filetype == "PNG") {
                       obj <- filtered_plot() +
                         theme_grattan(base_family = "Arial")

                       grattantheme::grattan_save(filename = file,
                                                object = obj,
                                                type = input$type)
                     } else {
                       grattantheme::grattan_save_pptx(p = filtered_plot(),
                                                       filename = file,
                                                       type = input$type)

                     }
                   })

                 output$download_data <- downloadHandler(
                   filename = function() {
                     paste0(id, ".xlsx")
                   },
                   content = function(file) {
                       obj <- filtered_plot()

                       grattantheme::save_chartdata(filename = file,
                                                    object = obj)

                   })

               })
}


dash_server <- function(input, output, session) {

  dash_data <- download_data()

  purrr::map2(
    .x = c("unemp",
           "corelogic_shutdown_panel",
           "corelogic_shutdown_lines",
           "gross_flows"),
    .y = c(viz_unemp_rate,
           viz_corelogic_shutdown_panel,
           viz_corelogic_shutdown_lines,
           viz_lf_grossflows),
    .f = plot_server,
    data = dash_data
  )
}

dash_app <- function(...) {

  shinyApp(dash_ui(), dash_server)

}
