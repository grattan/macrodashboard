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
    .x = c("unemp",
           "corelogic",
           "gross_flows"),
    .y = c(viz_unemp_rate,
           viz_corelogic_shutdown,
           viz_lf_grossflows),
    .f = plot_server,
    data = dash_data
  )
}

dash_app <- function(...) {

  shinyApp(dash_ui(), dash_server)

}
