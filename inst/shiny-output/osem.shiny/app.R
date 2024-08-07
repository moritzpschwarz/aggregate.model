library(shiny)

ui <- fluidPage(

  tabsetPanel(
    tabPanel("Input", fluid = TRUE,
             mainPanel(
               fileInput("upload", NULL, buttonLabel = "Upload Stored Model from Disk...", multiple = FALSE, accept = ".Rds"),
               tableOutput("files"),
               tags$hr(),
               h1("Specification"),
               DT::DTOutput("spec"),
               h1("Dependency"),
               plotOutput("network", height = "600", width = "800")#,
               #tableOutput(outputId = "test")
             )
    ),
    tabPanel("Graphs", fluid = TRUE,
             dateRangeInput(inputId = "range_plot", label = "Date Range for Plots",
                            start = as.Date("1960-01-01"), end = Sys.Date()),
             mainPanel(
               plotOutput("plots", height = "600", width = "800")
             )
    ),
    tabPanel("Equations", fluid = TRUE,
             mainPanel(
               verbatimTextOutput("equations")
             )
    ),
    tabPanel("Diagnostics", fluid = TRUE,
             mainPanel(
               DT::DTOutput("diag")
             )
    )
  )

)

server <- function(input, output) {

  # data <- reactive({
  #   req(input$upload)
  # })

  # input$upload <- observe({
  #   if (is.null(input$upload)){
  #     input$upload <- getShinyOption("object", model)
  #   }
  # })


  osem <- reactive({
    if (is.null(input$upload)) {
      getShinyOption("osem_direct")
    } else {
      readRDS(file = input$upload$datapath)
    }
  })

  wide <- reactive({
    osem()$full_data %>%
      tidyr::pivot_wider(names_from = na_item, values_from = values)
  })

  sel <- reactive({
    f <- colnames(wide())[grep(colnames(wide()), pattern = "\\.hat")]
    basef <- sub("\\.hat*", "", f)
    sub <- wide() %>%
      dplyr::select(time, union(basef, f)) %>%
      tidyr::pivot_longer(cols = !time, names_to = c("variable", "type"), names_sep = "\\.", values_to = "value") %>%
      dplyr::mutate(type = dplyr::case_when(is.na(type) ~ "observed",
                                            type == "hat" ~ "fitted"))
    return(sub)
  })

  eq <- reactive({
    modulesprint <- osem()$module_collection %>%
      filter(type == "n")
    wholeprint <- ""
    for (i in 1:NROW(modulesprint)) {
      wholeprint <- capture.output(print(modulesprint[[i, "model"]]), file = NULL)
    }
    return(wholeprint)
  })

  # get_file_or_default <- reactive({
  #   if (is.null(input$upload)) {
  #     getShinyOption("osem_direct ")
  #   } else {
  #     readRDS(file = input$upload$datapath)
  #   }
  # })
  #
  # output$test <- renderTable({
  #   #if(is.null(input$upload)){
  #    # tibble("test")
  #   #} else {
  #     get_file_or_default()$args$specification
  #     #object$args$specification
  #   #}
  # })

  output$test <- renderText(input$range_plot)

  output$files <- renderTable(input$upload)
  output$spec <- DT::renderDT(osem()$module_order)
  output$plots <- renderPlot({
    sel() %>%
      filter(time >= as.Date(input$range_plot[1]) & time <= as.Date(input$range_plot[2])) %>%
      ggplot2::ggplot(ggplot2::aes(x = time, y = value, color = type)) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(facets = "variable", scales = "free", nrow = length(unique(sel()$variable))) +
      ggplot2::theme_minimal(base_size = 20) +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
      ggplot2::labs(x = NULL, y = NULL)
  })
  output$equations <- renderPrint(eq(), width = 1000)
  output$diag <- DT::renderDT({

    diagnostics_model(osem()) %>%
      DT::datatable() %>%
      DT::formatStyle(columns = c("AR", "ARCH"),
                      backgroundColor = DT::styleInterval(cuts = c(0.01, 0.05), values = c("lightcoral", "lightsalmon", "lightgreen"))) %>%
      DT::formatRound(columns = c("AR", "ARCH", "indicator_share"),
                      digits = 4)

  })
  output$network <- renderPlot({
    req(osem())
    network(osem())
  })

}


shinyApp(ui = ui, server = server)


