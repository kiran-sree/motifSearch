library(shiny)

ui <- fluidPage(
  tags$style("h1 {text-align: center;}"),
  tags$h1("MOTIF SEARCH"),
  tags$style(".name {position: absolute; top: 10px; right: 10px;}"),
  tags$div(class = "name", "By: Kiran Manoharan & Lakshmi A R"),
  fileInput(inputId = "file1", label = "Choose a file", accept = ".fasta"),
  style = "background-color: lightblue;",
  tags$textarea(id = "seq_input", label = "Sequence", rows = 10, style = "width:100%"),
  textInput(inputId = "motif_len", label = "Enter the length of the motif"),
  actionButton(inputId = "submit", label = "Submit"),
  downloadButton("download_data", "Download Motif File"),
  textInput(inputId = "search_term", label = "Search"),
  tableOutput(outputId = "output")
)

server <- function(input, output, session) {
  observeEvent(input$file1, {
    file_content <- readLines(input$file1$datapath)
    
    if (substr(file_content[1], 1, 1) == ">") {
      file_content <- file_content[-1]
    }
    dna_seq <- paste0(file_content, collapse = "")
    updateTextInput(session, "seq_input", value = dna_seq)
  })

  data <- reactive({
    if (!is.null(input$seq_input) && !is.null(input$motif_len) && input$motif_len != "") {
      dna_seq <- input$seq_input
      motif_len <- as.numeric(input$motif_len)
      
      motifs <- list()
      
      for (i in 1:(nchar(dna_seq) - motif_len + 1)) {
        motif <- substr(dna_seq, i, i + motif_len - 1)
        motifs[[i]] <- motif
      }
      
      motif_counts <- data.frame(table(unlist(motifs)))
      colnames(motif_counts) <- c("Motif", "Count")
      return(motif_counts)
    }
    
    return(NULL)
  })

  output$output <- renderTable({
    data_filtered <- data()
    if (!is.null(data_filtered) && !is.null(input$search_term) && input$search_term != "") {
      data_filtered <- data_filtered[grep(input$search_term, data_filtered$Motif, ignore.case = TRUE), ]
    }
    data_filtered
  })

  output$download_data <- downloadHandler(
    filename = function() { "motif_counts.txt" },
    content = function(file) {
      write.table(data(), file, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  )
}

shinyApp(ui, server)
