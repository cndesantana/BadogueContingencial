library(shiny)

ui <- fluidPage(

  # App title ----
  titlePanel("Badogue's FB data download"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

         textInput("urlpost", 
                   "Escreva a URL do post que deseja analisar:", 
                   value = "http://..."
         ),
         textInput("fbid", 
                   "Escreva o ID da página do Facebook onde foi publicado esse post:", 
                   value = "acesse http://findmyfbid.com/ para obter o ID..."
         ),
         dateInput('date',
                   label = 'Selecione a data para a qual deseja analisar o post:',
                   value = Sys.Date()
         ),

      # Input: Choose dataset ----
#      selectInput("dataset", "Choose a dataset:",
#                  choices = c("rock", "pressure", "cars")),

      # Button
      downloadButton("downloadExcelData", "Download Excel"),
      downloadButton("downloadPalavrasData", "Download Lista de Palavras")

    ),

    # Main panel for displaying outputs ----
    mainPanel(

#      tableOutput("table")

    )

  )
)
