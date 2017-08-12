source("./dependencies.R")
source("./experiment_sentimental_analysis.R")


fluidPage(
   titlePanel("Badogue Contingencial"),
   
   # Sidebar with controls to select the random distribution type
   # and number of observations to generate. Note the use of the
   # br() element to introduce extra vertical spacing
   sidebarLayout(
      sidebarPanel(
         textInput("workdir", 
                   "Escreva o endereço do Diretório onde guardar as saídas:", 
                   value = "C:/..."
         ),
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
         actionButton("do", "Baixar")
      ),
      
      # Show a tabset that includes a plot, summary, and table view
      # of the generated distribution
      mainPanel(
         tabsetPanel(type = "tabs", 
                     tabPanel("Lista de Palavras", plotOutput("plotLista")), 
                     tabPanel("Nuvem de Palavras", plotOutput("plotNuvem")),
                     tabPanel("Reações", plotOutput("plotReactions"))
         )
      )
   )
)



