library(stringr)
library(quanteda)
library(readtext)
library(ggplot2)
library(tidyr)
library(shinydashboard)
library(shiny)
library(shinyFiles)
library(devtools)
library(Rfacebook)
library(lubridate)
library(dplyr)
library(stylo)
library(tidytext)
library(tm)
library(wordcloud)
library(xlsx)
library(gdata)
library(readxl)
library(htmlwidgets)

workdir <- "/srv/shiny-server/cns/BadogueExcel"

getTidySentimentos <- function(file){
   polaridade <- toupper(file$Polaridade)
   text <- file$Conteúdo
   myCorpus <- corpus(text)
   metadoc(myCorpus, "language") <- "portuguese"
   tokenInfo <- summary(myCorpus)
   kwic(myCorpus, "gestor")
   myStemMat <- dfm(myCorpus, remove = stopwords("portuguese"), stem = TRUE, remove_punct = TRUE)
   byPolaridadeDfm <- dfm(myCorpus, groups = polaridade, remove = c(stopwords("portuguese"),"scontent.xx.fbcdn.net","https","oh","oe","pra"," v ","como","para","de","do","da","das","dos","isso","esse","nisso","nesse","aquele","nesses","aqueles","aquela","aquelas","que"), remove_punct = TRUE)
   ap_td <- tidy(byPolaridadeDfm)
   names(ap_td) <- c("sentimento","term","count")
   return(ap_td);
}

getTidyWords <- function(text){
   myCorpus <- corpus(text)
   metadoc(myCorpus, "language") <- "portuguese"
   tokenInfo <- summary(myCorpus)
   kwic(myCorpus, "gestor")
   myStemMat <- dfm(myCorpus, remove = stopwords("portuguese"), stem = TRUE, remove_punct = TRUE)
   mydfm <- dfm(myCorpus, remove = c(stopwords("portuguese"),"scontent.xx.fbcdn.net","https","oh","oe","pra"," v ","como","para","de","do","da","das","dos","isso","esse","nisso","nesse","aquele","nesses","aqueles","aquela","aquelas","que"), remove_punct = TRUE, remove_numbers= TRUE)
   ap_td <- tidy(mydfm)
   names(ap_td) <- c("sentimento","term","count")
   return(ap_td);
}

options(shiny.fullstacktrace = TRUE)

server <- function(input, output) {
  plotPalavras = function() {
      url <- input$urlpost
      id_pagina <- input$fbid 
      data <- input$date
      
      # command file.path already controls for the OS
      load(paste(workdir,"/fb_oauth",sep=""));
      
      data_inicio <- ymd(as.character(data)) + days(-2);
      data_final <- ymd(as.character(data)) + days(2);
      
      mypage <- getPage(id_pagina, token = fb_oauth, feed=TRUE, since= as.character(data_inicio), until=as.character(data_final))
      id_post <- mypage$id[which(as.character(mypage$link)%in%url)]
      
      post_dados <- getPost(id_post, token=fb_oauth, n= 10000)
      text <- post$comments$message
      words_td <- getTidyWords(text);
      p <- words_td %>%
            count(sentimento, term, wt = count) %>%
            ungroup() %>%
            filter(n >= 150) %>%
            mutate(term = reorder(term, n)) %>%
            ggplot(aes(term, n, fill = sentimento)) +
              geom_bar(stat="identity", fill="gray50") +
              theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
              ylab("Numero de ocorrencias") +
              xlab("Palavras") + coord_flip()
      print(p);
  }

  output$downloadPalavrasData <- downloadHandler(
    filename = function() {
      paste("palavras.png", sep = "")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plotPalavras2(), device = device)

    }
  )

  plotPalavras2 = function(){
      url <- input$urlpost
      id_pagina <- input$fbid 
      data <- input$date
      
      # command file.path already controls for the OS
      load(paste(workdir,"/fb_oauth",sep=""));
      
      data_inicio <- ymd(as.character(data)) + days(-2);
      data_final <- ymd(as.character(data)) + days(2);
      
      mypage <- getPage(id_pagina, token = fb_oauth, feed=TRUE, since= as.character(data_inicio), until=as.character(data_final))
      id_post <- mypage$id[which(as.character(mypage$link)%in%url)]
      
      post_dados <- getPost(id_post, token=fb_oauth, n= 10000)
      text <- post_dados$comments$message
      words_td <- getTidyWords(text);
     
      p <- words_td %>%
            count(sentimento, term, wt = count) %>%
            ungroup() %>%
            filter(n >= 150) %>%
            mutate(term = reorder(term, n)) %>%
            ggplot(aes(term, n, fill = sentimento)) +
              geom_bar(stat="identity", fill="gray50") +
              theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
              ylab("Numero de ocorrencias") +
              xlab("Palavras") + coord_flip()
      print(p);
#      plot(1:1000,log(1:1000));
  }
#####

  output$downloadExcelData <- downloadHandler(
    filename = function() {
      paste("comentarios.xlsx", sep = "")
    },
    content = function(file) {
      url <- input$urlpost
      id_pagina <- input$fbid 
      data <- input$date
      
      # command file.path already controls for the OS
      load(paste(workdir,"/fb_oauth",sep=""));
      
      data_inicio <- ymd(as.character(data)) + days(-2);
      data_final <- ymd(as.character(data)) + days(2);
      
      mypage <- getPage(id_pagina, token = fb_oauth, feed=TRUE, since= as.character(data_inicio), until=as.character(data_final))
      id_post <- mypage$id[which(as.character(mypage$link)%in%url)]
      
      post_dados <- getPost(id_post, token=fb_oauth, n= 10000)
      allmessages <- post_dados$comments %>% select(created_time,from_id,from_name,message);
      names(allmessages) <- c("Data","Autor ID","Autor Nome","Conteúdo");
      wb<-createWorkbook(type="xlsx")
      TABLE_COLNAMES_STYLE <- CellStyle(wb) + Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER")
      sheet <- createSheet(wb, sheetName = "facebookdata")
      setColumnWidth(sheet, colIndex=1:length(allmessages[1,]), 25)
      addDataFrame(allmessages, sheet, colnamesStyle = TABLE_COLNAMES_STYLE, colStyle = TABLE_COLNAMES_STYLE, startColumn=1, row.names = FALSE)
      saveWorkbook(wb, file)
    })
}
###### 
  # Reactive value for selected dataset ----
#  datasetInput <- reactive({
#    switch(input$dataset,
#           "rock" = rock,
#           "pressure" = pressure,
#           "cars" = cars
#    )
#  })

  # Table of selected dataset ----
#  output$table <- renderTable({
#    datasetInput()
#  })

