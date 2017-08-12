source("./dependencies.R")
source("./experiment_sentimental_analysis.R")

server <- function(input, output) {
   
   # Reactive expression to generate the requested distribution.
   # This is called whenever the inputs change. The output
   # functions defined below then all use the value computed from
   # this expression
   observeEvent(input$do, {
      cat('Thank you for clicking\n')
      withProgress(message = 'Badogando post...', value = 0, {
            # Increment the progress bar, and update the detail text.
            # Pause for 0.1 seconds to simulate a long computation.
      incProgress(1/2, detail = "Estamos badogando...")
      analiseDeMonitoramento(input$workdir, 
                             input$urlpost, 
                             input$fbid, 
                             input$date
      )
      incProgress(2/2, detail = "Badogada Perfeita! :)")
      
      Sys.sleep(3)
      })
   })

   data <- reactive({
      badogue <- switch(input$workdir, 
                        input$urlpost, 
                        input$fbid, 
                        input$date)
      
      analiseDeMonitoramento(input$workdir, 
              input$urlpost, 
              input$fbid, 
              input$date
           )
   })
   
   # Generate a plot of the data. Also uses the inputs to build
   # the plot label. Note that the dependencies on both the inputs
   # and the data reactive expression are both tracked, and
   # all expressions are called in the sequence implied by the
   # dependency graph

   
   output$plotReactions <- renderPlot({
      workdir <- input$workdir
      url <- input$urlpost
      id_pagina <- input$fbid 
      data <- input$date
      
      sufix <- format(Sys.time(),"%d%m%Y%H%M");
      # command file.path already controls for the OS
      load(file.path(workdir,"fb_oauth"))
      
      data_inicio <- ymd(as.character(data)) + days(-2);
      data_final <- ymd(as.character(data)) + days(2);
      
      mypage <- getPage(id_pagina, token = fb_oauth, feed=TRUE, since= as.character(data_inicio), until=as.character(data_final))
      id_post <- mypage$id[which(as.character(mypage$link)%in%url)]
      
      reactions_post <- getReactions(id_post, token=fb_oauth)
      allreactions <- reactions_post[,2:7]
      
      names(allreactions) <- c("Likes","Loves","Haha","Wow","Sad", "Angry")
      allreactions <- allreactions[order(as.numeric(allreactions))]
      p <- ggplot() + geom_bar(stat="identity", aes(x=names(allreactions), y = as.numeric(allreactions))) + xlab("Reações") + ylab("Número de Ocorrências") + coord_flip() 
      print(p)
#      plot(1:1000, log(1:1000),xlab="",ylab="")
   })
   
   output$plotNuvem <- renderPlot({
      workdir <- input$workdir
      url <- input$urlpost
      id_pagina <- input$fbid 
      data <- input$date
      
      sufix <- format(Sys.time(),"%d%m%Y%H%M");
      # command file.path already controls for the OS
      load(file.path(workdir,"fb_oauth"))
      
      data_inicio <- ymd(as.character(data)) + days(-2);
      data_final <- ymd(as.character(data)) + days(2);
      
      mypage <- getPage(id_pagina, token = fb_oauth, feed=TRUE, since= as.character(data_inicio), until=as.character(data_final))
      id_post <- mypage$id[which(as.character(mypage$link)%in%url)]
      
      post_dados <- getPost(id_post, token=fb_oauth, n= 10000)
      id_comments <- (post_dados$comments$from_id);
      allmessages <- post_dados$comments$message
      
      spSamp<- unlist(strsplit(allmessages, split=", "))
      nonAscIDX<- grep("spSamp", iconv(spSamp, "latin1", "ASCII", sub="spSamp"))
      ascVec<- spSamp[ - nonAscIDX]
      ascSamp<- paste(ascVec, collapse = ", ")
      clnSamp<- gsub('[[:digit:]]+', '', ascSamp)
      clnSamp<- gsub('[[:punct:]]+', '', clnSamp)
      clnSamp<- gsub("http[[:alnum:]]*", "", clnSamp)
      clnSamp<- gsub("([[:alpha:]])\1+", "", clnSamp)
      SampCrps<- Corpus(VectorSource(clnSamp))
      SampCrps<- tm_map(SampCrps, tolower)
      SampCrps<- tm_map(SampCrps, removePunctuation)
      SampCrps<- tm_map(SampCrps, removeNumbers)
      urlPat<-function(x) gsub("(ftp|http)(s?)://.*\\b", "", x)
      SampCrps<-tm_map(SampCrps, urlPat)
      emlPat<-function(x) gsub("\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b", "", x)
      SampCrps<- tm_map(SampCrps, emlPat)
      tt<-function(x) gsub("RT |via", "", x)
      SampCrps<- tm_map(SampCrps, tt)
      tun<-function(x) gsub("[@][a - zA - Z0 - 9_]{1,15}", "", x)
      SampCrps<- tm_map(SampCrps, tun)
      SampCrps<-tm_map(SampCrps, removeWords, stopwords("portuguese"))
      tdm <- TermDocumentMatrix(SampCrps)
      m <- as.matrix(tdm)
      v <- sort(rowSums(m),decreasing=TRUE)
      d <- data.frame(word = names(v),freq=v)
      pal <- brewer.pal(9, "BuGn")
      pal <- pal[-(1:2)]
      pal2 <- brewer.pal(8,"Dark2")
      wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.15, colors=pal2, vfont=c("sans serif","plain"))

   })
   
   output$plotLista <- renderPlot({
      workdir <- input$workdir
      url <- input$urlpost
      id_pagina <- input$fbid 
      data <- input$date
      
      sufix <- format(Sys.time(),"%d%m%Y%H%M");
      # command file.path already controls for the OS
      load(file.path(workdir,"fb_oauth"))
      
      data_inicio <- ymd(as.character(data)) + days(-2);
      data_final <- ymd(as.character(data)) + days(2);
      
      mypage <- getPage(id_pagina, token = fb_oauth, feed=TRUE, since= as.character(data_inicio), until=as.character(data_final))
      id_post <- mypage$id[which(as.character(mypage$link)%in%url)]
      
      post_dados <- getPost(id_post, token=fb_oauth, n= 10000)
      id_comments <- (post_dados$comments$from_id);
      allmessages <- post_dados$comments$message
      
      spSamp<- unlist(strsplit(allmessages, split=", "))
      nonAscIDX<- grep("spSamp", iconv(spSamp, "latin1", "ASCII", sub="spSamp"))
      ascVec<- spSamp[ - nonAscIDX]
      ascSamp<- paste(ascVec, collapse = ", ")
      clnSamp<- gsub('[[:digit:]]+', '', ascSamp)
      clnSamp<- gsub('[[:punct:]]+', '', clnSamp)
      clnSamp<- gsub("http[[:alnum:]]*", "", clnSamp)
      clnSamp<- gsub("([[:alpha:]])\1+", "", clnSamp)
      SampCrps<- Corpus(VectorSource(clnSamp))
      SampCrps<- tm_map(SampCrps, tolower)
      SampCrps<- tm_map(SampCrps, removePunctuation)
      SampCrps<- tm_map(SampCrps, removeNumbers)
      urlPat<-function(x) gsub("(ftp|http)(s?)://.*\\b", "", x)
      SampCrps<-tm_map(SampCrps, urlPat)
      emlPat<-function(x) gsub("\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b", "", x)
      SampCrps<- tm_map(SampCrps, emlPat)
      tt<-function(x) gsub("RT |via", "", x)
      SampCrps<- tm_map(SampCrps, tt)
      tun<-function(x) gsub("[@][a - zA - Z0 - 9_]{1,15}", "", x)
      SampCrps<- tm_map(SampCrps, tun)
      SampCrps<-tm_map(SampCrps, removeWords, stopwords("portuguese"))
      myCrps<- txt.to.words(SampCrps)
      tblUniGrm<-data.frame(table(make.ngrams(myCrps, ngram.size = 1)))
      stblUnigrm<-tblUniGrm[order(tblUniGrm$Freq, decreasing = TRUE),]
      top20unig<-stblUnigrm[1:20,]
      colnames(top20unig)<-c("UniGram","Frequency")
      p <- ggplot (top20unig, aes(x = reorder(UniGram, Frequency), y= Frequency )) + 
         geom_bar( stat = "Identity" , fill = "magenta" ) +  
         geom_text( aes (label = Frequency ) , vjust = - 0.10, hjust = -0.8, size = 2 ) +
         xlab( "Termos" ) +
         ylab( "Frequência" ) +
         theme ( axis.text.x = element_text ( angle = 45 , hjust = 1 ) ) + coord_flip()
      print(p)
   })
   
   
}

