get_os <- function(){
   sysinf <- Sys.info()
   if (!is.null(sysinf)){
      os <- sysinf['sysname']
      if (os == 'Darwin')
         os <- "osx"
   } else { ## mystery machine
      os <- .Platform$OS.type
      if (grepl("^darwin", R.version$os))
         os <- "osx"
      if (grepl("linux-gnu", R.version$os))
         os <- "linux"
   }
   tolower(os)
}

##### Aqui a função

analiseDeMonitoramento <- function(workdir, url,id_pagina,data){

   sufix <- format(Sys.time(),"%d%m%Y%H%M");
   myOS <- get_os();
   # command file.path already controls for the OS
   resultsdir <- file.path(workdir,paste("results_",sufix,sep=""))
   reactionsfilename <- file.path(resultsdir,"reactions.png")
   palavrasfilename <- file.path(resultsdir,"palavras.png")
   wordcloudfilename <- file.path(resultsdir,"wordcloud.png")
   excelfilename <- file.path(resultsdir,"comentarios.xlsx")
   load(file.path(workdir,"fb_oauth"))
      
   dir.create(resultsdir)
   
   data_inicio <- ymd(as.character(data)) + days(-2);
   data_final <- ymd(as.character(data)) + days(2);
   
   mypage <- getPage(id_pagina, token = fb_oauth, feed=TRUE, since= as.character(data_inicio), until=as.character(data_final))
   id_post <- mypage$id[which(as.character(mypage$link)%in%url)]

   reactions_post <- getReactions(id_post, token=fb_oauth)
   allreactions <- reactions_post[,2:7]

   cat("Plotting the Reactions...\n")
   
   names(allreactions) <- c("Likes","Loves","Haha","Wow","Sad", "Angry")
   allreactions <- allreactions[order(as.numeric(allreactions))]
   p <- ggplot() + geom_bar(stat="identity", aes(x=names(allreactions), y = as.numeric(allreactions))) + xlab("Reações") + ylab("Número de Ocorrências") + coord_flip() 
   png(reactionsfilename,width=1980,height=1280,res=300)
   print(p)
   dev.off()

   post_dados <- getPost(id_post, token=fb_oauth, n= 10000)
   id_comments <- (post_dados$comments$from_id);
   allmessages <- post_dados$comments$message
   cat("Creating Excel file...\n")   
   write.xlsx(allmessages,file=excelfilename)
 
   # convert string to vector of words
   spSamp<- unlist(strsplit(allmessages, split=", "))
   # find indices of words with non-ASCII characters
   nonAscIDX<- grep("spSamp", iconv(spSamp, "latin1", "ASCII", sub="spSamp"))
   # subset original vector of words to exclude words with non-ACCII characters
   ascVec<- spSamp[ - nonAscIDX]
   # convert vector back to string
   
   #   ascVec %>% unnest_tokens(digram, ascVec, token = "ngrams", n = 2)
   ascSamp<- paste(ascVec, collapse = ", ")
   #remove numbers and punctuation
   clnSamp<- gsub('[[:digit:]]+', '', ascSamp)
   clnSamp<- gsub('[[:punct:]]+', '', clnSamp)
   clnSamp<- gsub("http[[:alnum:]]*", "", clnSamp)
   clnSamp<- gsub("([[:alpha:]])\1+", "", clnSamp)
   SampCrps<- Corpus(VectorSource(clnSamp))
   ##Convert characters to lower case
   SampCrps<- tm_map(SampCrps, tolower)
   ##Remove punctuation
   SampCrps<- tm_map(SampCrps, removePunctuation)
   ##Remove numbers
   SampCrps<- tm_map(SampCrps, removeNumbers)
   # Create patterns to elimina special code and other patterns
   # URLs
   urlPat<-function(x) gsub("(ftp|http)(s?)://.*\\b", "", x)
   SampCrps<-tm_map(SampCrps, urlPat)
   # Emails
   emlPat<-function(x) gsub("\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b", "", x)
   SampCrps<- tm_map(SampCrps, emlPat)
   # Twitter tags
   tt<-function(x) gsub("RT |via", "", x)
   SampCrps<- tm_map(SampCrps, tt)
   # Twitter Usernames
   tun<-function(x) gsub("[@][a - zA - Z0 - 9_]{1,15}", "", x)
   SampCrps<- tm_map(SampCrps, tun)
   #White Space
   #SampCrps<- tm_map(SampCrps, stripWhitespace)
   #Remove profane words
   #First get the list of bad words
   #bwdat<-read.table("en_bws.txt", header=FALSE, sep="\n", strip.white=TRUE)
   #names(bwdat)<-"Bad Words"
   #SampCrps<- tm_map(SampCrps, removeWords, bwdat[,1])
   SampCrps<-tm_map(SampCrps, removeWords, stopwords("portuguese"))
   # Create nGrams
   myCrps<- txt.to.words(SampCrps)
   #create data frames of one, two and three ngrams, 
   tblUniGrm<-data.frame(table(make.ngrams(myCrps, ngram.size = 1)))
   #Create a sorted table "stbl*" by decending frequency count
   stblUnigrm<-tblUniGrm[order(tblUniGrm$Freq, decreasing = TRUE),]
   top20unig<-stblUnigrm[1:20,]
   colnames(top20unig)<-c("UniGram","Frequency")
   #top20unig <- top20unig[order(top20unig$Frequency, decreasing=TRUE), ]  # sort
   p <- ggplot (top20unig, aes(x = reorder(UniGram, Frequency), y= Frequency )) + 
      geom_bar( stat = "Identity" , fill = "magenta" ) +  
      geom_text( aes (label = Frequency ) , vjust = - 0.10, hjust = -0.8, size = 2 ) +
      xlab( "Termos" ) +
      ylab( "Frequência" ) +
      theme ( axis.text.x = element_text ( angle = 45 , hjust = 1 ) ) + coord_flip()
   cat("Plotting the Words...\n")
   png(palavrasfilename,width=1980,height=1280,res=300)
   print(p)
   dev.off()
   
   tdm <- TermDocumentMatrix(SampCrps)
   m <- as.matrix(tdm)
   v <- sort(rowSums(m),decreasing=TRUE)
   d <- data.frame(word = names(v),freq=v)
   pal <- brewer.pal(9, "BuGn")
   pal <- pal[-(1:2)]
   pal2 <- brewer.pal(8,"Dark2")
   p <- wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.15, colors=pal2, vfont=c("sans serif","plain"))
   png(wordcloudfilename,width=1980,height=1280,res=300)
   print(p)
   dev.off()
   
#   mycorpus <- plotPalavras(allmessages);
#   analiseDeSentimento <- getSentimentalAnalysis(as.character(mycorpus$Var1));
}

plotReactions <- function(allreactions){
   names(allreactions) <- c("Likes","Loves","Haha","Wow","Sad", "Angry")
   allreactions <- allreactions[order(as.numeric(allreactions))]
   p <- ggplot() + geom_bar(stat="identity", aes(x=names(allreactions), y = as.numeric(allreactions))) + xlab("Reações") + ylab("Número de Ocorrências") + coord_flip() 
   png(reactionsfilename,width=1980,height=1280,res=300)
   print(p)
   dev.off()
}

plotPalavras <- function(allmessages){
   
   # convert string to vector of words
   spSamp<- unlist(strsplit(allmessages, split=", "))
   # find indices of words with non-ASCII characters
   nonAscIDX<- grep("spSamp", iconv(spSamp, "latin1", "ASCII", sub="spSamp"))
   # subset original vector of words to exclude words with non-ACCII characters
   ascVec<- spSamp[ - nonAscIDX]
   # convert vector back to string
   
#   ascVec %>% unnest_tokens(digram, ascVec, token = "ngrams", n = 2)
   
   
   ascSamp<- paste(ascVec, collapse = ", ")
   #remove numbers and punctuation
   clnSamp<- gsub('[[:digit:]]+', '', ascSamp)
   clnSamp<- gsub('[[:punct:]]+', '', clnSamp)
   clnSamp<- gsub("http[[:alnum:]]*", "", clnSamp)
   clnSamp<- gsub("([[:alpha:]])\1+", "", clnSamp)
   SampCrps<- Corpus(VectorSource(clnSamp))
   ##Convert characters to lower case
   SampCrps<- tm_map(SampCrps, tolower)
   ##Remove punctuation
   SampCrps<- tm_map(SampCrps, removePunctuation)
   ##Remove numbers
   SampCrps<- tm_map(SampCrps, removeNumbers)
   # Create patterns to elimina special code and other patterns
   # URLs
   urlPat<-function(x) gsub("(ftp|http)(s?)://.*\\b", "", x)
   SampCrps<-tm_map(SampCrps, urlPat)
   # Emails
   emlPat<-function(x) gsub("\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b", "", x)
   SampCrps<- tm_map(SampCrps, emlPat)
   # Twitter tags
   tt<-function(x) gsub("RT |via", "", x)
   SampCrps<- tm_map(SampCrps, tt)
   # Twitter Usernames
   tun<-function(x) gsub("[@][a - zA - Z0 - 9_]{1,15}", "", x)
   SampCrps<- tm_map(SampCrps, tun)
   #White Space
   #SampCrps<- tm_map(SampCrps, stripWhitespace)
   #Remove profane words
   #First get the list of bad words
   #bwdat<-read.table("en_bws.txt", header=FALSE, sep="\n", strip.white=TRUE)
   #names(bwdat)<-"Bad Words"
   #SampCrps<- tm_map(SampCrps, removeWords, bwdat[,1])
   SampCrps<-tm_map(SampCrps, removeWords, stopwords("portuguese"))
   # Create nGrams
   myCrps<- txt.to.words(SampCrps)
   #create data frames of one, two and three ngrams, 
   tblUniGrm<-data.frame(table(make.ngrams(myCrps, ngram.size = 1)))
   #Create a sorted table "stbl*" by decending frequency count
   stblUnigrm<-tblUniGrm[order(tblUniGrm$Freq, decreasing = TRUE),]
   top20unig<-stblUnigrm[1:20,]
   colnames(top20unig)<-c("UniGram","Frequency")
   #top20unig <- top20unig[order(top20unig$Frequency, decreasing=TRUE), ]  # sort
   p <- ggplot (top20unig, aes(x = reorder(UniGram, Frequency), y= Frequency )) + 
      geom_bar( stat = "Identity" , fill = "magenta" ) +  
      geom_text( aes (label = Frequency ) , vjust = - 0.10, hjust = -0.8, size = 2 ) +
      xlab( "Termos" ) +
      ylab( "Frequência" ) +
      theme ( axis.text.x = element_text ( angle = 45 , hjust = 1 ) ) + coord_flip()
   png(palavrasfilename,width=1980,height=1280,res=300)
   print(p)
   dev.off()
   return(tblUniGrm)
}
