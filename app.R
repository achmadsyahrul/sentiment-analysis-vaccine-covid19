library(ggplot2)
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyverse)
library(tm)
library(wordcloud)
library(caret)
library(RTextTools)
library(e1071)
library(syuzhet)
library(here)
library(vroom)
library(plotly)

#load dataset
tweetclean = read.csv("tweetclean.csv", stringsAsFactors = FALSE)

#cloumn text to char
tweet <- as.character(tweetclean$text)

#Calls the NRC sentiment dictionary to calculate the presence of eight different emotions and their corresponding valence in a text file.
sentiment<-get_nrc_sentiment(tweet, language = "english")
tweetsentiment<-cbind(tweetclean$text,sentiment)
par(mar=rep(3,4))
barplot(colSums(sentiment),col=rainbow(10),ylab='count',main='sentiment analisis')


class_sentiment <- data.frame(negative=sentiment$negative,positive=sentiment$positive)
klasifikasi <- mutate(class_sentiment, text_sentiment = ifelse((class_sentiment$negative != class_sentiment$positive),
                                                               ifelse(class_sentiment$negative!=0,print("negative"),
                                                                      print("positive")),print("neutral")))

data <- data.frame(text=tweet,sentiment=klasifikasi$text_sentiment)



ui <- fluidPage(
  titlePanel("Vaccine Covid Tweet"),
  mainPanel(
    
    tabsetPanel(type = "tabs",
                tabPanel("Data ", DT::dataTableOutput('data')), # Output Data Dalam Tabel
                tabPanel("Data testing", DT::dataTableOutput('testing')), # Output Data Test
                tabPanel("Scatterplot", plotOutput("scatterplot")), # Plot
                tabPanel("Freq Word", plotOutput('freq')), # Plot
                tabPanel("Wordcloud", plotOutput("Wordcloud")) # Plot Wordcloud
    )
  )
)

# SERVER
server <- function(input, output, session) {
  # Output Data tabel
  output$data = DT::renderDataTable({
    
    DT::datatable(data, options = list(lengthChange = FALSE))
  })
  output$freq <- renderPlot({
    corpus <- Corpus(VectorSource(tweetclean$text))
    corpus[[1]][1]
    corpus<-tm_map(corpus, content_transformer(tolower))
    corpus<-tm_map(corpus, removeNumbers)
    corpus<-tm_map(corpus, removeWords, stopwords("english"))
    corpus<-tm_map(corpus, removePunctuation)
    corpus<-tm_map(corpus, removeWords, c("vaccine","covid"))
    corpus[[1]][1]
    
    tdm<-TermDocumentMatrix(corpus)
    m<-as.matrix(tdm)
    v<-sort(rowSums(m),decreasing = TRUE)
    d<-data.frame(word=names(v),freq=v)
    barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
            col ="lightgreen", main ="Most frequent words",
            ylab = "Word frequencies")
  })
  output$testing = DT::renderDataTable({
    #testing
    #training
    twittertrain <- data[1:300,]
    twittertesting <- data[301:500,]
    tweetcorpus <-VCorpus(VectorSource(twittertrain$text))
    tweetDTM <-DocumentTermMatrix(tweetcorpus)
    
    tweetfreq<-findFreqTerms(tweetDTM,1)
    tweetDTMfreq<-tweetDTM[,tweetfreq]
    
    
    tweet_sentiment <- factor(twittertrain$sentiment)
    tweet_sentiment
    
    #Naive Bayes
    tweetclass<-naiveBayes(twittertrain,tweet_sentiment)
    predicted = predict(tweetclass, twittertesting); 
    predicted
    confusionMatrix(table(twittertesting[, 2], predicted))
    
    analysis_result<-data.frame(twittertesting$text,predicted)
    
    DT::datatable(analysis_result, options = list(lengthChange = FALSE))
  })
  
  #scatterplot
  output$scatterplot <- renderPlot({
    tweetsentiment<-cbind(tweetclean$text,sentiment)
    par(mar=rep(3,4))
    barplot(colSums(sentiment),col=rainbow(10),ylab='count',main='sentiment analisis')
  }, height=400)
  
  #wordcloud
  output$Wordcloud <- renderPlot({
    
    corpus <- Corpus(VectorSource(tweetclean$text))
    corpus[[1]][1]
    corpus<-tm_map(corpus, content_transformer(tolower))
    corpus<-tm_map(corpus, removeNumbers)
    corpus<-tm_map(corpus, removeWords, stopwords("english"))
    corpus<-tm_map(corpus, removePunctuation)
    corpus<-tm_map(corpus, removeWords, c("vaccine","covid"))
    corpus[[1]][1]
    
    tdm<-TermDocumentMatrix(corpus)
    m<-as.matrix(tdm)
    v<-sort(rowSums(m),decreasing = TRUE)
    d<-data.frame(word=names(v),freq=v)
    
    wordcloud(d$word, d$freq, min.freq = 1,           
              max.words=50, random.order=FALSE, rot.per=0.35,            
              colors=brewer.pal(8, "Dark2"))
    
  })
}


shinyApp(ui = ui, server = server, options = list(height = "500px"))


