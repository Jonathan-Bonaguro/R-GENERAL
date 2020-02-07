library(shiny)
library(tidyverse)
library(rtweet)
library(tidytext)
library(topicmodels)
library(wordcloud)
library(RColorBrewer)
library(tm)
twitter_token <- create_token(
  app = "UIUCBONSCRAPER",
  consumer_key = "4aJXOYnN12pbwIlXq7F1SCFKt",
  consumer_secret = "ZW4jzK6vkrQbdszaQOAdzPq7cT7E82jNR4ycmRMbI1ifh2QYhF",
  access_token = "1102735221949108226-D4zPvd1T4hYKd5KkbM5k4xZm7R5Vr7",
  access_secret = "QPA30E2SyNkoYaUTzAyDK0fkf3jROsdVRDBIKNCuOCFmg"
)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Word Cloud for Twitter Users"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         textInput("text", h3("Twitter Handle"),
                   value="realDonaldTrump")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot <- renderPlot({
    stuff <- get_timeline(input$text, n = 3200, include_rts = FALSE)
    stuff_original <- stuff[is.na(stuff$reply_to_screen_name),]
    tweets <- stuff_original['text']
    tweets <-  gsub("https\\S*", "", tweets)
    tweets <-  gsub("@\\S*", "", tweets) 
    tweets  <-  gsub("amp", "", tweets) 
    tweets  <-  gsub("[\r\n]", "", tweets)
    tweets  <-  gsub("[[:punct:]]", "", tweets)
    corp <- Corpus(VectorSource(tweets))
    corp <- tm_map(corp, content_transformer(tolower))
    corp <- tm_map(corp, removeNumbers)
    corp <- tm_map(corp, removeWords, stopwords("english"))
    corp <- tm_map(corp, removeWords, c("blabla1", "blabla2")) 
    corp <- tm_map(corp, removePunctuation)
    corp <- tm_map(corp, stripWhitespace)
    dtm <- TermDocumentMatrix(corp)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    wordcloud(words = d$word, freq = d$freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

