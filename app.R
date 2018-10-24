
library(tidytext)
library(tidyverse)
library(shiny)
library(sentimentr)
library(wordcloud)
library(reshape2)
library(googleVis)
library(dplyr)

ui <- fluidPage( 
  
  tags$head(
    tags$style("textarea {
                    width:400px; 
                    height:100px;
                    display: block;
                    padding: 6px 12px;
                    font-size: 14px;
                    line-height: 1.42857143;
                    color: #555;
                    background-color: #fff;
                    background-image: none;
                    border: 1px solid #ccc;
                    border-radius: 4px;
                    -webkit-box-shadow: inset 0 1px 1px rgba(0,0,0,.075);
                    box-shadow: inset 0 1px 1px rgba(0,0,0,.075);
    }"
)),
  
  
withTags(
  div(
    h5(b("Enter text")), 
    textarea(id = "inputtext")
  )
),
#submitButton(text = "Analyze"),
  
  tabsetPanel(
  tabPanel("Package sentimentr", 
           tableOutput("dtab")
           , tableOutput("dtab1")
           , htmlOutput("dtab3")
           , tableOutput("dtab2")
           
           ),
  tabPanel("Package tidytext"
           , tableOutput("tibb"), plotOutput("ttab1")))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
   output$dtab <- renderTable( {
     sentby = as.data.frame(sentiment_by(input$inputtext, by = NULL))
     sentby$element_id = NULL
     sentby$sd = NULL
     sentby
     
   })
   
   output$dtab1 <- renderTable( {
     sentby = as.data.frame(sentiment(input$inputtext))
     sentby$element_id = NULL
     
     sentby
     
   })
   
   myf = reactive({
     input$inputtext
   })
   

   
   output$dtab3 <- renderUI({
     x=sentiment_by(myf(),by = NULL)
     highlight(x,file = "polarity.html",open = FALSE)
     getPage()
   })
   
   getPage<-function() {
     return(includeHTML("polarity.html"))
   }
 
   
   output$dtab2 <- renderTable({
     ss= as.data.frame(extract_sentiment_terms(myf()))
     ss$element_id = NULL
     for(i in 1:length(ss$sentence_id))
     {
       ss$negative[i]=  paste(ss$negative[i], collapse=', ' )
                   ss$neutral[i]=  paste(ss$neutral[i], collapse=', ' )
                   ss$positive[i]=  paste(ss$positive[i], collapse=', ' )
     }
    
     ss
   })


   output$ttab1 <- renderPlot({
     
    
     
     mytext = gsub("\\$", "", input$inputtext)
     
     tokens = data_frame(text = mytext) %>% unnest_tokens(word, text)
     
     
    y = tokens %>% anti_join(stop_words) %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort=TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
       comparison.cloud(colors = c("gray20", "darkgreen"),
                        max.words=100)
    y
   })
   
   output$tibb <- renderTable({
     mytext = gsub("\\$", "", myf())
     
     tokens = data_frame(text = mytext) %>% unnest_tokens(word, text)
     
     tokens %>%
       inner_join(get_sentiments("bing")) %>% # pull out only sentimen words
       count(sentiment) %>% # count the # of positive & negative words
       spread(sentiment, n, fill = 0)
   })

   
}

# Run the application 
shinyApp(ui = ui, server = server)

