
#Link to Shiny app: https://allie-baker-uncc.shinyapps.io/problemset3/
#Collaborated with Syed Suffwan

library(shiny)
library(tidytext)
library(tidyverse)
library(wordcloud)
library(RColorBrewer)
library(shinythemes)


books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

# task4: add in getFreq function for pre-processing
getFreq <- function(book, stopwords = TRUE) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  
  text <-  tibble(text = readLines(sprintf("./data/%s.txt", book), encoding="UTF-8"))
  
  # could also pass column of text/character instead
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) 
  
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  return(text)
}


# task6: add in shinythemes function

ui <- fluidPage(
  #theme = shinytheme("yeti"),
  
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  sidebarLayout(
    
    
    # task2: add in the inputs in the sidebarPanel
    ## define inputs in sidebar -------------------------------
    sidebarPanel(
      
      ## select the book
      selectInput(inputId = "book", label = "Select a Book",
                  choices = books, selected = NULL),
      
      #stopwords checkbox
      checkboxInput(inputId = "stopwords", label = "Stopwords", value = TRUE),
      
      
      ## add action button to run app
      actionButton(inputId = "run_app", 
                   label = "Run"),
      
      ## seperate sections ---------
      hr(),
      
       #Header
      h3("Word Cloud Settings"),
      
      #Slider: Maximum number of words in word cloud
      sliderInput(inputId = "max_words", label = "Maximum Number of Words in Cloud",
                  min = 10, max = 200, value = 100, step = 10),
      
      #Slider: Size of the largest words
      sliderInput(inputId = "largest_words", label = "Size of Largest Words",
                  min = 1, max = 8, value = 4),
      
      #Slider: Size of the smallest words
      sliderInput(inputId = "smallest_words", label = "Size of Smallest Words",
                  min = 0.1, max = 4, value = 0.5),
      
      ## seperate sections ... adds in a line ---------
      hr(),

    #Header
      h3("Word Count Settings"),

      #Slider: Minimum word count (the minimum frequency of words to be able to show up in the counts chart)
      sliderInput(inputId = "min_word_cnt", label = "Minimum Frequency of Words for Counts Chart",
                  min = 10, max = 100, value = 25),

      #Slider: Font size of the actual words on the chart
      sliderInput(inputId = "font_size", label = "Font Size of Words for Counts Chart",
                  min = 8, max = 30, value = 14),
    ),
    
    # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)
    # task3: add in the outputs in the sidebarPanel
    # task6: and modify your figure heights
    ## Show output in main panel -----------------------------------------------
    mainPanel(
      
      ## show output in multiple tabs ----------------
      tabsetPanel(type = "tabs",
                  tabPanel(title = "Word Cloud",
                            plotOutput(outputId = "cloud", height = "600px", width = "800px")),
                  tabPanel(title = "Word Counts",
                            plotOutput(outputId = "freq", height = "600px", width = "800px")),
      ))))


server <- function(input, output) {
  
  # task5: add in reactivity for getFreq function based on inputs
  freq <- eventReactive(input$run_app,{
    withProgress({
      setProgress(message = "Processing corpus...")
      getFreq(input$book, input$stopwords) # ... = replace with the two inputs from Task 2
    })
  })

  #Render Plots for Output: Word Cloud
  output$cloud <- renderPlot({
    v <- freq()
    pal <- brewer.pal(8,"Dark2")
    v %>% 
      with(
        wordcloud(
          word, 
          n, 
          scale = c(input$largest_words, input$smallest_words),
          random.order = FALSE, 
          max.words = input$max_words, 
          colors=pal))
})

  #Render Plots for Output: Word Count
  output$freq <- renderPlot({
    v <- freq()
    pal <- brewer.pal(8,"Dark2")
    v %>%
      dplyr::filter(n > input$min_word_cnt) %>%
      ggplot(aes(x = reorder(word, + n), y = n)) +
      geom_col() +
      coord_flip() +
      theme(axis.text = element_text(size = input$font_size),
            axis.title = element_blank()
            )
  })
  
  }
  


shinyApp(ui = ui, server = server)
