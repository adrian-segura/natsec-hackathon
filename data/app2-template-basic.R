# ===============================================
# Fill in the following fields
# ===============================================
# Title: ShinyApp2 - Lyric Analysis
# Description: Analyzes lyrics.
# Details: Made with RStudio
# Author: Nicholas Perematko
# Date: 04/11/2024


# ===============================================
# R packages
# (you can use other packages if you want to)
# ===============================================
library(shiny)
library(tidyverse) # for data manipulation and graphics
library(tidytext)  # for text mining
library(DT)        # to work with HTML table widgets


# =======================================================
# Sentiment Lexicons
# Uncomment the lexicon(s) that you would like to use if
# you choose to perform some kind of sentiment analysis
# =======================================================
bing = read_csv("bing.csv", col_types = "cc")
nrc = read_csv("nrc.csv", col_types = "cc")
loughran = read_csv("loughran.csv", col_types = "cc")
# afinn = read_csv("afinn.csv", col_types = "cd")


# =======================================================
# Import data
# Uncomment the commands below in order to import the data
# =======================================================
 dat = read_csv(
   file = "lyrics.csv", 
   col_types = cols(
     artist = col_character(),
     album = col_character(),
     year = col_character(),
     song = col_character(),
     lyrics = col_character()
   ))

# for demo purposes in this "template", we use data starwars
# (but you will have to replace this with the lyrics data)
#dat <- dplyr::starwars



# ===============================================
# Define "ui" for application
# ===============================================
ui <- fluidPage(
  
  # Application title
  titlePanel("Lyric Sentiment App"),
  hr(),
  
  # -------------------------------------------------------
  # Input widgets 
  # Customize the following dummy widgets with your own inputs
  # -------------------------------------------------------
  fluidRow(
    # replace with your widgets
    column(3,
           p(em("Inputs (column 1)")),
           selectInput(inputId = "artist_widget", 
                        label = "Choose an artist",
                        choices = (
                          c("Taylor Swift" = "Taylor Swift",
                            "Beyonce" = "Beyonce",
                            "Katy Perry" = "Katy Perry",
                            "Pink" = "Pink",
                            "Rihanna" = "Rihanna"
                          )
                        ),
                        selected = "Beyonce"),
           selectInput(inputId = "album_widget",
                       label = "Choose an album",
                       choices = (
                          c("Dangerously In Love" = "Dangerously In Love",
                            "The Fighting Temptations" = "The Fighting Temptations",
                            "Bday" = "Bday",
                            "Bday Deluxe Edition" = "Bday Deluxe Edition",
                            "I Am... Sasha Fierce (Deluxe Edition)" = "I Am... Sasha Fierce (Deluxe Edition)",
                            "I Am... Sasha Fierce - The Bonus Tracks" = "I Am... Sasha Fierce - The Bonus Tracks",
                            "4" = "4",
                            "Beyoncé" = "Beyoncé",
                            "Lemonade" = "Lemonade",
                            "Everything Is Love" = "Everything Is Love",
                            "HOMECOMING: THE LIVE ALBUM" = "HOMECOMING: THE LIVE ALBUM",
                            "The Lion King: The Gift" = "The Lion King: The Gift",
                            "RENAISSANCE" = "RENAISSANCE",
                            "Katy Hudson" = "Katy Hudson",
                            "(A) Katy Perry	" = "(A) Katy Perry	",
                            "One Of The Boys" = "One Of The Boys",
                            "Teenage Dream" = "Teenage Dream",
                            "Prism" = "Prism",
                            "Witness" = "Witness",
                            "Smile" = "Smile"
                          )
                       ),
                       selected = "Beyoncé"),
    ), # closes column 1
    
    # replace with your widgets
    column(3,
           p(em("Inputs (column 2)")),
           checkboxGroupInput(inputId = "display_widget", 
                        label = "Choose which graphs to display",
                        choices = c("Contribution to Sentiment" = 1, "Narriative Sentiment" = 2)
                        )
    ), # closes column 2
    
    # replace with your widgets
    column(3,
           p(em("Inputs (column 3)")),
           textInput(inputId = "search_widget", 
                        label = "Type to search an artists albums", 
                        value = "Enter text...")
    ), # closes column 3
    
    # replace with your widgets
    column(3,
           p(em("Inputs (column 4)")),
           radioButtons(inputId = "sentiment_widget", 
                        label = "Select a sentiment scheme (Table only)", 
                        choices = c("Bing (bigrams)" = 1, "nrc (emotion)" = 2, "Loughran" = 3)),
           textInput(inputId = "sentiment_word_widget", 
                     label = "Type to search a sentiment word", 
                     value = "Enter text...")
    ) # closes column 4
    
  ), # closes fluidRow
  
  hr(), # horizontal rule
  
  # -------------------------------------------------------
  # Main Panel with outputs: plot and table
  # Feel free to customize the following output elements
  # -------------------------------------------------------
  mainPanel(
    h3("Plot"),
    fixedRow(
      column(7,
        plotOutput(outputId = "plot1")),
      column(3,
        plotOutput(outputId = "plot2")),
    ),
    hr(),
    h3("Table"),
    fluidRow(
      column(6,
        dataTableOutput(outputId = "table")),
      column(8,
             dataTableOutput(outputId = "plot3")),
    ),
  ) # closes mainPanel
  
) # closes ui



# ===============================================
# Define server logic
# ===============================================
server <- function(input, output) {
  
  # ------------------------------------------------------------
  # Reactive object(s)
  # ------------------------------------------------------------
  # the following code is for demo purposes only;
  # replace the code below with your code!!!
  plot1_data <- reactive({
    input_song = dat |>
      filter(artist == input$artist_widget & album == input$album_widget) |>
      select(lyrics)
    input_tokens = unnest_tokens(tbl = input_song, output = word, input = lyrics)
    input_sentiments <- input_tokens |>
      inner_join(sentiments, by = "word")
    
    input_tokens <- input_song |>
      mutate(linenumber = row_number()) |>
      unnest_tokens(output = word, input = lyrics)
    #input_tokens
    
    inputtokens <- input_tokens |>
      inner_join(sentiments, by = "word") |>
      count(word, sentiment, sort = TRUE) |>
      ungroup() |>
      group_by(sentiment) |>
      slice_head(n = 10) |>
      mutate(word = reorder(word, n))
  })
    
  plot2_data <- reactive({
    input_song = dat |>
      filter(artist == input$artist_widget & album == input$album_widget) |>
      select(lyrics)
    input_tokens = unnest_tokens(tbl = input_song, output = word, input = lyrics)
    input_sentiments <- input_tokens |>
      inner_join(sentiments, by = "word")
    
    input_tokens <- input_song |>
      mutate(linenumber = row_number()) |>
      unnest_tokens(output = word, input = lyrics)
    
    input_sentiments <- input_tokens |>
      inner_join(sentiments, by = "word")
    
    input_sections <- input_sentiments |>
      count(index = linenumber, sentiment) 
    #input_sections
    
    lyric_sentim_flow <- input_sections |>
      pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
      mutate(sentiment = positive - negative)
    #lyric_sentim_flow
    
    lyric_sentim_flow
  })
  
  plot3_data <- reactive ({
    plot_data = dat |>
      filter(artist == input$search_widget) |>
      select(album) |>
      unique()
    plot_data
  })
      
  table_data <- reactive({
    
    input_song = dat |>
      filter(artist == input$artist_widget & album == input$album_widget) |>
      select(lyrics)
    input_tokens = unnest_tokens(tbl = input_song, output = word, input = lyrics)
    input_sentiments <- input_tokens |>
      inner_join(sentiments, by = "word")
    
    if(input$sentiment_widget == 1) {
    
    input_bigrams <- input_song |>
      unnest_tokens(
        output = bigram, 
        input = lyrics, 
        token = "ngrams", 
        n = 2)
    input_bigrams
    
    # step 2) filter those containing selected word
    bigrams_separated <- input_bigrams |>
      separate(bigram, c("word1", "word2"), sep = " ")
    bigrams_separated
    
    # step 3) make a bigram table with selected word
    specific_bigrams = input_bigrams |>
      filter(str_detect(bigram, input$sentiment_word_widget)) |>
      count(bigram, sort = TRUE, name = "count")
    specific_bigrams
    } else if(input$sentiment_widget == 2) {
      nrc_filter <- nrc %>%
        filter(sentiment == input$sentiment_word_widget)
      
      table_data <- input_tokens %>%
        inner_join(nrc_filter) %>%
        count(word,name = input$sentiment_word_widget, sort = TRUE)
      table_data
      
    } else if(input$sentiment_widget == 3) {
      
      input_tokens <- input_song |>
        mutate(linenumber = row_number()) |>
        unnest_tokens(output = word, input = lyrics)
      input_tokens
      
      input_sentiments <- input_tokens |>
        inner_join(loughran, by = "word")
      
      input_sections <- input_sentiments |>
        count(index = linenumber, sentiment) 
      input_sections
      
      lyric_sentim_flow <- input_sections |>
        pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
        mutate(sentiment = positive - negative)
      lyric_sentim_flow
      
    }
  })
  
  
  # ------------------------------------------------------------
  # Plot
  # Adapt code to display appropriate graphic(s)
  # ------------------------------------------------------------
  output$plot1 <- renderPlot({
    # the following code is for demo purposes only;
    # replace the code below with your code!!!
  if(any(1==input$display_widget)) {
      ggplot(data = plot1_data(), aes(y = word, x = n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ sentiment, scales = "free_y") +
      labs(y = "Contribution to sentiment",
           x = NULL,
           title = "Words that contribute to positive and negative sentiments")
  }
  })
    
  output$plot2 <- renderPlot({
    if(any(2==input$display_widget)) {
    ggplot(data = plot2_data(),
           aes(x = index, y = sentiment, fill = factor(sign(sentiment)))) +
      geom_col(show.legend = FALSE) +
      theme_bw() +
      labs(title = "Sentiment through the narrative of input_album")
    }
 })
  
  output$plot3 <- renderDataTable({
    plot3_data()
  })
  
  
  # ------------------------------------------------------------
  # Table
  # Adapt code to display appropriate table
  # ------------------------------------------------------------
  output$table <- renderDataTable({
    # the following code is for demo purposes only;
    # replace the code below with your code!!!
    #step 1) pass lyrics into bigram maker
    table_data()
    
  })

  
} # closes server



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

