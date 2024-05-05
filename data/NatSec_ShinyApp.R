# ===============================================
# Fill in the following fields
# ===============================================
# Title: NatSec Hackathon 2024
# Description: News Aggregate Dashboard
# Details: Made with RStudio
# Author: Berke1337
# Date: 05/05/2024


# ===============================================
# R packages
# (you can use other packages if you want to)
# ===============================================
library(shiny)
library(tidyverse)    # data wrangling and graphics
library(lubridate)    # for working with dates
library(sf)           # for working with geospatial vector-data
library(leaflet)      # web interactive maps
library(plotly)       # web interactive graphics
library(tidytext)     # for text mining
library(DT)           # to work with HTML table widgets
library(stringr)
library(udpipe)
library(textrank)
library(lattice)


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
  file = "primer_hackathon_data_control.csv", 
  col_types = cols(
    Source = col_character(),
    Date = col_character(),
    URL = col_character(),
    Title = col_character(),
    Content = col_character()
  ))
dat = dat |>
  select(c(Source, Date, URL, Title, Content))
clean_months = dat$Date |>
  str_extract('\\d\\d\\d\\d-\\d\\d-\\d\\d')
dat$Date = clean_months
dat = dat |>
  mutate(year = year(Date), month = month(Date), day = day(Date))
source_list = dat |>
  select(Source)
valid_sources = c(slice(arrange(unique(source_list)), 1:6))
dat <- dat %>% filter(Source %in% valid_sources$Source)

topics = read_csv(
  file = "PolData.csv", 
  col_types = cols(
    topics = col_character(),
  ))
topics <- topics |> select(topics)

topics <- unnest_tokens(tbl = topics, output = words, input = topics)

topics <- c(arrange(unique(topics)))

countries = data.frame(
  countries = readLines("countries")
)

topics <- data.frame(topics)
#topics$words

countries <- data.frame(countries)
#countries$countries

topics <- c(topics$words, countries$countries)

ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

# for demo purposes in this "template", we use data starwars
# (but you will have to replace this with the lyrics data)
#dat <- dplyr::starwars



# ===============================================
# Define "ui" for application
# ===============================================
ui <- fluidPage(
  
  # Application title
  fluidRow(width=12,
           column(12, align="center",
                  titlePanel("News Aggregate Dashboard"),
                  h4("Choose a country")
           )
  ),
  
  # -------------------------------------------------------
  # Input widgets 
  # Customize the following dummy widgets with your own inputs
  # -------------------------------------------------------
  #fluidRow(
    # replace with your widgets
  #  column(3),
  #  ), # closes column 1
    
    # replace with your widgets
   # column(3),
   # ), # closes column 2
    
    # replace with your widgets
  #  column(3),
  #  ), # closes column 3
    
    # replace with your widgets
  #  column(3,
  #         p(em("Inputs (column 4)")),
   #        radioButtons(inputId = "sentiment_widget", 
  #                      label = "Select a sentiment scheme (Table only)", 
   #                     choices = c("Bing (bigrams)" = 1, "nrc (emotion)" = 2, "Loughran" = 3)),
    #       textInput(inputId = "sentiment_word_widget", 
     #                label = "Type to search a sentiment word", 
 #                    value = "Enter text...")
#    ) # closes column 4
    
  #), # closes fluidRow
  
  # -------------------------------------------------------
  # Main Panel with outputs: plot and table
  # Feel free to customize the following output elements
  # -------------------------------------------------------
  mainPanel(width=12,
    fluidRow(
      column(12, align="center",
        leafletOutput("map", height = 600)
      )
    ),
    hr(),
    fluidRow(
      column(6,
             plotlyOutput(outputId = "plot2")
        ),
    ),
    fluidRow(
      column(6,
             plotlyOutput(outputId = "plot1")
        ),
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
  output$map <- renderLeaflet({
    # the following code is for demo purposes only; adapt it!!!
    leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2.5, maxZoom = 2.5)) |>
      addTiles() |>
      setView(lat = 20, lng = 0, zoom = 2.5)
  })
  
  plot1_data <- reactive({
    source <- "CNN"
    
    specific_source = dat |>
      #  mutate(valid_source = str_detect(dat$Source, "CNN Wire")) |>
      #  filter(valid_source == TRUE) |>
      filter(Source == source) |>
      filter(Content != is.na(Content)) |>
      select(Source, Content, Date) |>
      arrange(Source)
    
    sentiments = bing #customizable sentiment dictionary
    
    input_article = data.frame( 
      Content = specific_source$Content,
      Date = as.Date(specific_source$Date)
    )
    
    input_tokens <- input_article |>
      mutate(linenumber = Date) |>
      unnest_tokens(output = word, input = Content)
    
    input_sentiments <- input_tokens |>
      inner_join(sentiments, by = "word")
    
    input_sections <- input_sentiments |>
      count(index = linenumber, sentiment)
    
    article_sentim_flow <- input_sections |>
      pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
      mutate(sentiment = positive - negative)
    article_sentim_flow
  })
    
  plot2_data <- reactive({
    source <- "CNN"
    
    specific_source = dat |>
      #  mutate(valid_source = str_detect(dat$Source, "CNN Wire")) |>
      #  filter(valid_source == TRUE) |>
      filter(Source == source) |>
      filter(Content != is.na(Content)) |>
      select(Source, Content, Date) |>
      arrange(Source)
    
    specific_time = specific_source |>
      filter(Date >= as.Date("2023-12-01") & Date <= as.Date("2023-12-30"))
    
    highest_growing_subjects <- udpipe_annotate(ud_model, x = specific_time$Content, tagger = "default", parser = "none")
    highest_growing_subjects <- as.data.frame(highest_growing_subjects)
    
    stats <- subset(highest_growing_subjects, upos %in% c("PROPN", "ADJ"))
    stats <- txt_freq(x = stats$lemma)
    stats$key <- factor(stats$key, levels = rev(stats$key))
    
    banned_keywords = c("year", "people", "time", "week", "day", "month", "country", "company", "most", "old")
    
    stats <- stats |>
      filter(key %in% topics) %>%
      filter(!key %in% banned_keywords)
    stats
  })
  
  # ------------------------------------------------------------
  # Plot
  # Adapt code to display appropriate graphic(s)
  # ------------------------------------------------------------
  output$plot1 <- renderPlotly({
    # the following code is for demo purposes only;
    # replace the code below with your code!!!
    ggplot(data = plot1_data(),
           aes(x = index, y = sentiment, fill = factor(sign(sentiment)))) +
      geom_col(show.legend = FALSE) +
      scale_x_date(limits = as.Date(c("2023-11-01", "2023-12-31"))) + # customizable date range preference
      theme(legend.position = "none")
  })
    
  output$plot2 <- renderPlotly({
    ggplot(data = head(plot2_data(),30)) +
      geom_col(aes(x = freq, y = key), fill = 'cadetblue')
 })
  
  
  # ------------------------------------------------------------
  # Table
  # Adapt code to display appropriate table
  # ------------------------------------------------------------
  output$table <- renderDataTable({
    # the following code is for demo purposes only;
    # replace the code below with your code!!!
    #step 1) pass lyrics into bigram maker
    #table_data()
    
  })

  
} # closes server



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

