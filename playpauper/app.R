# 0. Init -----------------------------------------------------------------

# 0.1. Packages -----------------------------------------------------------
pkgs <- c("adagio", "dplyr", "httr", "shiny")
install.packages(pkgs)

library(adagio)
library(dplyr)
library(httr)
library(shiny)

# 0.2. Functions ----------------------------------------------------------
# source("./playpauper/functions.R")
source("functions.R")

# 0.3. Data ---------------------------------------------------------------
cardnames <- get.card_names()
decklist <- get.deck_list()

# 1. UI -------------------------------------------------------------------
ui <- fluidPage(

  # Application title
  titlePanel("Play Pauper"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "search",
        label = "Search for a card name",
        choices = NULL,
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      textOutput(
        outputId = "cardinfo"
      ),

      htmlOutput("image"),

      textOutput("deckmessage"),

      DT::dataTableOutput("decklist")
    )
  )
)


# 3. Server ---------------------------------------------------------------

server <- function(input, output, session) {

card <- eventReactive(input$search, {
 card_data <- get.card_data(input$search)
 return(card_data)
})

decks <- eventReactive(card()$data[[1]]$price$usd, {
  decks_data <- filter.deck_list(card()$data[[1]]$price$usd)
  return(decks_data)
})

output$cardinfo <- renderText({
  req(input$search)
  paste0(card()$data[[1]]$name, " costs $", card()$data[[1]]$price$usd)
})

output$image <- renderText({
  req(input$search)
  c('<img src="', card()$data[[1]]$image_uris$small, '">')})

output$deckmessage <- renderText({
  req(input$search)
  decks()$message
})

output$decklist <- DT::renderDataTable({
  req(input$search)
  decks()$decklist
})

updateSelectizeInput(
  session,
  'search',
  choices = cardnames,
  selected = "Force of Will",
  options = list(placeholder = "Try: 'Force of Will'", maxItems = 1),
  server = TRUE)


}
# Run the application
shinyApp(ui = ui, server = server)
