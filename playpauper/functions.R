# 1. Card list ------------------------------------------------------------

get.card_names <- function() {
 out <- httr::content(
    httr::GET("https://api.scryfall.com/catalog/card-names")
  )$data
  
  return(out)
}

# 2. Pauper decklists -----------------------------------------------------
get.deck_list <- function() {
  decks <- readLines("https://mtgdecks.net/Pauper")
  
  deckname <- decks[grep("strong name=", decks)] %>%
    gsub("<strong name=\"|\">", "", .)
  price_usd <- decks[grep("class=\"paper option\"", decks) + 1] %>%
    gsub("$", "", ., fixed = TRUE) %>%
    as.integer()
  meta <- decks[grep("<td class=\"sort meta\"", decks) + 1] %>%
    gsub("<b>|</b>|%", "", .) %>%
    as.numeric()
  link <- decks[grep("a href=\"https://mtgdecks.net/Pauper/", decks)] %>%
    gsub("<a href=\"|\" class=.+", "", .)
  
  
  decklist <- data.frame(deckname, price_usd, meta, link)
  decklist <- decklist[decklist$price_usd != 0,]
  return(decklist)
}

filter.deck_list <- function(price) {
  price <- as.double(price)
  # if card value > max(decklist$price_usd). get subset sum closest to card value
  if(price>max(decklist$price_usd)) {
    decks <- decklist[adagio::subsetsum(decklist$price_usd, floor(price), method = "dynamic")$inds,]
    message <- paste0("For that amount you could buy the following ",
                      nrow(decks),
                      " decks with $",
                      round(price - sum(decks$price_usd), digits = 2),
                      " to spare."
    )
  } else if(price>min(decklist$price_usd)) {
    # if card value < max(decklist$price_usd) but > min(decklist$price_usd). get nearest deck value
    decks <- decklist[which(decklist$price_usd == max(decklist$price_usd[decklist$price_usd <= price])),]
    message <- paste0("That's as much as buying ",
                      " the following deck with $",
                      round(price - decks$price_usd, digits = 2),
                      " to spare"
    )
  } else {
    # if card value < min(decklist$price_usd). get lowest priced decks
    decks <- decklist[which(decklist$price_usd == min(decklist$price_usd)),]
    message <- paste0("That's not very expensive, but for $",
                      round(decks$price_usd - price, digits = 2),
                      " more you could buy the following deck:")
  }
  out <- list(message = message,
              decklist = decks)
  
  return(out)
}

# 3. Card info ------------------------------------------------------------
get.card_data <- function(name) {
  out <- httr::content(
    httr::GET("https://api.scryfall.com/cards/search",
      query = list(q = paste0("cheapest:usd ", "!\"", name, "\""))
    )
  )
  return(out)
}
