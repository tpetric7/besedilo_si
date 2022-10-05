###########################################################
# Web scraping with the tidyverse, rvest, and Selenium    #
# Prepared for Salt Lake City R User Group                #
# Author: Josh McCrain                                    #
# joshuamccrain.com | @joshmccrain                        #
###########################################################

library(tidyverse)
library(rvest)




# The Basics: -------------------------------------------------------------


read_html("test.html") %>% 
  html_nodes("div") %>% 
  html_text(trim=T)


# identical to this clumsy thing:
h <- read_html("test.html")
nodes <- html_nodes(h, "div")
html_text(nodes, trim = T)


read_html("test.html") %>% 
  html_nodes("#div1") %>% 
  html_text(trim=T)


read_html("test.html") %>% 
  html_nodes("#div1 h1") %>% 
  html_text(trim=T)



read_html("test.html") %>% 
  html_nodes("#div2 span.important") %>% 
  html_text(trim=T)







# A Real Example ----------------------------------------------------------


## Number and partisan breakdown of both chambers:

read_html("https://openstates.org/ut/") %>% 
  html_nodes(".state-overview-chambers") %>% 
  html_text(trim = T) %>% 
  str_squish


## Just the Senate

read_html("https://openstates.org/ut/") %>% 
  html_nodes(".state-overview-chambers div:nth-child(1)") %>% 
  html_text(trim = T) %>% 
  str_squish


## Just the partisan breakdown in the Senate

read_html("https://openstates.org/ut/") %>% 
  html_nodes(".state-overview-chambers div:nth-child(1) li") %>% 
  html_text(trim = T) %>% 
  str_squish


## Bills recently introduced

read_html("https://openstates.org/ut/") %>% 
  html_nodes(".card") %>% 
  html_text(trim = T) %>% 
  str_squish %>% 
  .[1:4] # Why this? Otherwise returns recently passed as well


## Just bill numbers

read_html("https://openstates.org/ut/") %>% 
  html_nodes(".card h3") %>% 
  html_text(trim = T) %>% 
  str_squish %>% 
  .[1:4]


## Just bill URLs:

urls <- read_html("https://openstates.org/ut/") %>% 
  html_nodes(".card h3 a") %>% 
  html_attr("href") %>% # can also return, e.g., image sources
  str_squish %>% 
  .[1:4]









# Iterative Scraping ------------------------------------------------------



## Go to each bill's page; download PDF of bill text

download_pdf <- function(page){
  
  #page <- urls[1]
  
  page <- paste0("https://openstates.org", page)
  
  bill_versions <- read_html(page) %>% 
    html_nodes(".hover") %>% 
    html_table %>% 
    pluck(1)
  
  bill_urls <- read_html(page) %>% 
    html_nodes(".hover td a") %>% 
    html_attr("href") %>% 
    .[1:nrow(bill_versions)]
  
  bill_versions <- cbind(bill_versions, bill_urls)
  
  bill_num <- read_html(page) %>% 
    html_node(".overview__heading") %>% 
    html_text
  
  # Clean up our dataframe:
  bill_versions$bill_num <- bill_num
  bill_versions <-
    mutate(bill_versions,
           file_name = paste0(bill_num, "-", `Bill Text Versions`, ".pdf"))

  # Download bill PDFs to our computer:
  map2(
    bill_versions$bill_urls,
    bill_versions$file_name,
    ~ download.file(.x, .y, method = "curl", quiet=T)
  )
  
  # Build in a pause:
  cat("Finished scraping: ", page, "\n")
  
  Sys.sleep(1)
  
 }

# quiet version of map
walk(urls, download_pdf)



## Automate large-scale data collection:
# https://openstates.org/ut/bills/?session=2022&page=1

# Notice URL construction!

url <- "https://openstates.org/ut/bills/?session=2022&page=1"

read_html(url) %>% 
  html_node(".cell.medium-9 p") %>% 
  html_text

# grab the total number of bills:
num_bills <- read_html(url) %>% 
  html_node(".cell.medium-9 p") %>% 
  html_text %>% 
  str_replace_all("[:alpha:]", "") %>% 
  str_trim %>% 
  word(-1) %>% 
  as.numeric

pages <- ceiling(num_bills/20)

# comment this out if wrapping in function
page <- 5

new_url <- paste0("https://openstates.org/ut/bills/?session=2022&page=", page)

bill_df <- read_html(new_url) %>% 
  html_node("table.hover") %>% 
  html_table %>% 
  janitor::clean_names() %>% 
  mutate_all(str_squish)

# add column of bill URLs:
bill_urls <- read_html(new_url) %>% 
  html_nodes("table.hover a.td-link") %>% 
  html_attr("href")

bill_df <- cbind(bill_df, bill_urls)

## iterate this exercise over 1:pages









# Selenium ----------------------------------------------------------------


library(RSelenium)


# Open up a browser instance!

rD <- rsDriver(browser="firefox", port=4544L, verbose=F)
remDr <- rD[["client"]]


# Then have R control the browser:

remDr$navigate("https://www.espn.com")

remDr$navigate("https://www.fcc.gov/media/engineering/dtvmaps")

# Type in a zip, hit "Go!" 
# But notice URL doesn't change on page. Now what??

read_html("https://www.fcc.gov/media/engineering/dtvmaps") %>% 
  html_nodes(".tbl_mapReception") %>% 
  html_table 

# Uh oh...

# We can use Selenium to work the browser for us:

zip <- "84124"
remDr$findElement(using = "id", value = "startpoint")$sendKeysToElement(list(zip))
# other possible ("xpath", "css selector", "id", "name", "tag name", "class name", "link text", "partial link text")


remDr$findElements("id", "btnSub")[[1]]$clickElement()


html <- remDr$getPageSource()[[1]]

# And now back to rvest!

signals <- read_html(html) %>% # parse HTML
  html_nodes("table.tbl_mapReception") %>% # extract table nodes with class = "tbl_mapReception"
  .[3] %>% # keep the third of these tables
  .[[1]] %>% # keep the first element of this list
  html_table(fill=T) # have rvest turn it into a dataframe


# But how to iterate? 

zip <- "48823"
remDr$findElement(using = "id", value = "startpoint")$sendKeysToElement(list(zip))

# Now we have an incorrect zip code in the text box...

# Clear the text box:
remDr$findElement("id", "startpoint")$clearElement()

# But what if we still accidentally input a bad zip?

zip <- "27511111"
remDr$findElement(using = "id", value = "startpoint")$sendKeysToElement(list(zip))
remDr$findElements("id", "btnSub")[[1]]$clickElement()

# Alert!

alert <- try(remDr$getAlertText(), silent=T) # check if there is an alert window

if(class(alert) != "try-error") { # if an alert window is present, do the following
  
  # Return a blank dataframe for this zip:
  signals <- data.frame(callsign = NA, network = NA, ch_num = NA, band = NA, strength = NA, cont.strength = NA)
  
  # Accept the alert to close it out
  remDr$acceptAlert()
  
  # Delete the bad zip:
  remDr$findElement("id", "startpoint")$clearElement()
  
} else { # if no alert, continue on as normal
  
  # normal scraping procedure code here
  
}








# Example of how to iterate Selenium: -------------------------------------
# Do not run, it takes a long time

zips.df <- read.csv("zip_code_data.csv") # csv of zip codes

rD <- rsDriver(browser="firefox", port=4557L)
remDr <- rD[["client"]]

remDr$navigate("https://www.fcc.gov/media/engineering/dtvmaps")

scrape.zips <- function(zip){ # our scraping function
  
  remDr$findElement("id", "startpoint")$sendKeysToElement(list(zip))
  remDr$findElements("id", "btnSub")[[1]]$clickElement()
  
  alert <- try(remDr$getAlertText(), silent=T)
  
  if(class(alert) != "try-error") {
    
    signals <- data.frame(callsign = NA, network = NA, ch_num = NA, band = NA, strength = NA, cont.strength = NA)
    remDr$acceptAlert()
    remDr$findElement("id", "startpoint")$clearElement()
    
  } else {
    Sys.sleep(2)
    
    html <- remDr$getPageSource()[[1]]
    
    cont.strength <- read_html(html) %>% 
      html_nodes(".callsign") %>% 
      html_attr("onclick") %>% 
      str_extract("(?<=RX Strength: )\\s*\\-*[0-9.]+")
    
    signals <- read_html(html) %>%
      html_nodes("table.tbl_mapReception") %>%
      .[3] %>%
      .[[1]] %>%
      html_table(fill=T)
    
    names(signals) <- c("rm", "callsign", "network", "ch_num", "band", "rm2")
    
    signals <- signals %>%
      slice(2:n()) %>%
      filter(callsign != "") %>%
      select(callsign:band)
    
    strength <- read_html(html) %>%
      html_nodes("table.tbl_mapReception:nth-child(3) .ae-img") %>%
      html_attr("src")
    
    if(length(strength)==0) { strength <- "none" }
    if(length(cont.strength)==0) { cont.strength <- "none" }
    
    signals <- cbind(signals, strength) %>% cbind(cont.strength)
    
    signals <- mutate(signals, strength = strength %>% str_extract("strength."))
  }
  
  remDr$findElement("id", "startpoint")$clearElement()
  
  return(signals)
  
  Sys.sleep(runif(1, 1, 3))
  
}


scrape_safe <- function(zip){
  
  result <- try(scrape.zips(zip))
  
  if (class(result) == "try-error") { # if there is any error caught, return a blank dataframe and keep going
    cat("Error encountered for zip:", zip, "\n")
    return(data.frame()) 
    Sys.sleep(runif(1, 1, 3))
  } else { # if no error, keep going as normal to next zip
    return(result)
  }
}
