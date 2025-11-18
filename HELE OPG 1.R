
library(rvest)
library(dplyr)
library(stringr)
library(httr)

#OPG 1.1

startlink <- "https://www.bilbasen.dk/brugt/bil/land_rover/range_rover_sport?fuel=2&includeengroscvr=true&includeleasing=false&pagesize=200"

rawres     <- GET(startlink)
rawcontent <- content(rawres, as = "text")
page       <- read_html(rawcontent)

carlist <- page %>% html_elements("article")


ptag      <- "[class^='Listing_price']"
proptag   <- "[class^='Listing_properties']"
maketag   <- "[class^='Listing_makeModel']"
detailtag <- "[class^='Listing_details']"
desctag   <- "[class^='Listing_description']"
loctag    <- "[class^='Listing_location']"

#DF
colldf <- data.frame()

#Loop 
for (car in carlist) {
  tryCatch({
    
#Data fra listesiden
    price       <- car %>% html_element(ptag)       %>% html_text(trim = TRUE)
    props       <- car %>% html_element(proptag)    %>% html_text(trim = TRUE)
    makemodel   <- car %>% html_element(maketag)    %>% html_text(trim = TRUE)
    details     <- car %>% html_elements(detailtag) %>% html_text(trim = TRUE) %>% paste(collapse = "-")
    description <- car %>% html_elements(desctag)   %>% html_text(trim = TRUE) %>% paste(collapse = " ")
    location    <- car %>% html_elements(loctag)    %>% html_text(trim = TRUE) %>% paste(collapse = " ")
    
#Linket
    link_raw <- car %>% html_element("a") %>% html_attr("href")
    if (is.na(link_raw)) next
    
    link <- ifelse(grepl("^https?://", link_raw),
                   link_raw,
                   paste0("https://www.bilbasen.dk", link_raw))
    
    carid <- str_extract(link, "[0-9]{6,7}")
    
#Hent forhandlersiden
car_page <- GET(link) %>%
      content("text", encoding = "UTF-8") %>%
      read_html()
    
h2_texts <- car_page %>%
      html_elements("h2.bas-MuiTypography-h3") %>%
      html_text(trim = TRUE)
    
    seller_name <- h2_texts[!h2_texts %in% c("Detaljer","Beskrivelse","Generelle modeloplysninger*","Udstyr og tilbehør","Finansiering")][1]
    
    seller_address <- car_page %>%
      html_element("[data-e2e='seller-address'], .bas-MuiSellerInfoComponent-address") %>%
      html_text(trim = TRUE) %>%
      str_squish()
    
    cvr_text <- car_page %>%
      html_element("p.bas-MuiSellerInfoComponent-cvr") %>%
      html_text(trim = TRUE)
    
    seller_cvr <- str_extract(cvr_text, "\\d+")
    
#Dataframe
    colldf <- bind_rows(
      colldf,
      data.frame(
        price = price,
        details = details,
        makemodel = makemodel,
        props = props,
        description = description,
        location = location,
        link = link,
        carid = carid,
        seller_name = seller_name,
        seller_address = seller_address,
        seller_cvr = seller_cvr
      )
    )
    
  }, error = function(e) {
    message("Fejl på en bil: ", e$message)
  })
}

#OPG 1.2



colldf <- colldf %>%
  mutate(
    description = description %>%
      str_replace_all("\n|\r", ". ") %>%      # newline → ". "
      str_replace_all("[^A-Za-z0-9ÆØÅæøå., -]", " ") %>%  # fjern mærkelige tegn
      str_replace_all(" +", " ") %>%          # fjern dobbelte mellemrum
      str_trim()                               # trim i begge ender
  )



# OPG 1.3

#Første scraping med  scrapedate
old_df <- colldf %>%
  mutate(scrapedate = Sys.Date())

#Ny scraping én dag senere scrapedate
new_df <- old_df %>%
  mutate(scrapedate = scrapedate + 1)

# Tilføj 2 nye biler
new_cars <- data.frame(
  price = c("599.900 kr.", "649.900 kr."),
  details = c("Ny bil 1", "Ny bil 2"),
  makemodel = c("RR Sport 3.0 SDV6", "RR Sport 4.4 SDV8"),
  props = c("2018", "2019"),
  description = c("Ny bil tilføjet", "Ny bil tilføjet"),
  location = c("København", "Aarhus"),
  link = c("https://www.bilbasen.dk/ny1", "https://www.bilbasen.dk/ny2"),
  carid = c("900001", "900002"),
  seller_name = c("Ny Forhandler 1", "Ny Forhandler 2"),
  seller_address = c("Testvej 1, 2100 KBH", "Testvej 2, 8000 Aarhus"),
  seller_cvr = c("11111111", "22222222"),
  scrapedate = max(new_df$scrapedate)
)

# Pisændringer (tilføj 10.000 kr.)
change_ids <- sample(old_df$carid, 3)

new_df <- new_df %>%
  mutate(
    price_numeric = as.integer(str_remove_all(price, "\\D")),
    price_numeric = if_else(
      carid %in% change_ids,
      price_numeric + 10000L,
      price_numeric
    ),
    price = paste0(price_numeric, " kr.")
  ) %>%
  select(-price_numeric)

# Fjern 5 biler
remove_ids <- sample(old_df$carid, 5)

new_df <- new_df %>%
  filter(!carid %in% remove_ids)

# NY scrape df
new_scrape <- bind_rows(new_df, new_cars)



