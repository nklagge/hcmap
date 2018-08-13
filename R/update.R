library(dplyr)
library(ggmap)
library(httr)
library(magrittr)
library(purrr)
library(readr)
library(rvest)
library(stringr)
library(xml2)

# utilities
qry_nyt <- function(pageno = 0, begin_date = "20120521"){
  arglist <- list(apikey = getOption("NYT_API_KEY"),
                  q = "\"Hungry City\" \"Ligaya Mishan\"",
                  fq = "news_desk:\"Dining\"",
                  begin_date = begin_date,
                  sort = "newest",
                  fl = "web_url,snippet,headline,byline",
                  page = pageno)
  url <- "https://api.nytimes.com/svc/search/v2/articlesearch.json?"
  Sys.sleep(1)
  url %>%
    GET(query = arglist) %>%
    content(as = "parsed")
}
get_hit_meta <- function(item){
  tibble(url     = if_not_null(item$web_url),
         snippet = if_not_null(item$snippet),
         ckkr    = if_not_null(item$headline$content_kicker))
}
if_not_null <- function(entity){
  if (is.null(entity)) NA else entity
}
get_content <- function(url){
  url %>%
    read_html() %>%
    html_nodes(".review-meta, .ReviewHeader-metadata--3l_zc") %>%
    lapply(parse_node) %>%
    bind_rows() %>%
    mutate(url = url)
}
parse_node <- function(n){
  tibble(name    = get_fields(n, "h4, dt"),
         address = get_fields(n, ".address"),
         cuisine = get_fields(n, ".cuisine, .cuisines"),
         cost    = get_cost(n),
         status  = get_fields(n, ".restaurant_status, .ReviewHeader-isClosed--Z3_Mp"))
}
get_fields <- function(node, fields){
  x <- node %>%
    html_nodes(fields) %>%
    html_text()
  if (length(x) == 0) NA else x
}
get_cost <- function(n){
  x <- n %>%
    get_fields("dd, li") %>%
    str_subset("[$]+")
  if (length(x) == 0) NA else x
}

# main
get_hclinks <- function(begin_date) {
  new_hits <- qry_nyt(begin_date = begin_date) 
  # how many new hits do we have
  num_hits <- new_hits$response$meta$hits
  new_hits <- new_hits$response$docs
  # are there results pages beyond p. 0?
  last_pg_no <- floor(num_hits / 10)
  # if so, get them
  if (last_pg_no > 0) {
    further_hits <- c(1:last_pg_no) %>%
      map(~qry_nyt(pageno = .x, begin_date = begin_date)) %>%
      map(~ .x$response$docs) %>%
      flatten()
    new_hits <- append(further_hits, new_hits, after = 0)
  }
  if (length(new_hits) > 0) {
    new_hits %>%
      map(get_hit_meta) %>%
      bind_rows() %>%
      filter(ckkr == "Hungry City") %>%
      select(url, snippet) %>%
      return()
  }
  # return zero-length tibble if no new hits
  tibble()
}
get_hcdata <- function(links) {
  links$url %>%
    map(get_content) %>%
    bind_rows() %>%
    filter(is.na(status)) %>%
    select(-status)
}
add_geodata <- function(dat) {
  # this is supposed to help but seems like it causes problems
  #register_google(key = getOption("GM_API_KEY"))
  dat %>%
    mutate(address = str_squish(address),
           address_geo = paste0(address, ", New York, NY")) %>%
    mutate_geocode(address_geo)
}
add_county <- function(dat){
  # this is supposed to help but seems like it causes problems
  #register_google(key = getOption("GM_API_KEY"))
  res <- map(1:nrow(dat), ~ revgeocode(c(dat[[.x, "lon"]], dat[[.x, "lat"]]),
                                       output = "more")) %>%
    bind_rows() %>%
    select(neighborhood, political)
  dat %>% bind_cols(res)
}
clean_hcdata <- function(dat) {
  dat %>%
    mutate(cuisine = ifelse(is.na(cuisine), "Other", str_trim(cuisine))) %>%
    mutate(cuisine = str_replace(cuisine, "Latin American", "Latin")) %>%
    mutate(neighborhood = ifelse(is.na(neighborhood), "Other", neighborhood)) 
}
add_popups <- function(dat) {
  dat %>%
    mutate(date = as.Date(str_extract(url,"[0-9]{4}/[0-9]{2}/[0-9]{2}")),
           popup = paste0("<a href=\"", url, "\", target=\"_blank\">", 
                          name,"</a>",
                          "<br>",
                          "<strong>Cuisine: </strong>",
                          cuisine,
                          "<br>",
                          "<strong>Cost: </strong>",
                          cost,
                          "<br>",
                          "<strong>Review Date: </strong>",
                          date,
                          "<br>",
                          "<strong>Address: </strong>",
                          address,
                          "<br>",
                          snippet))
}

# update it 
path <- "Data/restaurants.csv"
resto <- read_csv(path)
# check for hits since latest cached result
begin_date <- format.Date(max(resto$date) + 1, "%Y%m%d")
hclinks <- get_hclinks(begin_date)
# if there are new links
if (nrow(hclinks) > 0) {
  # retrieve and clean restaurant data
  hcdata <- hclinks %>%
    get_hcdata()  %>%
    add_geodata() %>%
    add_county() %>% 
    clean_hcdata() %>%
    left_join(hclinks) %>%
    add_popups() 
  # add to existing data and export updated file
  hcdata %>%
    bind_rows(resto) %>%
    write_csv(path)
}