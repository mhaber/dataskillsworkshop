library(rvest)
library(purrr)
library(furrr)


# Get Profile Links -------------------------------------------------------

url <- "https://www.abgeordnetenwatch.de/bundestag/profile?"
getAllProfiles <-function(url, page) {
  url <- paste0(url,"page=",page)
  profile_links <- url %>% read_html() %>% 
    html_nodes(".tile__links__item+ .tile__links__item .tile__links__item__link") %>% 
    html_attr("href")
  tibble(profile_links = profile_links) 
}
pages <- c(0:59)
urls <- rep("https://www.abgeordnetenwatch.de/bundestag/profile?",
            length(pages))
plan(multiprocess)
profile_links <- future_map2_dfr(urls, pages, getAllProfiles, 
                                 .progress = TRUE)


# Get profile data --------------------------------------------------------

getAllContent <-function(link, profile) {
  currentUrl <- paste0(link,profile)
  
  read_name <- possibly(~.x %>% 
                     read_html() %>%
                     html_nodes(.,".deputy__title") %>% 
                     html_text(), 
                   NA)
  name <- read_name(currentUrl)
  read_birthyear <- possibly(~.x %>% 
                          read_html() %>%
                          html_nodes(.,".date-display-single") %>% 
                          html_text(), 
                        NA)
  birthyear <- read_birthyear(currentUrl)
  read_location <- possibly(~.x %>% 
                          read_html() %>%
                          html_nodes(.,".dl__dd:nth-child(4)") %>% 
                          html_text(), 
                        NA)
  location <- read_location(currentUrl)
  read_party <- possibly(~.x %>% 
                      read_html() %>%
                      html_nodes(.,".party-indicator") %>% 
                      html_text() %>% 
                      .[1], 
                    NA)
  party <- read_party(currentUrl)
  read_answer_ratio <- possibly(~.x %>% 
                      read_html() %>%
                      html_nodes(.,".hstats__item__label__value") %>% 
                      html_text() %>% 
                      .[1] %>% 
                        str_trim(), 
                    NA)
  answer_ratio <- read_answer_ratio(currentUrl)
  tibble(name = name, birthyear = birthyear, location = location,
         party = party, answer_ratio = answer_ratio) 
}

profiles <- profile_links$profile_links
urls <- rep("https://www.abgeordnetenwatch.de",
            length(profiles))
plan(multiprocess)
profile_content <- future_map2_dfr(urls, profiles, getAllContent, 
                                 .progress = TRUE)

