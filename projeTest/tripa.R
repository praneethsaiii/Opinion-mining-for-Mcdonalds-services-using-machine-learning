library(rvest)

url <- "https://www.tripadvisor.in/Restaurant_Review-g60763-d878353-Reviews-or10-McDonald_s-New_York_City_New_York.html#REVIEWS"

reviews <- url %>%
  read_html() %>%
  html_nodes("#REVIEWS .innerBubble")

id <- reviews %>%
  html_node(".quote a") %>%
  html_attr("id")

quote <- reviews %>%
  html_node(".quote span") %>%
  html_text()


review <- reviews %>%
  html_node(".entry .partial_entry") %>%
  html_text()

data.frame(id, quote, review, stringsAsFactors = FALSE) %>% View()

write.csv(data.frame(id, quote, review, stringsAsFactors = FALSE),file="F:\\r\\data.csv")

