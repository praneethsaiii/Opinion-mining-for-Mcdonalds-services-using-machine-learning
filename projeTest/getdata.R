library(rvest)
for(i in 1:10)
{
url <- paste0("https://www.tripadvisor.in/Restaurant_Review-g60763-d878353-Reviews-or",i,"0-McDonald_s-New_York_City_New_York.html#REVIEWS")

reviews <- url %>%
  read_html() %>%
  html_nodes("#REVIEWS .innerBubble")

#id <- reviews %>%
 #html_node(".quote a") %>%
 # html_attr("id")

#quote <- reviews %>%
# html_node(".quote span") %>%
 # html_text()

review <- reviews %>%
  html_node(".entry .partial_entry") %>%
  html_text()
df<-data.frame(review, stringsAsFactors = FALSE)
#data.frame(id, quote, review, stringsAsFactors = FALSE) %>% View()
write.table(df,file="tripnewyork1.txt",append=TRUE,row.names = FALSE)
}


