library(rvest)
for(i in 1:29)
{
url <- paste0("https://www.tripadvisor.com/Restaurant_Review-g34515-d3774679-Reviews-or",i,"0-McDonald_s-Orlando_Florida.html#REVIEWS")
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
write.table(df,file="TripAdvisor\\triplasand.txt",append=TRUE,row.names = FALSE)

}


