library(RedditExtractoR)
library(inspectdf)
library(tidyverse)
library(lubridate)
library(rvest)
library(xml2)

# REDDIT SCRAPE FUNCTIONS AND PURRR LOOP THROUGH THINGS

reddit_links <- reddit_urls(
  search_terms   = "donald",
  page_threshold = 1
) %>%
  mutate(date = dmy(date))

show_plot(inspect_types(reddit_links))
show_plot(inspect_na(reddit_links)) # Good solid dataframe here
show_plot(inspect_cat(reddit_links)) # Politics dataframe shows up a lot

reddit_thread <- reddit_content(reddit_links$URL[1])
show_plot(inspect_types(reddit_thread))
show_plot(inspect_na(reddit_thread)) # Again no NA's
show_plot(inspect_cat(reddit_thread)) # Looks like a few users are commenting more than once on posts, not sure what structure variable is supposed to be

graph_object <- construct_graph(reddit_content(reddit_thread$URL[1])) # Comment network, basically a decision tree


# target_urls <- reddit_urls(search_terms="cats", subreddit="Art", cn_threshold=50) # isolate some URLs
# target_df <- target_urls %>% filter(num_comments==min(target_urls$num_comments)) %$% URL %>% reddit_content # get the contents of a small thread
# network_list <- target_df %>% user_network(include_author=FALSE, agg=TRUE) # extract the network

getcontent <- reddit_urls(
  search_terms = "apple", # Search term
  page_threshold = 10000, # Page threshold
  cn_threshold = 10#, # Comment threshold for number of comments that need to be in thread for scrape
  # subreddit = "pics"
  # regex_filter = "i\.redd\.it"
)

reddit_url_df <- getcontent %>% select(URL) %>% as_vector() %>% toL()

# img_sites <- c("i.redd.it|imgur|giphy")


reddit_scrape <- function(url, xpath = '//*[@id="media-preview-kw9fc7"]/div/a/img') {
  url <- url
  images <- url %>%
    read_html() %>%
    html_nodes(xpath = xpath) %>%
    html_attr('src')
}

image <- reddit_scrape(url = "http://www.reddit.com/r/memes/comments/kw9fc7/laughs_in_apple/")

content <- getcontent %>%
  select(URL) %>%
  as_vector() %>%
  read_html() %>%
  html_nodes(xpath = '') %>%
  html_attr('src')

  # filter(str_detect(link, ))

# So basically get_reddit to get links then filter?




