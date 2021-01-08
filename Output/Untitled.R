library(RedditExtractoR)

reddit_links <- reddit_urls(
  search_terms   = "donald",
  page_threshold = 1
)

reddit_thread <- reddit_content(reddit_links$URL[1])
str(reddit_thread)