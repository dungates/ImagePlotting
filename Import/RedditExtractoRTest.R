library(RedditExtractoR)
library(inspectdf)

reddit_links <- reddit_urls(
  search_terms   = "donald",
  page_threshold = 1
)
show_plot(inspect_types(reddit_links))
show_plot(inspect_na(reddit_links)) # Good solid dataframe here
show_plot(inspect_cat(reddit_links)) # Politics dataframe shows up a lot

reddit_thread <- reddit_content(reddit_links$URL[1])
show_plot(inspect_types(reddit_thread))
show_plot(inspect_na(reddit_thread)) # Again no NA's
show_plot(inspect_cat(reddit_thread)) # Looks like a few users are commenting more than once on posts, not sure what structure variable is supposed to be

graph_object <- construct_graph(reddit_content(reddit_thread$URL[1]))


# target_urls <- reddit_urls(search_terms="cats", subreddit="Art", cn_threshold=50) # isolate some URLs
# target_df <- target_urls %>% filter(num_comments==min(target_urls$num_comments)) %$% URL %>% reddit_content # get the contents of a small thread
# network_list <- target_df %>% user_network(include_author=FALSE, agg=TRUE) # extract the network

