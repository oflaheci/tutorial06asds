#proof no drama

movies %>%
  filter(grepl("Warner|WARNER", studio) & genre == "drama")