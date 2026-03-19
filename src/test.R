install.packages("readr")
install.packages("igraph")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("proxy")
install.packages("lubridate")
install.packages("gridExtra")
install.packages("ggplot2")

library("readr")
library("igraph")
library("dplyr")
library("tidyverse")
library("proxy")
library("lubridate")
library("gridExtra")
library("ggplot2")



ratings_path <- "assets/dataset/ratings.dat"
movies_path <- "assets/dataset/movies.dat"
tags_path <- "assets/dataset/tags.dat"

ratings_content <- read_delim(ratings_path, delim = "::", col_names = c("userID", "movieID", "rating", "timestamp"), escape_double = FALSE, trim_ws = TRUE)
movies_content <- read_delim(movies_path, delim = "::", col_names = c("movieID", "title", "genre"), escape_double = FALSE, trim_ws = TRUE)
tags_content <- read_delim(tags_path, delim = "::", col_names = c("userID", "movieID", "tag", "timestamp"), escape_double = FALSE, trim_ws = TRUE)

# so we can compute multiple times and get the same results
set.seed(42)
start_year <- 1995
end_year <-2009

# select top users
top_users <- ratings_content %>%
  slice_sample(n = 1000)

top_ratings <- ratings_content %>%
  filter(userID %in% top_users$userID) %>%
  mutate(year = year(as_datetime(timestamp))) %>%
  filter(year >= start_year & year <= end_year)

# make user movie data
user_movie_content <- top_ratings %>%
  select(userID, movieID, rating) %>%
  pivot_wider(names_from = movieID, values_from = rating) %>%
  column_to_rownames("userID")


#
#
# Compute distance factor for all users
#
#


total_movie_amount <- ncol(user_movie_content)

# user x user mat with rating dist as cells
user_rating_distance <- as.matrix(proxy::dist(as.matrix(user_movie_content), method = "manhattan"))

# get rid of total_movie_amount cause manhattan distance is sum of all dist / sim entries * total_movie_amount
avg_user_rating_distance <- user_rating_distance / total_movie_amount

# cause ratings diff up to 4.5 stars
user_user_norm_rating_sim <- 1 - (avg_user_rating_distance / 4.5)


#
#
# compute seen movie difference for all users
#
#


user_movie_watched <- !is.na(as.matrix(user_movie_content))
user_user_common_count <- user_movie_watched %*% t(user_movie_watched)

user_total_watched <- rowSums(user_movie_watched)
user_user_total_watched_a <- matrix(user_total_watched, nrow=nrow(user_movie_watched), ncol=nrow(user_movie_watched))
user_user_total_watched_b <- t(user_user_total_watched_a)

user_user_diff_a <- user_user_total_watched_a - user_user_common_count
user_user_diff_b <- user_user_total_watched_b - user_user_common_count

user_user_diff_count <- user_user_diff_a + user_user_diff_b
user_user_norm_movie_sim <- user_user_common_count / (user_user_diff_count/2 + user_user_common_count)
user_user_norm_movie_sim[is.nan(user_user_norm_movie_sim)] <- 0

movie_difference_punnish_factor = .5

user_sim <- (movie_difference_punnish_factor * user_user_norm_movie_sim
             + (1 - movie_difference_punnish_factor) * user_user_norm_rating_sim)


diag(user_sim) <- 0 


#
#
# compute sim graph matrix and graph
#
#


adj_matrix <- user_sim
adj_matrix[is.na(adj_matrix)] <- 0
adj_matrix[adj_matrix <= .65] <- 0

sim_graph <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE, diag = FALSE)
sim_graph <- delete_vertices(sim_graph, V(sim_graph)[degree(sim_graph) < 1])


# comm detection
communities <- cluster_louvain(sim_graph)
V(sim_graph)$community <- communities$membership
E(sim_graph)$width <- 1

node_degree <- degree(sim_graph)
V(sim_graph)$size <- 3 + (node_degree / max(node_degree) * 12)


#
#
# compute table of popular genres for all groups
#
#


user_comm_df <- data.frame(
  userID = as.numeric(names(membership(communities))),
  community = as.vector(membership(communities))
)

# get all genre group sets then count, then divide by total count, get top 3
comm_genre_table <- ratings_content %>%
  filter(userID %in% user_comm_df$userID) %>%
  inner_join(user_comm_df, by = "userID") %>%
  inner_join(movies_content, by = "movieID") %>%
  separate_rows(genre, sep = "\\|") %>%
  group_by(community, genre) %>%
  summarise(count = n(), .groups = 'drop_last') %>%
  
  mutate(percent = (count / sum(count)) * 100) %>%
  slice_max(order_by = percent, n = 3, with_ties = FALSE) %>%
  mutate(rank = row_number()) %>%
  mutate(genre_label = paste0(genre, " (", round(percent, 1), "%)")) %>%
  
  select(community, rank, genre_label) %>%
  pivot_wider(names_from = rank, values_from = genre_label) %>%
  ungroup()

grid.table(comm_genre_table, rows = NULL)



#
#
# compute relative impact of blockbusters vs niche movies as table
#
#


movie_edge_threshold = 0.3

movie_popularity <- ratings_content %>%
  group_by(movieID) %>%
  summarise(vote_count = n()) %>%
  arrange(desc(vote_count))

n_movies <- nrow(movie_popularity)
top_threshold <- round(n_movies * movie_edge_threshold)

movie_popularity <- movie_popularity %>%
  mutate(category = case_when(
    row_number() <= top_threshold ~ "Popular",
    row_number() > (n_movies - top_threshold) ~ "Niche",
    TRUE ~ "Average"
  ))

user_movie_comm_pop <- ratings_content %>%
  filter(userID %in% user_comm_df$userID) %>%
  inner_join(user_comm_df, by = "userID") %>%
  inner_join(movie_popularity, by = "movieID")

comm_pop_analysis <- user_movie_comm_pop %>%
  group_by(community, category) %>%
  summarise(count = n(), .groups = 'drop_last') %>%
  mutate(percentage = count / sum(count) * 100)


comm_comparison_table <- comm_pop_analysis %>%
  select(community, category, percentage) %>%
  pivot_wider(names_from = category, values_from = percentage, values_fill = 0) %>%
  select(community, Popular, Average, Niche) %>%
  mutate(across(where(is.numeric), \(x) round(x, 2)))

grid.table(comm_comparison_table, rows = NULL)


#
#
# plot graph
#
#


plot(sim_graph, 
     layout = layout_with_fr(sim_graph),
     vertex.label = NA, 
     vertex.color = V(sim_graph)$community, 
     edge.color = adjustcolor("grey", alpha.f = 0.8),
     )



#------------------------------ GENRES ------------------------------------
# no longer needed



user_genre_data <- top_ratings  %>%
	left_join(movies_content, by = "movieID") %>% 
	separate_rows(genre, sep="\\|") %>%
	group_by(userID, genre) %>%
	summarise(count = n(), .groups = "drop") %>%
	group_by(userID) %>%
  mutate(genreWeights = count / sum(count)) %>%
  ungroup()

user_genre_content <- user_genre_data %>%
	select(userID, genre, genreWeights) %>%
	pivot_wider(names_from = genre, values_from = genreWeights, values_fill = 0) %>%
	column_to_rownames("userID")

user_genre_sim <- cosine(as.matrix(t(user_genre_content)))


genre_adj_matrix <- user_genre_sim
genre_adj_matrix[genre_adj_matrix <= .997] <- 0

genre_graph <- graph_from_adjacency_matrix(genre_adj_matrix, mode = "undirected", weighted = TRUE, diag = FALSE)
genre_graph <- delete_vertices(genre_graph, V(genre_graph)[degree(genre_graph) <= 2])

genre_communities <- cluster_louvain(genre_graph)
V(genre_graph)$community <- genre_communities$membership

genre_node_degree = degree(genre_graph)
V(genre_graph)$size <- 3 + (genre_node_degree / max(genre_node_degree) * 12)

plot(genre_graph,
     layout = layout_with_fr(genre_graph),
     vertex.label = NA,
     vertex.color = V(genre_graph)$community,
     edge.width = 1,
     edge.color = adjustcolor("grey", alpha.f = 0.3),
     main = "Genre-basiertes User-Netzwerk")



#------------------------------ Timeline ------------------------------------

ratings_time <- ratings_content %>%
  mutate(year = year(as_datetime(timestamp)))

# from 1995 till 2009
ratings_time <- ratings_time %>%
  mutate(period = case_when(
    year < 2000 ~ "Phase 1: Early",
    year < 2005 ~ "Phase 2: Mid",
    TRUE ~ "Phase 3: Late"
  ))

time_genre_analysis <- ratings_time %>%
  filter(userID %in% user_comm_df$userID) %>%
  inner_join(user_comm_df, by = "userID") %>%
  inner_join(movies_content, by = "movieID") %>%
  separate_rows(genre, sep = "\\|") %>%
  group_by(period, community, genre) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(period, community) %>%
  mutate(share = count / sum(count) * 100) %>%
  slice_max(order_by = share, n = 1, with_ties = FALSE)

time_evolution_table <- time_genre_analysis %>%
  mutate(genre_label = paste0(genre, " (", round(share, 1), "%)")) %>%
  select(community, period, genre_label) %>%
  pivot_wider(names_from = period, values_from = genre_label, values_fill = "No Data") %>%
  arrange(community)

grid.table(time_evolution_table, rows = NULL)



#
#
# Graph for amount of niche reviews over years
#
#



timeline_comparison <- ratings_time %>%
  filter(userID %in% user_comm_df$userID) %>%
  filter(year != 2009) %>%
  inner_join(movie_popularity, by = "movieID") %>%
  filter(category %in% c("Popular","Average", "Niche")) %>%
  group_by(year, category) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(year) %>%

  mutate(percentage = (count / sum(count)) * 100) 

ggplot(timeline_comparison, aes(x = year, y = percentage, color = category, group = category)) +
  geom_line(size = 1.2) + 
  geom_point(size = 2) +
  
  geom_smooth(method = "lm", linetype = "dashed", se = FALSE) +
  scale_color_manual(values = c("Popular" = "#f1c40f", "Niche" = "#2980b9", "Average" = "#666666")) +
  
  labs(title = "Mainstream vs. Nische im Zeitverlauf",
       subtitle = "Vergleich der oberen 30% (Popular) und unteren 30% (Niche) Filme",
       x = "Jahr",
       y = "Anteil an den jährlichen Bewertungen (%)",
       color = "Film-Kategorie") +
  
  theme_minimal() +
  theme(legend.position = "bottom")


#
#
# Graph for review amount over years
#
#



review_amount_year_comparison <- ratings_time %>%
  filter(userID %in% user_comm_df$userID) %>%
  group_by(year) %>%
  summarise(amount = n(), .groups = 'drop')

ggplot(review_amount_year_comparison, aes(x = year, y = amount)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8)
  labs(title = "Aktivität der Community-User über die Zeit",
       subtitle = "Gesamtanzahl der abgegebenen Reviews pro Jahr",
       x = "Jahr",
       y = "Anzahl der Bewertungen") +
  theme_minimal()

