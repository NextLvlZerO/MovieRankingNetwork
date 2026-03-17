install.packages("readr")
install.packages("igraph")
install.packages("dplyr")
install.packages("tidyr")
install.packages("tibble")
install.packages("lsa")
install.packages("coop")
install.packages("parallelDist")
install.packages("proxy")

library("readr")
library("igraph")
library("dplyr")
library("tidyr")
library("tibble")
library("lsa")
library("coop")
library("parallelDist")
library("proxy")


ratings_path <- "assets/dataset/ratings.dat"
movies_path <- "assets/dataset/movies.dat"
tags_path <- "assets/dataset/tags.dat"

ratings_content <- read_delim(ratings_path, delim = "::", col_names = c("userID", "movieID", "rating", "timestamp"), escape_double = FALSE, trim_ws = TRUE)
movies_content <- read_delim(movies_path, delim = "::", col_names = c("movieID", "title", "genre"), escape_double = FALSE, trim_ws = TRUE)
tags_content <- read_delim(tags_path, delim = "::", col_names = c("userID", "movieID", "tag", "timestamp"), escape_double = FALSE, trim_ws = TRUE)


# select top users
top_users <- ratings_content %>%
  group_by(userID) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1:1000) 

top_ratings <- ratings_content %>%
  filter(userID %in% top_users$userID)

# make user movie data
user_movie_content <- top_ratings %>%
  select(userID, movieID, rating) %>%
  pivot_wider(names_from = movieID, values_from = rating) %>%
  column_to_rownames("userID")


#
# Compute distance factor for all users
#


total_movie_amount <- ncol(user_movie_content)

# user x user mat with rating dist as cells
user_rating_distance <- as.matrix(proxy::dist(as.matrix(user_movie_content), method = "manhattan"))

# get rid of total_movie_amount cause manhattan distance is sum of all dist / sim entries * total_movie_amount
avg_user_rating_distance <- user_rating_distance / total_movie_amount

# cause ratings diff up to 4.5 stars
user_user_norm_rating_sim <- 1 - (avg_user_rating_distance / 4.5)


#
# compute seen movie difference for all users
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
# compute sim graph matrix and graph
#

adj_matrix <- user_sim
adj_matrix[adj_matrix <= .7] <- 0

sim_graph <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE, diag = FALSE)
sim_graph <- delete_vertices(sim_graph, V(sim_graph)[degree(sim_graph) <= 2])


# comm detection
communities <- cluster_louvain(sim_graph)
V(sim_graph)$community <- communities$membership
E(sim_graph)$width <- 1

node_degree <- degree(sim_graph)
V(sim_graph)$size <- 3 + (node_degree / max(node_degree) * 12)


# plot graph
plot(sim_graph, 
     layout = layout_with_fr(sim_graph),
     vertex.label = NA, 
     vertex.color = V(sim_graph)$community, 
     edge.color = adjustcolor("grey", alpha.f = 0.5),
     main = "Gewichtetes User-Netzwerk (Dicke = Ähnlichkeit)")



#------------------------------ GENRES ------------------------------------



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
# To get timeline just use 2 variables one for start one for end and then compute normal graph just filter the ratings for this shit
