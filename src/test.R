install.packages("readr")
install.packages("igraph")
install.packages("dplyr")
install.packages("tidyr")
install.packages("lsa")
install.packages("coop")

library("readr")
library("igraph")
library("dplyr")
library("tidyr")
library("lsa")
library("coop")


ratings_path <- "assets/dataset/ratings.dat"
movies_path <- "assets/dataset/movies.dat"
tags_path <- "assets/dataset/tags.dat"

ratings_content <- read_delim(ratings_path, delim = "::", col_names = c("userID", "movieID", "rating", "timestamp"), escape_double = FALSE, trim_ws = TRUE)
movies_content <- read_delim(movies_path, delim = "::", col_names = c("movieID", "title", "genre"), escape_double = FALSE, trim_ws = TRUE)
tags_content <- read_delim(tags_path, delim = "::", col_names = c("userID", "movieID", "tag", "timestamp"), escape_double = FALSE, trim_ws = TRUE)

movie_difference_punnish_factor = 0.5

#combined_content <- ratings_content %>% left_join(movies_content, by = "movieID")



ratings_filtered <- ratings_content %>% group_by(userID) %>% filter(n() >= 1600) %>% ungroup()
popular_movies <- ratings_content %>% group_by(movieID) %>% filter(n() >= 500) %>% pull(movieID) %>% unique()
ratings_filtered <- ratings_filtered %>% select(-timestamp) %>% filter(movieID %in% popular_movies)



user_movie <- ratings_filtered %>% pivot_wider(names_from = movieID, values_from = rating)
rating_matrix <- as.matrix(user_movie[,-1])
rownames(rating_matrix) <- user_movie$userID


print(user_movie)
print(rating_matrix)
print(edges)



n <- nrow(rating_matrix)
sim_matrix = matrix(NA, n, n)
rownames(sim_matrix) <- rownames(rating_matrix)
colnames(sim_matrix) <- rownames(rating_matrix)


# custom func cause normal cosine does include all ratings (with NA)
get_pair_cosine <- function(x, y) {
	common <- !is.na(x) & !is.na(y)
	if (sum(common) == 0) {
		return(NA)
	}

	x <- x[common]
	y <- y[common]

	result <- sum(x*y) / (sqrt(sum(x^2)) * sqrt(sum(y^2)))
	return (result)
}


# better custom func cause directly implemented with 0-5 scale
get_pair_sim_factor <- function(x, y) {
	common <- !is.na(x) & !is.na(y)

	if (sum(common) == 0) {
		return(NA)
	}

	one_or_common = !is.na(x) | !is.na(y) 
	initial_length = length(x)
	not_reviewed_length = initial_length - sum(one_or_common)

	x <- x[common]
	y <- y[common]

	n <- length(x)
	movie_difference_length = initial_length - (n + not_reviewed_length)



	result <- 0

	diff <- abs(x - y)
	sim <- 1 - diff/5
	result_norm <- mean(sim)
	

	movie_difference_norm = 1 - movie_difference_length / (movie_difference_length + n)






	result <- ((1 - movie_difference_punnish_factor) * (result_norm)) + (movie_difference_punnish_factor * movie_difference_norm)
	return (result)
}


#for (i in 1:n) {
	#for (j in i:n) {
		#sim_factor <- get_pair_sim_factor(rating_matrix[i,], rating_matrix[j,])
		#sim_matrix[i,j] = sim_factor
		#sim_matrix[j,i] = sim_factor
	#}
#}



R <- as.matrix(rating_matrix)
B <- abs(!is.na(R))
R[is.na(R)] <- 0

n_shared <- B %*% t(B)

sum_a2 <- rowSums(R^2)
dot_product <- R %*% t(R)

sum_sq_diff <- ((R^2) %*% t(B)) + (B %*% t(R^2)) - 2 * (R %*% t(R))
mean_sq_diff <- sum_sq_diff / (n_shared)
mean_sq_diff[n_shared == 0] <- NA

dist_norm <- sqrt(pmax(mean_sq_diff, 0))
sim_matrix <- 1 / (1 + dist_norm)


print(sim_matrix)


edges <- as.data.frame(as.table(sim_matrix))
colnames(edges) <- c("from", "to", "weight")

edges <- edges %>% filter(weight >= .6 & from != to)
nodes = data.frame(userID = unique(ratings_filtered$userID))



g = graph_from_data_frame(d = edges, directed = FALSE, vertices = nodes)

plot(g, 
     vertex.label = NA,
     vertex.color = "skyblue",
     vertex.size = 3,
     edge.width = E(g)$weight * 2,
     layout = layout_with_fr)



