# Zwraca sumę dystansów dla k najbliższych punktów
# Wejściowy data frame `data` jest sortowany na podstawie kolumny `data.column.distance`
# Następnie zwracana jest suma odległości tych k punktów
knn.data_frame.sum_distances <- function(data, data.column.distance, k) {

	data.ordered.keys.k <- order(data[data.column.distance])[1:k]
	data.ordered.distances.k <- data[data.ordered.keys.k, data.column.distance]
	sum(data.ordered.distances.k)

}

knn.vector.get_indexes <- function(data.distances, k) {
	order(data.distances)[1:k]
}



# Metody legacy
# Deprecated
knn.data_frame <- function(data, data.column.distance, data.column.data, k) {

	# Posortuj wynik po kolumnie z odległością
	# Weź k pierwszych indeksów
	order_k <- order(data[data.column.distance])[1:k]

	# Weź k pierwszyc
	data_k <- data[order_k, data.column.data]

	data_k
}

knn.data_frame.continuous <- function(data, data.column.distance, data.column.data, k) {

	data_k <- knn.data_frame(data, data.column.distance, data.column.data, k)

	value_average <- sum(data_k) / k
	value_average
}

knn.data_frame.discrete <- function(data, data.column.distance, data.column.data, k) {

	data_k <- knn.data_frame(data, data.column.distance, data.column.data, k)
	table(data_k)
}

knn.data_frame.discrete.ordered <- function(data, data.column.distance, data.column.data, k) {

	data_k <- knn.data_frame.discrete(data, data.column.distance, data.column.data, k)
	data_k[order(data_k, decreasing = TRUE)]

}