source('knn.r')

# Posiadając macierz odległości punktu testowego od danych normalnych oblicza współczynnik podobieństwa
# Współczynnik jest obliczany jako suma odległości od k najbliższych sąsiadów
anomaly.score.single.k.dist <- function(distances.vector, k) {

	# Znajdź indeksy k najbliższych danych normalnych
	normal.rows.k_indexes <- knn.vector.get_indexes(distances.vector, k)

	# Weź odległości od najbliższych danych normalnych
	normal.rows.k_distances <- distances.vector[normal.rows.k_indexes]

	# Zwróć sumę od najbliższych danych normalnych
	sum(normal.rows.k_distances)
}

# J/w, jednak przyjmując na wejściu dane, a nie wektor odległości
# Wewnętrznie wykonuje obliczenie wektora odległości
anomaly.score.single.k <- function(test.row, normal.rows, k) {

	# Oblicz odległości od poszczególnych danych normalnych
	distances.vector <- distance.data_frame.single.continuous.2(test.row, normal.rows)

	# Zwróć wynik
	anomaly.score.single.k.dist(distances.vector)

}