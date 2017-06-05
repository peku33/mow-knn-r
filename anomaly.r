source('knn.r')
source('distance.r')

# Posiadając wektor odległości punktu testowego od danych normalnych oblicza współczynnik podobieństwa
# Współczynnik jest obliczany jako suma odległości od k najbliższych sąsiadów
anomaly.score.vector.k <- function(distances.vector, k) {

	# Znajdź indeksy k najbliższych danych normalnych
	normal.rows.k_indexes <- knn.vector.get_indexes(distances.vector, k)

	# Weź odległości od najbliższych danych normalnych
	normal.rows.k_distances <- distances.vector[normal.rows.k_indexes]

	# Zwróć sumę od najbliższych danych normalnych
	sum(normal.rows.k_distances)
}

# Dla podanego zbioru danych testowych oraz trenujących buduje macierz odległości
# Rozdzielenie implementacji pozwala na szybkie wykorzystanie tej samej macierzy z różnymi wartościami k
anomaly.score.data.multiple.k.matrix.build <- function(test.rows, train.rows.normal, method) {

	# Oblicz macierz odległości pomiędzy danymi testowymi a trenującymi.
	# Jeden wiersz macierzy dla jednego wektora z danych testowych
	# Kolejne kolumny macierzy to odległości od kolejnych danych trenujących
	distance.data_frame.multiple.continuous.1(
		test.rows,
		train.rows.normal,
		method
	)
}

# Dla podanej macierzy odległości oraz zbioru danych testowych i parametru k buduje wektor miar niepodobieństw.
# Każdy wektor ze zbioru danych testowych to jeden element wyjściowego wektora miar.
# Wewnętrznie użyta została metoda KNN, do której parametr k jest przekazywany
anomaly.score.data.multiple.k.matrix.by <- function(distances.matrix, test.rows, k) {

	# Zbieramy wartości współczynnika nieprawidłowości dla danych prawidłowych i nieprawidłowych
	distances <- c()

	for(test.row.id in 1:nrow(test.rows)) {

		# Z macierzy odległości wybierz wiersz odpowiadający tej danej testowej
		distances.vector <- distances.matrix[test.row.id, ]

		# Oblicz miarę nieprawidłowości dla k najbliższych sąsiadów
		distances.sum <- anomaly.score.vector.k(distances.vector, k)

		# Dodaj wynik do list
		distances <- c(distances, distances.sum)
	}

	# Zwróć listę
	distances
}

# Właściwa metoda obliczająca wektor miary niepodobieństw
# Łączy metody .build i .by
anomaly.score.data.multiple.k <- function(test.rows, train.rows.normal, method, k) {

	anomaly.score.data.multiple.k.matrix.by(
		anomaly.score.data.multiple.k.matrix.build(
			test.rows,
			train.rows.normal,
			method
		),
		test.rows,
		k
	)
}