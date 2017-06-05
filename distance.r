source('normalizer.r')

# Tworzy wektor odległości pomiędzy wierszem test.row a wierszami train.rows
# Zakłada, że wszystkie atrybuty są ciągłe i znormalizowane
# Do obliczenia odległości wykorzystuje wewnętrznie funkcję dist
# Zwraca wektor odległości o rozmiarze nrow(train.rows)

# 10k wierszy:
# 	użytkownik     system   upłynęło
# 	    12.474      0.257     12.732
#
# 311k wierszy:
# 	BŁĄD: nie można przydzielić wektora o rozmiarze 360.4 Gb
distance.data_frame.single.continuous.1 <- function(test.row, train.rows, method) {

	# Pierwsza wersja
	#data.combined <- rbind(test.row, train.rows)
	#distances.matrix <- dist(data.combined)
	#distances.vector <- distances.matrix[1:nrow(train.rows)]
	#distances.vector

	# Można, bez straty wydajnośći wykorzystać wersję dla wielu parametrów
	distance.data_frame.multiple.continuous.1(test.row, train.rows, method)[1, ]
}

# 10K wierszy:
# 	użytkownik     system   upłynęło
#  	   20.580      0.000     20.579
#
# 311k wierszy:
# użytkownik     system   upłynęło
#    676.116      0.609    676.679
distance.data_frame.single.continuous.2 <- function(test.row, train.rows, method) {
	
	distances.frame <- apply(train.rows, 1, function(train.row) {
		dist(rbind(test.row, train.row), method = method)[1]
	})

	distances.vector <- as.vector(distances.frame)
	distances.vector
}

# Dla podanego obiektu dist zwraca indeks punktu o współrzędnych i,j
# do - obiekt dist
# n - rozmiar zwrócony z funkcji n <- attr(do, "Size")
# i, j - indeksy, gdzie i < j ≤ n
dist.index.n.i.j <- function(n, i, j) {
	n*(i-1) - i*(i-1)/2 + j-i
}

# Korzystając z pojedynczego obliczenia macierzy odległości zwraca wektory odległości pomiędzy wierszami z test.rows, a wierszami z train.rows
distance.data_frame.multiple.continuous.1 <- function(test.rows, train.rows, method) {

	# Połącz dane testowe i trenujące w jedną macierz
	data.combined <- rbind(test.rows, train.rows)

	# Oblicz odległości pomiędzy elementami macierzy
	# To najszybsza opcja, pomimo tego, że w tle liczy wszystkie odległości
	distances.matrix <- dist(data.combined, method = method)
	distances.matrix.size <- attr(distances.matrix, "Size")

	# Dla każdego wiersza z danych testowych wyrwij wektor odległości od danych trenujących
	# Są to wartości w kolumnie i = test.row.id, wszystkie wiersze (od nrow(test.rows) + 1 do nrow(test.rows) + nrow(train.rows) + 1)
	test.rows.distances <- c()
	for(test.row.id in 1:nrow(test.rows)) {
		
		dist.index.start <- dist.index.n.i.j(distances.matrix.size, test.row.id, 1 + nrow(test.rows))
		dist.index.end <- dist.index.n.i.j(distances.matrix.size, test.row.id, 1 + nrow(test.rows) + nrow(train.rows)) - 1

		test.rows.distances <- c(test.rows.distances, distances.matrix[dist.index.start:dist.index.end])
	}

	# Stwórz wynikową macierz
	matrix(test.rows.distances, nrow = nrow(test.rows), byrow = TRUE)
}

# Poprzednie wersje
# Deprecated
distance.multiple.mixed_as_continuous <- function(test.row, train.rows, columns.continuous, columns.discrete, discrete.value.same, discrete.value.different) {

	result.distances <- c()

	for(i in 1:nrow(train.rows)) {
		
		train.row <- train.rows[i,]

		normalized.list <- normalizer.two.mixed_as_continuous(test.row, train.row, columns.continuous, columns.discrete, discrete.value.same, discrete.value.different)
		normalized.matrix <- matrix(c(normalized.list[[1]], normalized.list[[2]]), nrow = 2, byrow = TRUE)
		distance <- dist(normalized.matrix)

		result.distances <- c(result.distances, distance)
	}

	result.distances
}