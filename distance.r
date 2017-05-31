# Funkcja służąca do zamiany wartości dyskretnych na ciągłe, umożliwiające obliczenie odległości eudklidesowej.
# Przyjmuje na wejściu dwa wektory atrybutów dyskretnych, dla których wykonuje porównanie ==
# Wszystkim atrybutom dyskretnym z row1 przypisuje wartość ciągłą value.same
# Równym atrybutom z row2 przypisuje wartość value.same a różnym value.different
# Wynikiem jest dwuelementowa lista przetworzonych wektorów atrybutów
distance.discrete_as_continuous <- function(row1, row2, value.same, value.different) {

	# Znajdujemy, które atrybuty dyskretne mają różne wartości
	# Wynikiem tej operacji jest wektor bool'i (TRUE dla równych, FALSE dla różnych)
	row1_row2.discrete.diff <- row1 %in% row2

	# Pierwszy wiersz wartości dyskretnych zamieniamy na value.same
	row1.discrete_as_continuous <- rep(value.same, length(row1_row2.discrete.diff))
	# W drugim wierszu uzupełniamy takie same wartości wartoscią value.same, a różne value.different
	row2.discrete_as_continuous <- sapply(row1_row2.discrete.diff, function(is_same) if(is_same) value.same else value.different)

	# Wynik w postaci listy
	list(row1.discrete_as_continuous, row2.discrete_as_continuous)
}

# 
distance.mixed_as_continuous_matrix <- function(row1.continuous, row1.discrete, row2.continuous, row2.discrete, discrete.value.same, discrete.value.different) {

	# Zamieniamy atrybuty dyskretne na ciągłe
	r <- distance.discrete_as_continuous(row1.discrete, row2.discrete, discrete.value.same, discrete.value.different)
	row1.discrete_as_continuous <- r[[1]]
	row2.discrete_as_continuous <- r[[2]]

	# Składamy do kupy już istniejące atrybuty ciągłe z nowo powstałymi atrybutami ciągłymi
	row1.continuous_all <- c(t(row1.continuous), row1.discrete_as_continuous)
	row2.continuous_all <- c(t(row2.continuous), row2.discrete_as_continuous)

	matrix(c(row1.continuous_all, row2.continuous_all), nrow = 2, byrow = TRUE)
}

distance.continuous.euclidean <- function(row1, row2) {
	dist(
		matrix(
			c(row1, row2),
			nrow = 2,
			byrow = TRUE
		)
	)[1]
}