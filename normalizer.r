# Moduł odpowiedzialny za normalizację formatu danych
# Główna funkcjonalność dotyczy zamiany atrybutów dyskretnych na ciągłe

# Normalizuje kolumny ciągłe podanego zbioru danych z wykorzystaniem scale
# Zwraca zbiór danych, w którym ciągłe kolumny są znormalizowane
normalizer.set.scale_continuous <- function(data, columns.continuous) {
	data.copy <- data
	data[, columns.continuous] <- scale(data[, columns.continuous])
	as.data.frame(data)
}

# Funkcja służąca do zamiany wartości dyskretnych na ciągłe, umożliwiające obliczenie odległości eudklidesowej.
# Przyjmuje na wejściu dwa wektory atrybutów dyskretnych, dla których wykonuje porównanie ==
# Wszystkim atrybutom dyskretnym z row1 przypisuje wartość ciągłą value.same
# Równym atrybutom z row2 przypisuje wartość value.same a różnym value.different
# Wynikiem jest dwuelementowa lista przetworzonych wektorów atrybutów
normalizer.two.discrete_as_continuous <- function(row1, row2, value.same, value.different) {

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

# Funkcja służąca do zamiany dwóch wektorów atrybutów mieszanej postaci, na dwa wektory atrybutów ciągłych
# Atrybuty ciągłe przenoszone są bez zmian
# Atrybuty dyskretne są zamieniane na ciągłe funkcją normalizer.two.discrete_as_continuous
# Kolejność atrybutów nie jest zachowana (atrybuty dyskretne są przenoszone na koniec)
normalizer.two.mixed_as_continuous <- function(row1, row2, columns.continuous, columns.discrete, discrete.value.same, discrete.value.different) {

	row1.continuous <- row1[1, columns.continuous]
	row1.discrete <- row1[1, columns.discrete]

	row2.continuous <- row2[1, columns.continuous]
	row2.discrete <- row2[1, columns.discrete]

	rows.discrete_as_continuous <- normalizer.two.discrete_as_continuous(row1.discrete, row2.discrete, discrete.value.same, discrete.value.different)
	row1.discrete_as_continuous <- rows.discrete_as_continuous[[1]]
	row2.discrete_as_continuous <- rows.discrete_as_continuous[[2]]

	row1.continuous_all <- c(row1.continuous, row1.discrete_as_continuous)
	row2.continuous_all <- c(row2.continuous, row2.discrete_as_continuous)

	row1.continuous_all <- unname(unlist(row1.continuous_all))
	row2.continuous_all <- unname(unlist(row2.continuous_all))

	list(row1.continuous_all, row2.continuous_all)
}