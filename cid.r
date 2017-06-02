# Dla data frame `data` zwraca liczbę wierszy, dla których kolumna `data.column.distance` ma wartość mniejszą lub równą d
cid.count <- function(data, data.column.distance, d) {
	length(which(data[, data.column.distance] <= d))
}