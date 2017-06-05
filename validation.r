source('anomaly.r')
library('pROC')

# Funkcja wykonująca walidację krzyżową podanych zbiorów danych
# data.sets - lista zbiorów danych
# data.columns.use - wektor wykorzystania atrybutów ze zbioru danych
# data.label.column - etykieta kolumny zawierającej klasę
# data.label.value.normal - wartość kolumny klasy, klasyfikującej jako normalna
# k.values - wektor wartości parametru k do przetestowania
validation.cross.k <- function(data.sets, data.columns.use, data.label.column, data.label.value.normal, k.values) {

	data.sets.num <- length(data.sets)

	# Dla każdej pary zbiorów...
	for(index.train in 1:(data.sets.num-1)) {
		for(index.test in (index.train+1):data.sets.num) {

			print(sprintf(' Train set = %d, Test set = %d', index.train, index.test))

			# Oblicz macierz odległości pomiędzy wszytkimi parami "wektor testowy - wektory trenujące"
			# Użyj tylko podanych kolumn
			# Jako dane trenujące użyj tylko danych 'normalnych'
			matrix <- anomaly.score.data.multiple.k.matrix.build(
				data.sets[[index.test]][, data.columns.use],
				data.sets[[index.train]][data.sets[[index.train]][data.label.column] == data.label.value.normal, data.columns.use]
			)

			# Oblicz metrykę niepodobieństwa korzystając z KNN
			for(k in k.values) {

				print(sprintf('  k = %d', k))

				# Oblicz metrykę k
				distances <- anomaly.score.data.multiple.k.matrix.by(matrix, data.sets[[index.test]], k)

				# Stwórz krzywą ROC
				roc <- roc(
					factor(data.sets[[index.test]][data.label.column] != data.label.value.normal, ordered = TRUE),
					factor(distances, ordered = TRUE)
				)

				# Tytuł wykresu
				name <- sprintf('Training set = %d, Testing set = %d, K = %d', index.train, index.test, k)

				# Wykres
				plot.roc(
					roc,
					main = name,
					print.auc = TRUE
				)
			}
		}
	}
}