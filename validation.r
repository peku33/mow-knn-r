source('anomaly.r')
library('pROC')
library('e1071')

################################################################################
# KNN
################################################################################

# Funkcja wykonująca walidację krzyżową podanych zbiorów danych
# data.sets - lista zbiorów danych
# Pozostałe jak w @validation.pair.k
validation.cross.k <- function(data.sets, data.columns.use, data.label.column, data.label.value.normal, method, k.values) {

	data.sets.num <- length(data.sets)

	# Dla każdej pary zbiorów...
	for(index.train in 1:data.sets.num) {
		for(index.test in 1:data.sets.num) {
			print(sprintf(' Train set = %d, Test set = %d', index.train, index.test))

			# Tytuł wykresu
			name <- sprintf('Training set = %d, Testing set = %d', index.train, index.test)

			# Wykonaj walidację dla pary
			validation.pair.k(
				name,
				data.sets[[index.train]],
				data.sets[[index.test]],
				data.columns.use,
				data.label.column,
				data.label.value.normal,
				method,
				k.values
			)
		}
	}
}

# Funkcja wykonująca walidację i generowanie krzywej ROC dla pary zbiorów danych
# name - tekstowa nazwa wykresu
# data.set.train - trenujący zbiór danych
# data.set.test - testowy zbiór danych
# data.columns.use - wektor wykorzystania atrybutów ze zbioru danych
# data.label.column - etykieta kolumny zawierającej klasę
# data.label.value.normal - wartość kolumny klasy, klasyfikującej jako normalna
# k.values - wektor wartości parametru k do przetestowania
validation.pair.k <- function(name, data.set.train, data.set.test, data.columns.use, data.label.column, data.label.value.normal, method, k.values) {

	# Oblicz macierz odległości pomiędzy wszytkimi parami "wektor testowy - wektory trenujące"
	# Użyj tylko podanych kolumn
	# Jako dane trenujące użyj tylko danych 'normalnych'
	matrix <- anomaly.score.data.multiple.k.matrix.build(
		data.set.test[, data.columns.use],
		data.set.train[data.set.train[data.label.column] == data.label.value.normal, data.columns.use],
		method
	)

	# Oblicz metrykę niepodobieństwa korzystając z KNN
	for(k in k.values) {

		# Oblicz metrykę k
		distances <- anomaly.score.data.multiple.k.matrix.by(matrix, data.set.test, k)

		# Stwórz krzywą ROC
		roc <- roc(
			factor(data.set.test[data.label.column] != data.label.value.normal, ordered = TRUE),
			factor(distances, ordered = TRUE)
		)

		name.k <- sprintf('%s, K = %d', name, k)

		# Wykres
		plot.roc(
			roc,
			main = name.k,
			print.auc = TRUE
		)
	}
}

################################################################################
# SVM
################################################################################

# Poniższe metody posiadają analogiczną składnię, z wyjątkiem braku konieczności dostarczenia niektórych parametrów

validation.cross.svm <- function(data.sets, data.columns.use, data.label.column, data.label.value.normal, supervised) {

	data.sets.num <- length(data.sets)

	# Dla każdej pary zbiorów...
	for(index.train in 1:data.sets.num) {
		for(index.test in 1:data.sets.num) {
			print(sprintf(' Train set = %d, Test set = %d', index.train, index.test))

			# Tytuł wykresu
			name <- sprintf('Training set = %d, Testing set = %d', index.train, index.test)

			# Wykonaj walidację dla pary
			validation.pair.svm(
				name,
				data.sets[[index.train]],
				data.sets[[index.test]],
				data.columns.use,
				data.label.column,
				data.label.value.normal,
				supervised
			)
		}
	}
}

validation.pair.svm <- function(name, data.set.train, data.set.test, data.columns.use, data.label.column, data.label.value.normal, supervised) {

	# Wyselekcjonuj dane trenujące
	if(!supervised) {
		data.set.train <- data.set.train[data.set.train[data.label.column] == data.label.value.normal, ]
	}

	# Stwórz model klasyfikacji
	svm.model <- svm(
		x = data.set.train[, data.columns.use],
		y = data.set.train[data.label.column] == data.label.value.normal,
		scale = FALSE,
		type = 'one-classification'
	)

	# Przewidź wartości
	svm.predictions <- predict(svm.model, data.set.test[, data.columns.use], decision.values = TRUE)
	distances <- attr(svm.predictions, 'decision.values')

	roc <- roc(
		factor(data.set.test[data.label.column] != data.label.value.normal, ordered = TRUE),
		factor(distances, ordered = TRUE)
	)

	plot.roc(
		roc,
		main = name,
		print.auc = TRUE
	)
}