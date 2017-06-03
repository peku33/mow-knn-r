source('kdd/kdd_loader.r')
source('distance.r')
source('knn.r')
source('cid.r')

kdd.load.data <- function(kdd.data.filename, kdd.columns) {

	kdd.data <- kdd.data.read(kdd.data.filename, kdd.columns)

	# print('Changing discrete -> continuous')
	# Ogranicz dane tylko do atrybutów ciągłych, docelowo - zamień dane na dane ciągłe
	# kdd.data.continuous <- kdd.data[, kdd.columns$is_continuous]

	# print('Normalizing data')
	kdd.data <- normalizer.set.scale_continuous(kdd.data, kdd.columns$is_continuous)

	kdd.data
}

kdd.validate.train.test.knn <- function(test.rows, train.rows, k) {

	# Dane trenujące oznaczone jako normalne służą do obliczania współczynnika nieprawidłowości
	# Wyciągamy tylko te dane trenujące, oznaczone jako normalne
	train.rows.normal <- train.rows[train.rows$label == "normal.", ]

	# Oblicz macierz odległości pomiędzy danymi testowymi a trenującymi.
	# Jeden wiersz macierzy dla jednego wektora z danych testowych
	# Kolejne kolumny macierzy to odległości od kolejnych danych trenujących
	distances.matrix <- distance.data_frame.multiple.continuous.1(test.rows, train.rows.normal)

	# Zbieramy wartości współczynnika nieprawidłowości dla danych prawidłowych i nieprawidłowych
	knn.distances.normal <- c()
	knn.distances.not_normal <- c()

	for(test.row.id in 1:nrow(test.rows)) {

		# Z macierzy odległości wybierz wiersz odpowiadający tej danej testowej
		distances.test_row.to.test_rows <- distances.matrix[test.row.id, ]

		# Weź indeksy k najbliższych danych
		distances.test_row.to.test_rows.knn.indexes <- knn.vector.get_indexes(distances.test_row.to.test_rows, k)
	
		# Oblicz miarę nieprawidłowości testowego wektora jako sumę odległości k najbliższych sąsiadów
		distances.test_row.to.test_rows.knn.distance <- sum(distances.test_row.to.test_rows[distances.test_row.to.test_rows.knn.indexes])

		if(test.rows[test.row.id, ]$label == "normal.")
			knn.distances.normal <- c(knn.distances.normal, distances.test_row.to.test_rows.knn.distance)
		else
			knn.distances.not_normal <- c(knn.distances.not_normal, distances.test_row.to.test_rows.knn.distance)
	}

	list(knn.distances.normal, knn.distances.not_normal)
}

kdd.data.test.filename <- 'kdd/data/samples/kddcup.data_sample.1'
kdd.data.train.filename <- 'kdd/data/samples/kddcup.data_sample.2'
kdd.k <- 100

print('Loading namesfile')
kdd.namesfile <- kdd.namesfile.read('kdd/data/kddcup.names')

kdd.columns <- kdd.namesfile.columns.extract(kdd.namesfile)
kdd.columns <- kdd.columns.add.label(kdd.columns)

print('Loading datafiles')
kdd.data.test <- kdd.load.data(kdd.data.test.filename, kdd.columns)
kdd.data.train <- kdd.load.data(kdd.data.train.filename, kdd.columns)

print('Running evaluation')
kdd.result <- kdd.validate.train.test.knn(kdd.data.test, kdd.data.train, kdd.k)

print('Done')