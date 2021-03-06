source('messidor/messidor_loader.r')
source('validation.r')

messidor.data.pairs.num <- 10
messidor.k.values <- c(1, 2, 5, 10, 25, 50, 100, 150, 200, 250, 275)

print('Loading meta:')
messidor.columns <- messidor.columns.get()
messidor.columns.use <- messidor.columns$is_data_continuous
#messidor.columns.use <- c(TRUE, TRUE, rep(FALSE, 14), TRUE, TRUE, TRUE, FALSE)
#messidor.columns.use <- c(FALSE, FALSE, rep(TRUE, 14), FALSE, FALSE, FALSE, FALSE)

for(messidor.data.i in 1:messidor.data.pairs.num) {

	print(sprintf('Running data pair %d', messidor.data.i))

	file.name.train <- sprintf('messidor/data/messidor_samples_%d_train.arff', messidor.data.i)
	file.name.test <- sprintf('messidor/data/messidor_samples_%d_test.arff', messidor.data.i)

	messidor.data.set.train <- messidor.data.read(file.name.train)
	messidor.data.set.test <- messidor.data.read(file.name.test)

	validation.pair.svm(
		sprintf('Data set pair = %d', messidor.data.i),
		messidor.data.set.train,
		messidor.data.set.test,
		messidor.columns.use,
		'Class',
		'1',
		#'maximum',
		#messidor.k.values
		FALSE
	)
}

dev.off()