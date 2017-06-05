source('messidor/messidor_loader.r')
source('normalizer.r')

samples.count <- 10

print('Loading meta:')
messidor.columns <- messidor.columns.get()

print('Loading data:')
messidor.data <- messidor.data.read('messidor/data/messidor_features.arff')

print('Normalizing data:')
messidor.data <- normalizer.set.scale_continuous(messidor.data, messidor.columns$is_data_continuous)

indexes.num <- nrow(messidor.data)
for(sample.i in 1:samples.count) {

	print(sprintf('Generating sample %d:', sample.i))

	indexes.shuffled <- sample(indexes.num)
	indexes.shuffled.train <- indexes.shuffled[1:(indexes.num / 2)]
	indexes.shuffled.test <- indexes.shuffled[(indexes.num / 2 + 1):indexes.num]

	messidor.data.train <- messidor.data[indexes.shuffled.train, ]
	messidor.data.test <- messidor.data[indexes.shuffled.test, ]

	file.name.train <- sprintf('messidor/data/messidor_samples_%d_train.arff', sample.i)
	file.name.test <- sprintf('messidor/data/messidor_samples_%d_test.arff', sample.i)

	messidor.data.write(file.name.train, messidor.data.train)
	messidor.data.write(file.name.test, messidor.data.test)
}

print('Done')