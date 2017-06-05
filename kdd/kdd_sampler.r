source('kdd/kdd_loader.r')
source('normalizer.r')

samples.count <- 10
sample.nrow <- 5000

print('Loading meta:')
kdd.namesfile <- kdd.namesfile.read('kdd/data/kddcup.names')
kdd.columns <- kdd.namesfile.columns.extract(kdd.namesfile)
kdd.columns <- kdd.columns.add.label(kdd.columns)

print('Loading data:')
kdd.data <- kdd.data.read('kdd/data/kddcup.data_labeled', kdd.columns)

print('Generating indexes:')
kdd.data.size <- nrow(kdd.data)
kdd.data.indexes <- split(sample(kdd.data.size, size = samples.count * sample.nrow), 1:samples.count)

for(sample.i in 1:samples.count) {

	print(sprintf('Generating sample %d:', sample.i))

	sample.data <- kdd.data[kdd.data.indexes[[sample.i]], ]
	sample.data <- normalizer.set.scale_continuous(sample.data, kdd.columns$is_data_continuous)

	sample.file.name <- sprintf('kdd/data/samples/kddcup.data_sample.%d', sample.i)
	kdd.data.write(sample.file.name, sample.data)
}