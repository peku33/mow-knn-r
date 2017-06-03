source('kdd/kdd_loader.r')

kdd.namesfile <- kdd.namesfile.read('kdd/data/kddcup.names')

kdd.columns <- kdd.namesfile.columns.extract(kdd.namesfile)
kdd.columns <- kdd.columns.add.label(kdd.columns)

kdd.data <- kdd.data.read('kdd/data/kddcup.data_labeled', kdd.columns)

samples.count <- 10
sample.nrow <- 10000

for(sample.i in 1:samples.count) {

	sample.file.name <- sprintf('kdd/data/samples/kddcup.data_sample.%d', sample.i)
	sample.data <- kdd.data[sample(nrow(kdd.data), sample.nrow), ]

	print(sample.file.name)
	kdd.data.write(sample.file.name, sample.data)
}