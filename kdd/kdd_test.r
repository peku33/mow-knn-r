source('kdd/kdd_loader.r')
source('validation.r')

kdd.load.data <- function(kdd.data.filename, kdd.columns) {

	kdd.data <- kdd.data.read(kdd.data.filename, kdd.columns)

	# print('Changing discrete -> continuous')

	# print('Normalizing data')
	kdd.data <- normalizer.set.scale_continuous(kdd.data, kdd.columns$is_continuous)

	# Ogranicz dane tylko do atrybutów ciągłych, docelowo - zamień dane na dane ciągłe
	# kdd.data.continuous <- kdd.data[, kdd.columns$is_continuous]

	#kdd.data.continuous
	kdd.data
}

kdd.data.sets.num <- 10
kdd.k.values <- c(1, 2, 5, 10, 25, 50, 100)

print('Loading namesfile:')
kdd.namesfile <- kdd.namesfile.read('kdd/data/kddcup.names')

kdd.columns <- kdd.namesfile.columns.extract(kdd.namesfile)
kdd.columns <- kdd.columns.add.label(kdd.columns)

print('Loading datafiles:')
kdd.data.sets <- list()
for(i in 1:kdd.data.sets.num) {
	kdd.data.set.filename <- sprintf('kdd/data/samples/kddcup.data_sample.%d', i)
	kdd.data.sets[[i]] <- kdd.load.data(kdd.data.set.filename, kdd.columns)
}

print('Running cross evaluation:')
validation.cross.k(kdd.data.sets, kdd.columns$is_continuous, 'label', 'normal.', kdd.k.values)
dev.off()