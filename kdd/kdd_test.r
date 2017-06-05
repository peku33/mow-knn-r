source('kdd/kdd_loader.r')
source('validation.r')

kdd.data.sets.num <- 10
kdd.k.values <- c(1, 2, 5, 10, 25, 50, 100)

print('Loading meta:')
kdd.namesfile <- kdd.namesfile.read('kdd/data/kddcup.names')
kdd.columns <- kdd.namesfile.columns.extract(kdd.namesfile)
kdd.columns <- kdd.columns.add.label(kdd.columns)

print('Loading data:')
kdd.data.sets <- list()
for(i in 1:kdd.data.sets.num) {
	kdd.data.set.filename <- sprintf('kdd/data/samples/kddcup.data_sample.%d', i)
	kdd.data.sets[[i]] <- kdd.data.read(kdd.data.set.filename, kdd.columns)
}

print('Running cross evaluation:')
validation.cross.svm(
	kdd.data.sets,
	kdd.columns$is_continuous,
	'label',
	'normal.'#,
	#'manhattan',
	#kdd.k.values
)
dev.off()