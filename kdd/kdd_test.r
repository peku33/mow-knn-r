source('kdd/kdd_loader.r')
source('anomaly.r')
source('distance.r')
source('knn.r')
source('cid.r')

library('pROC')

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
# plot.new()
for(index.train in 1:(kdd.data.sets.num-1)) {
	for(index.test in (index.train+1):kdd.data.sets.num) {

		print(sprintf(' Train set = %d, Test set = %d', index.train, index.test))
		matrix <- anomaly.score.data.multiple.k.matrix.build(
			kdd.data.sets[[index.test]][, kdd.columns$is_continuous],
			kdd.data.sets[[index.train]][kdd.data.sets[[index.train]]$label == "normal.", kdd.columns$is_continuous]
		)

		for(k in kdd.k.values) {

			print(sprintf('  k = %d', k))
			name <- sprintf('Training set = %d, Testing set = %d, K = %d', index.train, index.test, k)
			distances <- anomaly.score.data.multiple.k.matrix.by(matrix, kdd.data.sets[[index.test]], k)

			roc <- roc(
				factor(kdd.data.sets[[index.test]]$label != "normal.", ordered = TRUE),
				factor(distances, ordered = TRUE),
			)

			#roc.auc <- auc(roc)
			#subname <- sprintf('AUC = %f', roc.auc)

			plot.roc(roc, main = name, print.auc = TRUE)
		}
	}
}
dev.off()