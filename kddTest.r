source('kddLoader.r')
source('distance.r')

kdd.namesfile <- kdd.namesfile_read()

#kddLabels <- kdd.namesfile_labels_extract(kdd.namesfile)
#print(kddLabels)

kdd.columns <- kdd.namesfile_columns_extract(kdd.namesfile)
kdd.columns <- kdd.columns_add_label(kdd.columns)
kdd.data <- kdd.data_read('kddcup.data_10_lines_labeled', kdd.columns)

matrix <- distance.mixed_as_continuous_matrix(
	kdd.data[1, kdd.columns$is_continuous],
	kdd.data[1, !kdd.columns$is_continuous],

	kdd.data[7, kdd.columns$is_continuous],
	kdd.data[7, !kdd.columns$is_continuous],

	100,
	1000
)