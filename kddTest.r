source('kddLoader.r')
source('distance.r')
source('knn.r')

kdd.namesfile <- kdd.namesfile_read()

#kddLabels <- kdd.namesfile_labels_extract(kdd.namesfile)
#print(kddLabels)

kdd.columns <- kdd.namesfile_columns_extract(kdd.namesfile)
kdd.columns <- kdd.columns_add_label(kdd.columns)
kdd.data <- kdd.data_read('kddcup.data_10k_lines_labeled', kdd.columns)

kdd.calculate_data_distances <- function(data.test.row, data.train.rows) {

	distance.multiple.mixed_as_continuous(data.test.row, data.train.rows, kdd.columns$is_continuous, !kdd.columns$is_continuous, 0, 1)

}

