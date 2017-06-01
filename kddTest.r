source('kddLoader.r')
source('distance.r')
source('knn.r')

print('Loading namesfile')
kdd.namesfile <- kdd.namesfile_read()

kdd.columns <- kdd.namesfile_columns_extract(kdd.namesfile)
kdd.columns <- kdd.columns_add_label(kdd.columns)

print('Loading datafile')
kdd.data <- kdd.data_read('kddcup.data_10k_lines_labeled', kdd.columns)

print('Normalizing data')
kdd.data.normalized <- normalizer.set.scale_continuous(kdd.data, kdd.columns$is_continuous)

print('Done')