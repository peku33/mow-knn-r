source('kdd_loader.r')
source('distance.r')
source('knn.r')
source('cid.r')

print('Loading namesfile')
kdd.namesfile <- kdd.namesfile.read('data/kddcup.names')

kdd.columns <- kdd.namesfile.columns.extract(kdd.namesfile)
kdd.columns <- kdd.columns.add.label(kdd.columns)

print('Loading datafile')
kdd.data <- kdd.data.read('data/kddcup.data_10_percent_labeled', kdd.columns)

print('Changing discrete -> continuous')
# Ogranicz dane tylko do atrybutów ciągłych, docelowo - zamień dane na dane ciągłe
kdd.data <- kdd.data[, kdd.columns$is_continuous]

print('Normalizing data')
kdd.data <- normalizer.set.scale_continuous(kdd.data, kdd.columns)

print('Selecting data')

print('Done')