kdd.namesfile.read <- function(kdd.namesfile.path) {
	f <- file(kdd.namesfile.path, open='r')
	data <- readLines(f)
	close(f)

	data
}

kdd.namesfile.labels.extract <- function(kdd.namesfile) {
	# Pierwsza linia zawiera etykiety
	kdd.namesfile_line <- kdd.namesfile[1]

	# Usuwamy ostatnią kropkę
	kdd.namesfile_line <- substr(kdd.namesfile_line, 0, nchar(kdd.namesfile_line) - 1)

	# Dzielimy kolumny
	strsplit(kdd.namesfile_line, ",")[[1]]
}

kdd.namesfile.columns.extract <- function(kdd.namesfile) {

	variables <- strsplit(kdd.namesfile, ": ")

	name <- c()
	is_continuous <- c()

	for(variable in variables[-1]) {

		name <- c(name, variable[1])
		is_continuous <- c(is_continuous, variable[2] == "continuous.")

	}

	data.frame(name, is_continuous)
}

kdd.columns.add.label <- function(columns) {
	
	name <- c('label')
	is_continuous <- c(FALSE)

	rbind(columns, data.frame(name, is_continuous))
}

kdd.data.read <- function(file.name, columns) {

	read.table(
		file.name,
		header = FALSE,
		sep = ",",
		col.names = columns$name,
		colClasses = lapply(columns$is_continuous, function(is_continuous) if(is_continuous) "numeric" else "character"),
	)
}

kdd.data.write <- function(file.name, data) {

	write.table(
		data,
		file.name,
		sep = ",",
		quote = FALSE,
		row.names = FALSE,
		col.names = FALSE,
	)

}