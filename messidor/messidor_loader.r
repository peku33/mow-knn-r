library('foreign')

messidor.columns.get <- function() {

	data.frame(
		name = c('quality', 'prescreening', 'ma1', 'ma2', 'ma3', 'ma4', 'ma5', 'ma6', 'ex1', 'ex2', 'ex3', 'ex4', 'ex5', 'ex6', 'ex7', 'ex8', 'dist', 'diam', 'amfm', 'Class'),
		is_data_continuous = c(rep(TRUE, 19), FALSE)
		#row.names = c('name', 'is_data_continuous')
	)

}
messidor.data.read <- function(file.path) {

	read.arff(file.path)

}
messidor.data.write <- function(file.path, data) {

	write.arff(data, file.path)
}