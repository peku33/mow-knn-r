source('normalizer.r')

distance.multiple.mixed_as_continuous <- function(test.row, train.rows, columns.continuous, columns.discrete, discrete.value.same, discrete.value.different) {

	result.distances <- c()

	for(i in 1:nrow(train.rows)) {
		
		train.row <- train.rows[i,]

		normalized.list <- normalizer.two.mixed_as_continuous(test.row, train.row, columns.continuous, columns.discrete, discrete.value.same, discrete.value.different)
		normalized.matrix <- matrix(c(normalized.list[[1]], normalized.list[[2]]), nrow = 2, byrow = TRUE)
		distance <- dist(normalized.matrix)

		result.distances <- c(result.distances, distance)
	}

	result.distances
}