## The makeCacheMatrix function takes a matrix and caches it for later use
## while the cacheSolve function solves for the inverse of the matrix
## previously cached.

## The first function is designed to cache a matrix for later use for example:
## x <- matrix(1:4, c(2,2))
## m <- makeCacheMatrix(x)
## by running these command R will cache the matrix and store it in object m

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setmatrix <- function(solve) m <<- solve
	getmatrix <- function() m
	list(set = set, get = get,
		setmatrix = setmatrix,
		getmatrix = getmatrix)
}


## This function is designed to return the inverse of a matrix that was previously cached 
## for example using the commands from above we can find the inverse with:
## cacheSolve(m) or cacheSolve(makeCacheMatrix(x)) both of wich will return
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5


cacheSolve <- function(x = matrix(), ...) {
	m <- x$getmatrix()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	matrix <- x$get()
	m <- solve(matrix, ...)
	x$setmatrix(m)
	m
}
