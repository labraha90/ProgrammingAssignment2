## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function sets the value of our matrix
## and sets and gets the inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinv <- function(solve) inverse <<- solve
	getinv <- function() inverse
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve function checks to see if the inverse of the matrix
## has been cached and if not it calculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data, ...)
	x$setinverse(inverse)
	inverse
}
