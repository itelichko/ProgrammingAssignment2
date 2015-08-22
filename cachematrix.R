## This file provides functions for working with matrix object that can cache its inverse

## Creates a object that can keep matrix and its inverse
## Provides get/set methods for accessing original matrix
## Provides getinverse/setinverse methods for accessing inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(mean) m <<- mean
	getinverse <- function() m
	list( set = set, get = get
	    , setinverse = setinverse
	    , getinverse = getinverse )
}

## Returns a matrix that is the inverse of 'x'
## Result is cached inside matrix object and used in the next
## function calls to avoid costly recomputations

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if (!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
