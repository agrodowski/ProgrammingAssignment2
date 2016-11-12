#
# R-programming, Programming Assignment 2
#

#
# Defines get/set, getinverse/setinverse closures for the matrix object
#
makeCacheMatrix <- function(x = matrix()) {
	ix <- NULL
	set <- function(y) {
		x <<- y
		ix <- NULL
	}
	get <- function() x
	setinverse <- function(inversem) ix <<- inversem
	getinverse <- function() ix
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#
# Computes the matrix inverse via solve() and caches the result.
#
# Args:
#   x: a matrix object envelopped by makeCacheMatrix() function
#   ...: other arguments passed to solve() function
#
# Returns:
#   The inverse matrix of x
#
cacheSolve <- function(x, ...) {
        ix <- x$getinverse()
        # take from the cache if already stored
        if (!is.null(ix)) {
        	message("getting inverse matrix")
        	return(ix)
        }
        # calculate and cache the inverse matrix of x
        ix <- solve(x$get(), ...)
        x$setinverse(ix)
        ix
}
