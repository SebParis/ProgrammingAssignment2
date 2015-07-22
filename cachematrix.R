## This pair of functions allow to cache and access the inverse of a matrix
## as it may be costly to compute it repeatedly

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

	## Initialize inverse
	inverse <- NULL

	## declare set function to set value of the matrix
	set <- function (y) {
		x <<-y
		inverse <<- NULL
	}

	## declare get fucntion to get the matrix
	get <- function() x
	
	## declare setinverse function to cache inverse
      setinverse <- function(inversedata) inverse <<- inversedata

	## declare getinverse data to recover cached data
      getinverse <- function() inverse
      
	## list all functions
	list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	## try to get cached inverse
	inverse <- x$getinverse()
	
	## return inverse if cached
	if (!is.null(inverse)) {
		message ("getting cached data")
		return(inverse)
	}

	## if not cached, compute inverse by recovering matrix data
	data <- x$get()
	inverse <- solve(data, ...)
	x$setinverse(inverse)
	inverse

}
