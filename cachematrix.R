## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mx = matrix()) {
	inv <- NULL
	set <- function(n){
		mx <<- n
		inv <<- NULL
	}
	get <- function() mx
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set, get = get, 
	     setInverse = setInverse,
	     getInverse = getInverse)	     
}


## The function computes the inverse of a matrix. 
## If the inverse has been computed, it skips the computation
## and gets the inverse of the matrix from the cache.

cacheSolve <- function(mx, ...) {
## Return a matrix that is the inverse of 'mx'

	inv <- mx$getInverse()
	if(!is.null(inv)) {
		message("retrieving cached matrix...")
		return(inv)
	}
	matrix_1 <- mx$get()
	inv <- solve(matrix_1)
	mx$setInverse(inv)
	inv
}
