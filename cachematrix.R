## makeCacheMatrix function creates a special "matrix" object, 
## which is really a list containing a function to
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse of the matrix
##    4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	invrs <- NULL
	set <- function(y){
		x <<- y
		invrs <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) invrs <<- inverse
	getinverse <- function() invrs
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix function  above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'

	invrs <- x$getinverse()
	if(!is.null(invrs)) {	
					## If the inverse has already been calculated, 
					## then retrieve the inverse from the cache
		message("getting cached data")	
		return(invrs)
	}
		# if not, then calculate the inverse
	data <- x$get()
	invrs <- solve(data, ...)
	x$setinverse(invrs)
	invrs
}
