## This function is meant to use the scoping rules of R to store the 
## inverse of a matrix, so that it can be called upon later.

## This function creates a special "matrix", which is really a list
## containing a function to 
##	1. set the values of the matrix
##	2. get the values of the matrix
##	3. set the value of the matrix
##	4. get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL 
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function () x
	setInverse function(solve) i <<- solve
	getInverse function() i
	list(set=set, get=get,setInverse=setInverse,getInverse=getInverse)
  
}


## This function checks if there is a cached inverse, and if not 
## computes the inverse and returns it.  

cacheSolve <- function(x, ...) {
        i <- x$getInverse
        if (!is.null(i)) {
        	message("getting cached data")
        	return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}



