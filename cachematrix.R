## The two functions work together to calculate the inverse of a matrix and
## cache it for later use


## Creates a list of functions which set and report the values of the passed 
## matrix and the variable inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL		## Resets the value of inverse

	## Allows matrix and inverse value to be reset in all environments by calling $set()
	set <- function(y, row, col) {
		x <<- matrix(y, row, col)
		inverse <<- NULL
	}

	## Prints the matrix
	get <- function() x	
	
	## Assigns a value to the variable inverse, for all environments - NOT TO BE CALLED DIRECTLY
	setInverse <- function(i) inverse <<- i
	
	## Prints the variable inverse
	getInverse <- function() inverse

	## Prints the list of functions, allowing each functions to be called as a list element
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Calculates and caches the inverse of the passed matrix only if there is no cached inverse
## Reports the inverse, indicating if it is a cached value

cacheSolve <- function(x, ...) {
	
	data <- x$get()
  inverse <- x$getInverse()				## Assigns the cached inverse to the variable
	
	if (!is.null(inverse)) {			## If the cached inverse is not NULL, 
		message("getting cached data")	## returns the inverse indicating that it is the cached value
		return(inverse)
	}
	
	x$setInverse(solve(data))				## Solves the inverse and caches it in the variable "inverse"
	x$getInverse()					## Prints the calculated result
}
