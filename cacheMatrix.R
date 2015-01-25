## The two functions work together to calculate the inverse of a matrix and
## cache it for later use


## makeCacheMatrix creates a list of functions which set and report the values of the passed 
## matrix and the variable inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL					## Reset variable inverse
        
        set <- function(y, row, col) {			## Allow matrix and inverse value to be reset
                x <<- matrix(y, row, col)		## in all environments by calling $set() 
                inverse <<- NULL
        }
        
        get <- function() x				## Print the matrix
        
        setInverse <- function(i) inverse <<- i		## Assign a value to the variable inverse
        ## NOT TO BE CALLED DIRECTLY
        
        getInverse <- function() inverse		## Print the variable inverse
        
        list(set = set, get = get, 			## Print the list of functions, 
             setInverse = setInverse,			## allowing each function to be called 
             getInverse = getInverse)			## as a list element
        
        
}


## cacheSolve(x) calculates and caches the inverse of the passed matrix only if there is no cached inverse
## Reports the inverse, indicating if it is a cached value

cacheSolve <- function(x, ...) {
        
        data <- x$get()
        inverse <- x$getInverse()				## Assign cached inverse to variable inverse
        
        if (!is.null(inverse)) {			## If cached inverse is not NULL, 
                message("getting cached data")		## return inverse, indicating that it is the cached value
                return(inverse)
        }
        
        x$setInverse(solve(data))			## Solve matrix inverse and cache it in the variable "inverse"
        x$getInverse()					## Print matrix
}