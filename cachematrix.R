## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## These two functions are used to create a special object that stores a numeric
## matrix and cache's its inverse.
## Example:
##      1. Initialize a square and invertible matrix
##         x <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2)
##      2. Create cacheMatrix
##         cacheMatrix <- makeCacheMatrix(x)
##      3. Calculate inversed matrix of x
##         cacheSolve(cacheMatrix)
## Repeat step 3. again will return cached data



## Create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        
        # Set matrix
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        # Get matrix
        get <- function() x
        
        # Set inverse of matrix
        setSolve <- function(solve) s <<- solve
        
        # Get inverse of matrix
        getSolve <- function() s
        
        # Return
        list(set = set, get = get, 
             setSolve = setSolve,
             getSolve = getSolve)
}



## Compute the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        s <- x$getSolve()
        
        # If the inverse has already been calculated (s is not null)
        # Return cached data
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        
        # Else, calculate the inverse, cache and return it
        data <- x$get()
        s <- solve(data, ...)
        x$setSolve(s)
        s
}
