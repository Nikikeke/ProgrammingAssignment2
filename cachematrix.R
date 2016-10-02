## Week3 

## ProgrammingAssignment2

## Defines a matrix with cachable inverse value
## Defines a Matrix which can cache its inverse
## This function creates a special "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inverse <<- solve
        getsolve <- function() inverse
        list(set = set, get = get, 
             setsolve = setsolve,
             getsolve = getsolve)
}


## Takes a cache Matrix, checks if the inverse has already been
## computed, calculates if it has not, and then returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getsolve()
        if (!is.null(inverse)) {
                message("Getting cached data")
                return (inverse)
        }
        data <- x$get()
        inverse <- solve(data,...)
        x$setsolve(inverse)
        inverse
}
