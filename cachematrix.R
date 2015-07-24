## makeCacheMatrix and cacheSolve are the two primary functions.
##
## makeCacheMatrix is responsible for creating a matrix object that
##  has the ability of caching it's inverse
##
## cacheSolve is responsible for computing the inverse of a matrix.  However,
##  the functions determines whether the inverse has already been cached
##  and if it has, then returns the result from the cache.  Otherwise, it
##  invokes a function to compute the inverse, storing the result in the cache.

## makeCacheMatrix -- creates a matrix which is capable of storing
##   the computation of it's inverse. There are four functions:
## set(x) -- saves a copy of the matrix and initializes it inverse to null
## get -- returns the matrix
## setsolve -- computes the inverse of the matrix and stores the solution
## getsolve -- retrieves the inverse of the solution from the cache

makeCacheMatrix <- function(x = matrix()) {
    mi <- NULL
    set <- function(y) {
        x <<- y
        mi <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) mi <<- solve
    getsolve <- function() mi
    list(set = set, get = get,
    setsolve = setsolve,
    getsolve = getsolve)
}

##
## cacheSolve -- retrieves the inverse of matrix by checking to determine
##   if the inverse has already been cached.  If not, it invokes the setsolve
##   function to compute the inverse, stores the result in the cache then
##   returns the result (the innverse of the matrix).
##   IF the inverse has been cached, it just returns the cached result.
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the solve of 'x'
        mi <- x$getsolve()
        if(!is.null(mi)) {
            message("getting cached data")
            return(mi)
        }
        data <- x$get()
        mi <- solve(data, ...)
        x$setsolve(mi)
        mi
}
