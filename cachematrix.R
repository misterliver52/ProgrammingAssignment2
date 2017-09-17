## Functions to store inverse-matrix data to cache to
## 1. set the value of the matrix-inverse
## 2. get the value of the matrix-inverse
## 3. set the value of the mean of the cached values
## 4. get the value of the mean of the cached values
##
##

## makeCacheMatrix() to cache the matrix 'x' for inversion
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(invMatrix) inv <<- invMatrix
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Calculates the inverse of the cached 'x' Matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data...")
                return(inv)
        }
        dat <- x$get()
        inv <- solve(dat, ...)
        x$setinv(inv)
        inv
}
