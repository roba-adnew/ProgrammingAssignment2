## The functions makeCacheMatrix and cachesolve will store a matrix and return its
## inverse by calculating it and storing it once rather than calculating it
## everytime cachesolve is called. A new matrix can be stored in an object
## if desired.

## makeCacheMatrix returns list of functions based on the initial input vector
## x. The list of functions that will be stored will (1) set a new matrix to 
## invert (2) return the currently set matrix to be inverted (3) set the inverse
## once calculated (4) return the inverse of the set matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}        

## cachesolve will take an input that will be an object created from
## makeCacheMatrix being called. If there is an inverse already calculated it
## will return that inverse, otherwise it will calculate the inverse of the
## currently set matrix.

cachesolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(i)
        m
}