## Using 2 functions below it is possible to cache inverted matrix
## to use it after

## The Function that makes a list of functions to work with a matrix: 
## set original and inverted matrix and get them
makeCacheMatrix <- function(x = matrix()) {
        inv_m <- NULL
        set <- function(y) {
                x <<- y
                inv_m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv_m <<- solve
        getinverse<- function() inv_m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function calculate the inverted matrix for the matrix that was 
## set with a function makeCacheMatrix. If inverted matrix was calculated
## once, after it is being taken from cache instead of calculating again.
cacheSolve <- function(x, ...) {
        inv_m <- x$getinverse()
        if(!is.null(inv_m)) {
                message("getting cached data")
                return(inv_m)
        }
        data <- x$get()
        inv_m <- solve(data, ...)
        x$setinverse(inv_m)
        inv_m
}
