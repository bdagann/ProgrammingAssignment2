# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly
## tested with m1 <- matrix (c(1,2,6,4),2,2)
## m2 <- matrix (c(1,1,1,2,3,2,3,3,4),3,3)

## creates a special "matrix" object that can cache its inverse, assuming that
## the matrix is invertible.
## The functoins cotains 4 sub functions (methods):
# set the matrix
# get the matrix
# set the inverse matrix
# get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has 
## not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    } else {
        message("calculating data")
        data <-x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        return (inv)
    }
}
