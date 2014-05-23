## Author: Shubham Agrawal
## Created on 24th May, 2014


## Following functions can be used to create a special matrix object and cache its inverse


## function makeCacheMatrix creates a special "matrix" object, which is actually a list containing functions to set/get the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    # assigns x_inv to NULL for each creation of special matrix object
    x_inv <- NULL
    
    # create funtions to set/get matrix 'x' and its inverse
    set <- function(y) {
        x <<- y
        # change to matrix 'x' resets x_inv to NULL        
        x_inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) x_inv <<- inv
    getinv <- function() x_inv
    
    # return list containing functions to get/set matrix and its inverse
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## function cacheSolve takes special matrix as input, caches(if not already) and returns its inverse(assuming matrix is always invertible)
cacheSolve <- function(x, ...) {
    # check and return the inverse if it was already computed and stored for x
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting inverse")
        return(inv)
    }
    
    # if inverse was not stored before: get the matrix, compute its inverse, store and return the inverse
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinv(inv)
    inv
}
