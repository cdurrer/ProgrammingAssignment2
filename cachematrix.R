## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly (there are also 
## alternatives to matrix inversion that we will not discuss here). 

## This function creates a special matrix object that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setInv <- function(inv) i <<- inv
    getInv <- function() i
    list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInv()
    if (!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    mat <- x$get()
    i <- solve(mat, ...)
    x$setInv(i)
    i
}