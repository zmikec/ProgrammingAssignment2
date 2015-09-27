## The functions below can be used to create a matrix object, and then calculate
## the inverse of the matrix (returning results directly from cache if available).

makeCacheMatrix <- function(x = matrix()) {
    # This function creates a special "matrix" object that can cache its inverse.
    # Limitations: Does not handle amendments to matrix - so drop/recreate matrix instead!
    
    # initialize cache
    inverse_x <- NULL

    # functions to store and retrieve cached values
    set <- function (xmat) {
        x <<- xmat
        inverse_x <<- NULL
    }
    get <- function() x
    setinverse <- function(xinv) inverse_x <<- xinv
    getinverse <- function() inverse_x
    
    # store the internal functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    # Calculate and return the inverse matrix of 'x', a matrix created using makeCacheMatrix.
    # Also, store that matrix and associated inverse matrix in memory (cache), so that if a  
    # subsequent call can return the inverse directly from cache.
    
    # check if inverse matrix available in cache, and if so, return it 
    cached_inverse_x <- x$getinverse()
    if(!is.null(cached_inverse_x)) {
        message("Getting inverse matrix from cache")
        return(cached_inverse_x)
    }
    
    # else calculate inverse matrix and cache it before returning it
    mat <- x$get()
    calc_inv_mat <- solve(mat)
    x$setinverse(calc_inv_mat)

    calc_inv_mat
}
