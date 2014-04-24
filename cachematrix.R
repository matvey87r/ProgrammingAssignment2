## These two functions computes the inverse of matrix. Answer is stored in
## cashe. In case of repeated computation of the same matrix, inverse answer is 
## extracted from cache rather than computed again.


## 'makeCacheMatrix' converts original data matrix to specific list object
## containing functions. They are then used inside 'cacheSolve' function.

makeCacheMatrix <- function(x = matrix()) {
    ## Empty variable to store the inverse of 'x'
    inv_mx <- NULL
    ## 'Re-writes' original data and resets cashed data (not used in current
    ## implementation of 'casheSolve' function).
    set <- function(y) {
        x <<- y
        inv_mx <<- NULL
    }
    
    ## Defines data to be processed
    get <- function() x
    
    ## Writes inverse matrix data to cache
    setCacheMx <- function (inverse) inv_mx <<- inverse
    
    ## Extracts inverse matrix data from cache
    getCacheMx <- function() inv_mx
    
    ## Creates list of 4 functions which is the output of the function
    list(set = set, get = get,
         setCacheMx = setCacheMx,
         getCacheMx = getCacheMx)
}


## 'cacheSolve' returns a matrix that is the inverse of 'x' and saves it 
## into cache. If inverse of 'x' is calcalated repeatedly, extracts previously 
## saved data from cache.

cacheSolve <- function(x, ...) {
    ## Gets inverse matrix from cache
    inv_mx <-x$getCacheMx()
    ## If inverse matrix already exists, extracts it from cache
    if(!is.null(inv_mx)) {
        message("getting cached data")
        return(inv_mx)
    }
    ## Otherwise computes inverse of x from scratch 
    data <- x$get()
    inv_mx <- solve(data, ...)
    x$setCacheMx(inv_mx)
    return(inv_mx)
}
