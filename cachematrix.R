## Caching inverted matrix example
## Using two functions
## First (makeCacheMatrix) create interface for any matrix given 
## as parameter, which allows:
## - get/set matrix itself
## - get/set inverted matrix for given one 
## (inverted matrix is not calculated here)
##
## Second function (cacheSolve) uses matrix with inteface
## created with makeCacheMatrix function
## Returns inverted matrix for given one
## Calculates inverted matrix and
## caches it if it's not has been calculated.
## Returns cashed value in all other cases

## Adds an intarface to a matrix "x"
## set/get allows setting/getting the matrix data
## setInverted/getInverted allows to set/get
## inverted matrix of "x"

makeCacheMatrix <- function(x = matrix()) {
    solved <- NULL
    
    set <- function(y) {
        x <<- y
        solved <<- NULL
    }
    get <- function() x
    
    setInverted <- function(slv) {
        solved <<- slv
    }
    getInverted <- function() solved
    
    list(set = set, get = get, setInverted = setInverted, getInverted = getInverted)
}


## Works with object created with "makeCacheMatrix" function ("x" param)
## Returns inverted matrix for "x" matrix
## Uses cashed value if exists or computes and cashes this value
## (when there is a first call)

cacheSolve <- function(x, ...) {
    inverted <- x$getInverted()
    if (!is.null(inverted)) {
        message("from cach")
        return(inverted)
    }
        
    mx <- x$get()
    inverted <- solve(mx, ...)
    x$setInverted(inverted)
        
    inverted
}
