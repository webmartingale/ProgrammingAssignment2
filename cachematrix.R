## Functions makeCacheMatrix and cacheSolve implement a matrix with calculation and storage of it's cacheable inverse.

## This function creates a structure which can store a matrix and also it's inverse.
## getter / setter functions included as list elements
makeCacheMatrix <- function(x = matrix()) {
    #init
    inv <- NULL

    #functions for storage
    setData <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getData <- function() x
    setInv <- function(y) inv <<- y
    getInv <- function() inv

    #return structure
    list(setData = setData, getData = getData, setInv = setInv, getInv = getInv)
}


## This function returns the inverse of the matrix stored in a cacheMatrix x
## if the inverse is not yet already computed and stored in cache
## then it's calculated on the spot and cached before returning
## otherwise it return the pre-cached inverse
cacheSolve <- function(x, ...) {

    #check if already cached
    inv <- x$getInv()
    if (!is.null(inv))
    {
        message("getting cached matrix inverse from cache.")
        return(inv)
    }

    #otherwise calculate inverse and cache
    inv <- solve(x$getData())
    x$setInv(inv)

    ## Return a matrix that is the inverse of 'x'
    inv
}
