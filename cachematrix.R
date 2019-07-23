## this function creates a special "matrix", which is really a list containing a function to:
## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(new_inv) inv <<- new_inv
    getinv <- function() inv
    list(set = set, get = get, getinv = getinv, setinv = setinv)
}


## The following function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse 
## in the cache via the setinv function.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
