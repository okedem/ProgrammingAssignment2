## The following creates a list object holding four functions to set and retrieve 
## a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # initialize the inverse as a null
    set <- function(y) { # setting the matrix in the external variable x
        x <<- y
        inv <<- NULL
    }
    get <- function() x # retrieve the matrix stored in x
    setinv <- function(inverse) inv <<- inverse # set the inverse of the matrix
    getinv <- function() inv # retrieve the inverse
    # create the list with the four functions just defined
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)    
}


## The following returns the inverse of the matrix x. It either calculates it,
## or, if the inverse has already been calculated, and the matrix is unchanged, 
## it retrieves the inverse from the cached value.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv() # get the inverse
    if(!is.null(inv)) { # if the inverse exists, return it (and exit the function)
        message("getting cached data")
        return(inv)
    }
    # if we're here, the inverse doesn't exist, and so the function calculates it
    data <- x$get() # get the matrix using "get" function in the list 'x'
    inv <- solve(data, ...) # invert the function
    x$setinv(inv) # set = cache the value of the inverse
    inv # report the inverse
}
