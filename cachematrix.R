## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special matrix object with functions that can set and return the matrix
# data and inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (y = matrix()) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invMatrix = matrix()) inv <<- invMatrix
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# This function returns the inverse of a matrix
# It first checks if the inverse already exists
# If yes, it directly returns the inverse stored in the matrix x
# Otherwise, it uses the solve function to calcuate the inverse and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
