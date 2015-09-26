## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## - an object called inv is created to hold the cahed inverse, and initialize as null
## - then we define a set function that overwrite the matrix and invalidate the cache
## - setinverse/getinverse set/get the inverse
## - we return the list of the new functions.
##
## The point is: inv and the functions lives in the same environment 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function()inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
##
## This function
## 1- retrieves the cached value.
## 2- if the cached value  is not null it is returned
## 3- else the matrix is retrieve and its inverse computed and stored
## 4- the inverse is returned
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    ii <- solve(data, ...)
    x$setinverse(ii)
    ii
}

