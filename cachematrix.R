## Two functions are used to create a special object that stores a matrix, 
## calculate and cache its inverse matrix 

## makeCacheMatrix - create a special list of functions:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse matrix
## - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        invM <- NULL
        set <- function(y) {
                x <<- y
                invM <<- NULL
        }
        get <- function() x
        setinverse <- function(inver) invM <<- inver
        getinverse <- function() invM
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calculate the inverse matrix. 
## If the inverse matrix has already been calculated it skips the computation
## and use the cache.

cacheSolve <- function(x, ...) {
        invM <- x$getinverse()
        if(!is.null(invM)) {
                message("getting cached data")
                return(invM)
        }
        data <- x$get()
        invM <- solve(data, ...)
        x$setinverse(invM)
        invM        
}
