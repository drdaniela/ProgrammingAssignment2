## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        initial<- NULL
        new<- function(y){ x <<- y
                         initial <<- NULL
                         }
        getnew <- function() x
        
        setinverse <- function(inverse)
                initial<<- inverse
        getinverse <- function () initial
        list(new = new, getnew = getnew, sentinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        initial <- x$getinverse()
        if(!is.null(initial)){
        message("hetting cached data")
                return (initial)
        
        }
        
        data <- x$get()
        initial <- solve(data, ...)
        
        x$setinverse(initial)
        
        initial
}
