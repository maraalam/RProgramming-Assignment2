# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly (there are 
# also alternatives to matrix inversion that we will not discuss here). 
# Your assignment is to write a pair of functions that cache the inverse of a matrix.


# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL # inverse of matrix
    
    set <- function(y) {
        x <<- y # assign y to x in enviroment
        i <<- NULL # re-inizialise the inverse to NULL
    }
    
    get <- function() x # function to get the matrix
    
    setinverse<- function(inverse) i <<- inverse #function to set the inverse of the matrix
    
    getinverse <- function() i # function to get the cache inverse of the matrix
    
    list(set = set, get = get, 
           setinverse = setinverse, getinverse = getinverse)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the inverse 
# from the cache.
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    
    # check if the inverse is in the cache
    if(!is.null(i)){
        message("getting cached inverse")
        return(i)
    }
    
    # if not -> is calculated
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    i
}
