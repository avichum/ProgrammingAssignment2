## The makeCacheMatrix and cacheSolve calculate the inverse of a matrix and cache it for future use.
## If the inverse of the same matrix needs to be calculated in the future, then it will retrieve the cached value
## instead of calculating a new one.

## the makeCacheMatrix initializes the value of the function to the matrix provided. This way it can be 
## used to get the value of the matrix or store the value of the computed cache for future use.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)



}


## The cacheSolve actually calculates the inverse of the matrix provided, and stores it in the cache if it didn't
## already exist. If it did exist, then it prints out the message "getting cached data" and retrieves the data 
## from the cache instead of calculating it again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
