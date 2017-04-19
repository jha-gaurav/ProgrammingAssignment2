library(MASS)

## The function makeCacheMatrix creates a special invertible matrix and calculates its inverse and retains
## the inverse in the cache.




makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## The function cacheSolve uses the list of functions defined in makeCacheMatrix to retrieve the matrix inverse, either 
## from the cached value or calculating it afresh.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- ginv(data)
        x$setinv(inv)
        inv
}
