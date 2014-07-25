## Those funtions is dedicated to faster calculation of inverse matrixes


## This function returns list of functurs that are able to push and pop 
## matrix and its inverse to memory (do some caching) 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x', but 'x' is only 'vector' that created by makeCacheMatrix.
cacheSolve <- function(x, ...) {
        i<-x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
