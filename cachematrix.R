## This pair of functions calculate and cache the inverse of a matrix, then
## retrieve the cached inverse (saving time and memory), 
## or calculate a new inverse if changes to the original matrix have been made

## Creates a matrix object that can cache itself
## Creates list of functions that can be called externally
makeCacheMatrix <- function(x = matrix()) {     ## defines a matrix, x
        i <- NULL                               ## initialization
        set <- function(y) {                    ## stores matrix in cache
                x <<- y                         
                i <<- NULL
        }
        get <- function() x                     ## returns matrix        
        
        setinv <- function(inv) i <<- inv       ## stores inverse (initialized as NULL) in cache
        getinv <- function () i                 ## returns inverse
        
        list(set = set, get = get,              ## creates a list of functions so 
             setinv = setinv,                   ## they can be called externally
             getinv = getinv)        
}


## Checks to see if inverse of matrix already exists
## Retrieves inverse from cache if already calculated, and matrix has not been changed
## Calculates inverse if no cached inverse exists or matrix has been changed
## Returns inverse matrix (either cached or calculated)
cacheSolve <- function(x, ...) {
        i <- x$getinv()                         ## returns a matrix that is the inverse of x
        if(!is.null(i)) {                       ## checks if inverse is in cache + prints message
                message("getting cached inverse matrix")
                return(i)                       ## returns cached inverse (without computation)
        }
        matrix <- x$get()                       ## gets matrix if not cached
        i <- solve(matrix, ...)                 ## solves for inverse
        x$setinv(i)                             ## stores computed inverse in cache
        i                                       ## returns computed inverse
}
