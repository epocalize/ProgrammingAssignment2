## Calculate and cache inverse of a matrix
## Calculate new inverse if changes to original matrix have been made

## Create a matrix object that can cache itself
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function () i
        list(set = set, get = get,
             getinv = getinv,
             setinv = setinv)
}


## Checks to see if inverse of matrix "x" already exists
## Retrieves inverse from cache if already calculated, and matrix has not been changed
## Calculates inverse if no inverse cached or matrix has changed
## Returns inverse of "x"

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached inverse matrix")
                return(i)
        }
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setinv(i)
        i
}
