## This pair of functions, used together, convert a matrix into a list,
##      and then perform the solve function on the "matrix" list in order
##      to get the inverse of the matrix.  Once this operation has been
##      performed once, the results are cached for quicker access.


## This function creates a special "matrix" object that can cache its inverse.
##      It assumed that the matrix supplied is invertible.
makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
##      returned by makeCacheMatrix above. If the inverse has already been calculated 
##      (and the matrix has not changed), then the cachesolve should retrieve 
##      the inverse from the cache.
cacheSolve <- function(x, ...) {

        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
