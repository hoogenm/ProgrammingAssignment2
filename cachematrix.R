##
## "Functions to cache the inverse of a matrix"
## The functions below can be used to cache the inverse of a matrix
##


## makeCacheMatrix creates a special "vector", which is really a list containing a function to:
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse
##  4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL # i holds the cached inverse (or NULL as long as it is not computed)
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

## cacheSolve calculates the inverse of the special "vector" created with the makeCacheMatrix function. However,
## it first checks to see if the inverse has already been computed. If so, it gets the inverse from the cache and
## skips the computation. Otherwise, it computes the inverse of the matrix and sets the value of the inverse in the
## cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()   
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...) # Compute the inverse
        x$setinverse(i)       # Cache the inverse for subsequent calls
        i                     # Return the inverted matrix as a result
}
