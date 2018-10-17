## 'makeCacheMatrix' initializes a special matrix that has several functions
## associated with it (see below). The makeCacheMatrix also stores its inverse,
## if calculated.
## ****************************************************************************
## 'cacheSolve' calculates the inverse of a cached matrix and feeds the result
##  into that matrix's cache. If the inverse already exists, 'cacheSolve' reads
##  the result.

# Goal: This function creates a special matrix that can cache its inverse.
# Assume the input will always be a square matrix (no error handling)
# Has several function calls associated with it:
#     1)  $set(x) allows the user to reset a 'makeCacheMatrix' object
#       with a new matrix.
#     2)  $get() takes no argument and prints the current matrix
#     3)  $setinverse(inverse) allows the user to set the inverse matrix
#     4)  $getinverse() prints the current inverse matrix
#  *Note that to make some of these functions push data up to the
#   makeCacheMatrix environment level, the superassign operator is used '<<-'
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y = matrix()) {
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


# Goal: This function calculates the inverse of a 'makeCacheMatrix' if the
# inverse does not exist already, using the $get() and $setinverse() functions
# of a cached matrix to read in the matrix. If the inverse exists, it instead
# prints the inverse from cache using the $getinverse() function of a
# cached matrix.
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
