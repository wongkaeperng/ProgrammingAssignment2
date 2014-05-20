## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# My comments:
# The function perform Get and Set methods for a matrix and its inverse 
makeCacheMatrix <- function(x = matrix()) {

        # store inverse matrix
        inv <- NULL

        # Set matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Get matrix
        get <- function() x

        # Set inverse matrix
        setinv <- function(inverse) inv <<- inverse
        # Get inverse matrix
        getinv <- function() inv

        # Return matrix
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function

# My comments:
# Compute the inverse of a matrix. If the result is already in cache,
# just return the cache and does not perform any computation.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # get the inverse of x        
        inv <- x$getinverse()
  
        # check if the matrix is cache. If so, just return the cache value.   
        if(!is.null(inv)) {
                return(inv)
        }
        
        # if not, get the inverse of the matrix   
        matrixData <- x$get()
        inv <- solve(matrixData, ...)
        
        # set the inverse of the matrix 
        x$setinverse(inv)
        
        # return the inv
        inv
}
