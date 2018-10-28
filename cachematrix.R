## These functions introduce a special matrix object which caches its inverse.
## The create such a special matrix m you execute \code{m <- makeCacheMatrix(x)}
## where x is an ordinary matrix. You can then get the value with \code{m$get()}
## and change the value with \code{m$set(y)} where y is an ordinary matrix.
## You can get the inverse with \code{cacheSolve(m)}.


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        # Define function to set the value of the matrix. It also clears the old
        # inverse from the cache
        set <- function(y) {
                x <<- y         # Set the value
                i <<- NULL      # Clear the cache
        }
        
        # Define function to get the value of the matrix
        get <- function() x
        
        # Define function to set the inverse. This is only used by getinverse() when
        # there is no cached inverse
        setinverse <- function(inverse) i <<- inverse
        
        # Define function to get the inverse
        getinverse <- function() i
        
        # Return a list with the above four functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, while if
## not, it computes, caches, and returns it.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getinverse()
        
        # If the cache was not empty, we can just return it
        if (!is.null(inv_x)) {  
                
                message("getting cached inverse matrix")
                return(inv_x)
                
        } else {
                
                inv_x <- solve(x$get()) # Get value of matrix
                x$setinverse(inv_x) # Calculate inverse
                return(inv_x)   # Return the inverse
                
        }
        
}
