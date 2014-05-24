
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#             If the inverse has already been calculated (and the matrix has not changed), 
#             then the cachesolve should retrieve the inverse from the cache.


# makeCacheMatrix: return a list of functions to:
#  i.   Set the value of the matrix
#  ii.  Get the value of the matrix
#  iii. Set the value of the inverse
#  iv.  Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
         
        invrs <- NULL
       
        # Set the matrix
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        # Get the matrix
        get <- function() x
        
        # Set the inverse matrix
        setinvrs <- function(inverse) invrs <<- inverse
        # Get the inverse matrix 
        getinvrs <- function() invrs
        
        # Return the matrix functions
        list(set = set, get = get, setinvrs = setinvrs, getinvrs = getinvrs)
}


# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated,it returns the cached result

cacheSolve <- function(x, ...) {
        invrs <- x$getinvrs()
        
        # If the inverse is already calculated, return it
        if (!is.null(invrs)) {
                print("fetched cached result")
                return(invrs)
        }
        
        # The inverse is not calculated, calculate it and cache it
        data <- x$get()
        invrs <- solve(data, ...)
        
        # Cache the inverse matrix
        x$setinvrs(invrs)
        
        # Return result
        invrs
}


