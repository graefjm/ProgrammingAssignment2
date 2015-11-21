## Matrix inversion can be a costly computation. These two functions below will help to 
## create a special matrix object and then cache its inverse.

## This function creates a special matrix and the functions used to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ##Initialize the variables and set to NULL
        nvrs <- NULL
        set <- function(y) {
                x <<- y
                nvrs <<- NULL
        }
        
        ##Defines the inverse functions
        get <- function() x
        setInverse <- function(inverse) nvrs <<- inverse
        getInverse <- function() nvrs
        
        ##Returns a list of functions that can be used later in cacheSolve()
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function caches and computes the inverse of the special matrix in makeCacheMatrix

cacheSolve <- function(x, ...) {
        
        ##Gets the inverse from the object x and returns it if calculated
        ##If it is not found the result will be NULL
        nvrs <- x$getInverse()
        if(!is.null(nvrs)) {
                message("getting cached data")
                return(nvrs)
        }
        
        ## If it is NULL, we can take x and use Solve() to get the inverse
        matrix_data <- x$get()
        nvrs <- solve(matrix_data, ...)
        x$setInverse(nvrs)
        nvrs
}