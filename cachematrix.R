## The functions contained here allow for caching the result of
##   a Matrix inversion, which can allow for more efficient use
##   of resources in this costly computation

## Thi function creates a special "matrix" object that can cache
##    its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    ## initialize variable to hold the cached inversed matrix
    inv <- NULL
    
    ## Setup function which will set the stored matrix
    ##   and re-initialize the cache variable
    set <- function(y) {
        
        ## set stored base matrix
        x <<- y
        
        ## re-initialize/clear cache variable
        inv <<- NULL
    }
    
    ## Setup function to return the stored base matrix
    get <- function() x
    
    ## Setup set function to store cached inversed matrix
    setinverse <- function(solved) inv <<- solved
    
    ## Setup get function to retrieve cached inversed matrix
    getinverse <- function() inv
    
    ## Create list of named elements to allow access
    ##    to sub-functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse
     )
    
}


## This function computes the inverse of the special "matrix"
##      returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    
    ## Get the value stored as the cached inverse
    inv <- x$getinverse()
    
    ## Check to see if the inverse has already been cached
    if(!is.null(inv)) {
        
        ## inverse has been cached; return the cached value
        message("getting cached data")
        return(inv)
    }
    
    ## inverse wasn't cached
    ## Gets the stored base matrix
    data <- x$get()
    
    ## Solve the inverse for the stored base matrix
    inv <- solve(data, ...)
    
    ## Call function to store/cache inversed matrix
    x$setinverse(inv)
    
    ## Return the inversed matrix
    inv
}
