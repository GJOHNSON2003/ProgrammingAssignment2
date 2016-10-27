## These two functions check to see if a cached version of
## an inverted matrix already exsits and if so uses the cached
## veriion.  Otherwise it computes the inverse of an input matrix
## and caches it.

makeCacheMatrix <- function(x = matrix()) {
    ## function creates a special "matrix" object that can 
    ##    cache its inverse
    ## Inputs; x: a square invertible matrix
    ## Returns; a list containing functions to
    ##    1. set the value of matrix
    ##    2. get the value of matrix
    ##    3. set the value of inverse
    ##    4. get the value of inverse
    ##      this list is used as the input to cacheSolve()
    
    inv = NULL
    set = function(y) {
        # use `<<-` to assign a value to an object in an environment 
        # different from the current environment. 
        x <<- y
        inv <<- NULL
    }
    get = function() x
    setinv = function(inverse) inv <<- inverse 
    getinv = function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
    ## Function computes the inverse of the special "matrix" 
    ##    returned by makeCacheMatrix() function
    ## If the inverse has already been calculated 
    ##    then the fucntion will retrieve the inverse from the cache
    ## If a cached version of the input matrix does not exist its computed
    ## 
    ## Inputs; x: output of makeCacheMatrix()
    ## Returns: inverse of the original matrix input to makeCacheMatrix()
    
    inv = x$getinv()
    
    # if the inverse has already been calculated retrieve the
    # cached version
    if (!is.null(inv)){
        # get it from the cache and skips calculating the inverse. 
        message("getting cached data")
        return(inv)
    }
    
    # if not cached already then compute the inverse 
    mat.data = x$get()
    inv = solve(mat.data, ...)
    
    # sets the value of the inverse in the cache via the setinv function.
    x$setinv(inv)
    
    return(inv)
}
