## This function creates a special "matrix" object that can cache its inverse
## Author Bob Stober
## Date: 4/26/2014

## input is numeric matrix
## output depends on function being called

makeCacheMatrix <- function(x = matrix()) {
        ## set m to null
        ## is not null only when setinv function applied 
        m <- NULL
        ## assign local matrix x equal to the matrix y from calling environment
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## return the local matrix x
        get <- function() x
        ## calculates the inverse matrix m
        setInverse <- function(solve) m <<- solve
        ## returns the inverse matrix m
        getInverse <- function() m
        ## returns list of functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)       
}


## This is a client function that uses "makecacheMatrix" function in its implementation
## Author Bob Stober
## Date: 4/26/2014

## The input is expecting a "special vector" made from makeVector (ignore the ... for now).
## The output is the a matrix that is the inverse of 'x' coming from cache or computation.

cacheSolve <- function(x, ...) {
        ## calls makeCacheMatrix getInv function
        m <- x$getInverse()
        ## if the inverse matrix m is not null then it is in cache can can be returned
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## the inverse matrix m is not in the cache
        ## assign the local matrix data the value of the orginal matrix x
        data <- x$get()
        ## Assign m the value of the inverse
        ## calculate using solve function
        m <- solve(data, ...)
        ## set the inverse matrix in the function makeCacheMatrix the value of m
        x$setInverse(m)
        ## return m
        m
}
