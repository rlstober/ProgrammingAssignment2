## This function creates a special "matrix" object that can cache its inverse
## Author Bob Stober
## Date: 4/26/2014

## input is numeric matrix
## output depends on function being called
makeCacheMatrix <- function(x = numeric()) {
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
        setinv <- function(solve) m <<- solve
        ## returns the inverse matrix m
        getinv <- function() m
        ## returns list of functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}