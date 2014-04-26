## This is a client function that uses "makecacheMatrix" function in its implementation
## Author Bob Stober
## Date: 4/26/2014

## The input is expecting a "special vector" made from makeVector (ignore the ... for now).
## The output is the mean coming whether from the special vector's  cache or computation.

cachesolve <- function(x, ...) {
        ## calls makeCacheMatrix getInv function
        m <- x$getinv()
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
        x$setinv(m)
        ## return m
        m
}