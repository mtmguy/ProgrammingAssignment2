## These functions calculate the inverse of a matrix, cache that value, and return
## that value if it exists rather than recalculating the inverse again.
##makeCacheMatrix is a function that can set the value of the matrix, 
## get the value of the matrix,set the value of the inverse, and 
## get the value of the inverse
## m indicates whether the inverse has been cached.  for m<-NULL, the matrix inverse
## has not been cached and will be calculated
makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}
cachesolve <- function(x, ...) {
    m <- x$getinv()
    ##if the value of m is not "null", the cached solution is used
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ##if the value of m is "null" (a cached solution does not exist), 
    ##the inverse of the matrix is calculated using the solve function
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}