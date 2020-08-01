## Assignment: Caching the Inverse of a Matrix.
## Here are functions that cache the inverse of a matrix.

## makeCacheMatrix() creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverted <- function(m_inverted) m <<- m_inverted
    getinverted <- function() m
    list(set = set, get = get,
         setinverted = setinverted,
         getinverted = getinverted)
    
}


## cacheSolve() returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    m <- x$getinverted()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverted(m)
    m
}