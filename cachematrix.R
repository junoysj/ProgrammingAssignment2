## Two functions that cache the inverse of a matrix.

## The first function makeCacheMatrix creates a special "matrix" object, which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    ## define the cache m
    m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    ## return the matrix x
    get <- function() x 
    ## assign the inverse of matrix x to m
    setinverse <- function(inverse) m <<- inverse
    ## return the cached inverse of x
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The second function calculates the inverse of the special matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
      
}
