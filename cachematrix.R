
## calculates and caches the inverse of a matrix 


## makeCacheMatrix creates a matrix object that can cache a matrix 
## and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve returns a matrix that is the inverse of x. It retrives 
## the cached inverse or calculates it if it is not cached.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}

## example usage

mat <- matrix(c(-2,2,3,-2),2,2)
a <- makeCacheMatrix()
a$set(mat)
a$get()
cacheSolve(a)
a$getinv()
cacheSolve(a)

