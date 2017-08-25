
## Here we have two functions that we use to cache the inverse of a matrix
## The first function sets the value and gets the value of matrix 
## as well it's inverse


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The second function returns the value of the inverse
## It checks whether inverse has already been 
## computed or not. If yes,it uses the previous value
## otherwise computes the value again and sets in the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
