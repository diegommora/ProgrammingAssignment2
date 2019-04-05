## Functions that cache the inverse of a matrix.

## Creates a particular matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        c <- NULL
        set <- function(y){
                x <<- y
                c <<- NULL
        }
        get <- function () x
        setcache <- function(cache) c <<- cache
        getcache <- function() c
        list(set=set, get=get, setcache=setcache, getcache=getcache)
}


## Calculates the inverse of the particular matrix returned 
## by makeCacheMatrix function.If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        c <- x$getcache()
        if(!is.null(c)){
                message("Getting cache data for this operation...")
                return(c)
        }
        data <- x$get()
        c <- solve(data,...)
        x$setcache(c)
        c
}
