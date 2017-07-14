## Put comments here that give an overall description of what your
## functions do
## These functions take advantage of lexical scoping and work 
## together so that matricies that have already been 
## inversed are not calculated again to save on computation
## cost and time.
## They also assume all matrices are inversable.


## Write a short comment describing this function
## The first function basically creates functions that can access
## objects from the cache/global envrionment, then puts them into
## a list so other functions can easily call on the objects in
## the global environment.

makeCacheMatrix <- function(x = matrix()) {
        inv <<- NULL
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


## Write a short comment describing this function
## This function calculates and stores the inverse of matrix x,
## but if the inverse had already been calculated,
## then it just retrives the cache inverse matrix.
## This function calls on functions in the list created by
## the first makeCacheMatrix() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
