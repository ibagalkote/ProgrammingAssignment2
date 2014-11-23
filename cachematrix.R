# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse
# 4. get the value of inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been calculated. If so, it gets the result and skips the
# calculated. If not, it calculates the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}

# Test Run
# > x = rbind(c(1, 5), c(5, 1))
# > m = makeCacheMatrix(x)
# > m$get()
#      [,1] [,2]
# [1,]    1    5
# [2,]    5    1
# > cacheSolve(m)
#             [,1]        [,2]
# [1,] -0.04166667  0.20833333
# [2,]  0.20833333 -0.04166667
# > cacheSolve(m)
# getting cached data.
#             [,1]        [,2]
# [1,] -0.04166667  0.20833333
# [2,]  0.20833333 -0.04166667
# > 
