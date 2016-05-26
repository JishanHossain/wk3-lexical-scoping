
# Caching the Inverse of a Matrix: 
# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than compute it repeatedly.

# Below are a pair of functions that are used to create a special object that
# stores a matrix and caches its inverse.

# This function creates a special "matrix" object that can cache its inverse.

# makeCacheMatrix creates a list containing a function to

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) 
         inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# The follwoing function computes the inverse of the special "matrix" created by
# makeCacheMatrix shown above. It first checks if the inverse has already been 
# computed (and the matrix has not changed). 
# If not, it computes the inverse, sets the value in the cache via 
# the setinverse function.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}


# Sample testing: 

sample_matrix <- makeCacheMatrix(matrix(5:8, 2, 2))

sample_matrix$get()

sample_matrix$getInverse()

cacheSolve(sample_matrix)

cacheSolve(sample_matrix)

sample_matrix$getInverse()

> # Sample testing: 
> 
> sample_matrix <- makeCacheMatrix(matrix(5:8, 2, 2))
> 
> sample_matrix$get()
     [,1] [,2]
[1,]    5    7
[2,]    6    8
> 
> sample_matrix$getInverse()
NULL
> 
> cacheSolve(sample_matrix)
     [,1] [,2]
[1,]   -4  3.5
[2,]    3 -2.5
> 
> cacheSolve(sample_matrix)
getting cached data
     [,1] [,2]
[1,]   -4  3.5
[2,]    3 -2.5
> 
> sample_matrix$getInverse()
     [,1] [,2]
[1,]   -4  3.5
[2,]    3 -2.5
> 
