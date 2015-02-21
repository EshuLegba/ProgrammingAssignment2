## cachematrix.R
#####################################################
# makeCacheMatrix() is used in conjunction with 
# cacheSolve() to cache the inverse of matrix
# by taking advantage of lexical scoping
#
# Usage:
# 
# assign makeCacheMatrix to a variable f
# f <- makeCacheMatrix(x)
# If you have a matrix m1
# pass the matrix to makeCacheMatrix()
# f$set(m1)
# view the passed matrix
# f$get()
# compute the inverse of m1 using cacheSolve()
# cacheSolve(f)
# if you call cacheSolve(f) a second time it will
# retrieve the value from cache.
# note: this function, as per assignment instructions,
#       assumes that the passed matrix is invertible.

# the function operates on invertible matrices

## Write a short comment describing this function
# makeCacheMatrix() allows you to pass it a matrix that
# you want to compute its inverse and is used in conjunction
# with CacheSolve() to cache the inverse so it doesn't have
# to be computed each time you need it.

makeCacheMatrix <- function(x = matrix()) {
    # initialize the solved matrix to NULL    
    s <- NULL
    # set() receives the passed matrix and resets
    # the previous solved matrix
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    # get() returns the passed matrix
    get <- function() x
    # setsolve() receives the solved matrix
    setsolve <- function(solve) s <<- solve
    # getsolve returns the solved matrix
    getsolve <- function() s
    # return a list of functions    
    list(set=set, get=get,setsolve=setsolve, getsolve=getsolve)
}


## Write a short comment describing this function
# CacheSolve() is used in conjunction with 
# makeCacheMatrix() to cache the inverse of matrix
# by taking advantage of lexical scoping

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    #S is set to the inverse of the passed matrix
    s <- x$getsolve()
    # if s already exists, retrieve the cached inverse
    # and return
    if(!is.null(s)){
        message("getting cached data")
        return(s)
    }
    # if s is not already cached, compute the inverse
    data <- x$get()
    s <- solve(data,...)
    x$setsolve(s)
    # return the inverse of the passed matrix
    s
}
