## PA2 week 3 by pn, 20180317

## <- : single arrow assignment operator works at the current level
## <<-: double arrow assignment operator can modify variables in parent levels
## a closure is a function written by/in another function



## makeCacheMatrix(): creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## @x: a square invertible matrix
    ## return: a list containing functions to
    ##              1. set the matrix
    ##              2. get the matrix
    ##              3. set the inverse
    ##              4. get the inverse
    ##         this list is used as the input to cacheSolve()
    
    mInverse = NULL
    set = function(y) {
        # use `<<-` to assign a value to an object in an environment different from the current environment. 
        x <<- y
        mInverse <<- NULL
    }
    get = function() x
    setInverse = function(inverse) mInverse <<- inverse 
    getInverse = function() mInverse
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}



## cacheSolve(): computes the inverse of the matrix returned by makeCacheMatrix().
## if the inverse has already been calculated and the matrix has not changed, it will
## retrieve the inverse from the cache directly

cacheSolve <- function(x, ...) {
    ## @x: output of makeCacheMatrix()
    ## return: inverse of the original matrix input to makeCacheMatrix()
    
    mInverse = x$getInverse()
    
    # if the inverse matrix exists then return the cached matrix
    if (!is.null(mInverse)){
        message("returning cached matrix data ...")
        return(mInverse)
    }
    
    # otherwise, calculates the inverse 
    matrix.data = x$get()
    mInverse = solve(matrix.data, ...)
    
    # sets the value of the inverse matrix in the cache by using the setInverse function.
    x$setInverse(mInverse)
    
    return(mInverse)
}


