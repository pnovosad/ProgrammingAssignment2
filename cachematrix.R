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
    
    inv = NULL
    set = function(y) {
        # use `<<-` to assign a value to an object in an environment 
        # different from the current environment. 
        x <<- y
        inv <<- NULL
    }
    get = function() x
    setinv = function(inverse) inv <<- inverse 
    getinv = function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv) 
    
    

}


## cacheSolve(): computes the inverse of the matrix returned by makeCacheMatrix().
## if the inverse has already been calculated and the matrix has not changed, it will
## retrieve the inverse from the cache directly

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
