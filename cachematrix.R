## Together these functions are able to calculate and cache the inverse of a matrix defined by the user. 
## Inverting matricies can be time consuming, therefore caching of the solution improves program efficiency
## by preventing redundant calculations of matrix inverses. 

## makeCacheMatrix is a function that returns a list consisting of 4 functions:
## set- used for resetting the matrix, running set will ensure that cacheSolve recalculates the inverse
##      i.e., just run matrix_name$set(newmatrix) to reset variable
## getinverse- gets the current inverse that is cached
## list-outputs the list of functions

## takes argument of matrix to be inverted

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## cacheSolve - used to solve the inverse if not already cached. Skips solving for inverse if already cached
## takes argument that is a variable produced by the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
