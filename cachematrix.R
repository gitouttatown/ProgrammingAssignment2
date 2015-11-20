## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        #initialize our local "inverse" matrix value m
        m <- NULL
        ## The set function assigns the value you give it, to the x variable in
        ## the parent function
        set <- function(y) {
                        x <<- y
                        m <<- NULL
        }
        ## get simply returns the current x value
        get <- function()x
        ## the set_m function takes the value you give it, and assigns that to 
        ## the 
        set_m <- function(inverse) m <<- inverse
        get_m<-function() m 
        list(set_m = set_m, get_m = get_m, set = set, get = get)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_m()
        if (!is.null(m)){
                message("getting cached inverse of the matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        ## the following step stores the inverse matrix just calculated,
        ## into the variable within our list object
        x$set_m(m)
        m
}
