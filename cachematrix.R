#R Programming Week 3 Programming Assignment 2: Lexical Scoping
#Assignment: Caching the Inverse of a Matrix

---------------------------------------------
        ## Overall Description ## -------------------------------------------------------
## Together, makeCacheMatrix and cacheSolve calculate and caches the inverse of an 
## input matrix in order to save the inverse to memory. This allows the inverse
## to be accessed in future code without having recalculate it

## makeCacheMatrix ## -----------------------------------------------------------
## makeCacheMatrix contains four functions. 1) set() defines the value of the input
## matrix. This value can be changed by calling the set() function after
## the makeCacheMatrix function has been run for a matrix. 2) get() retrieves
## the value of the matrix and shows the contents when called up independently.
## 3) setInverse() sets the value of the inverse matrix. 4) getInverse() retrieves
## the inverse matrix for the input matrix. Defining i as NULL first creates an 
## object to be used later in the function, and second resets i as NULL in case
## a previous execution of makeCacheMatrix has cached a value for i.
## Finally, every function is named as such in the final line of makeCacheMatrix 
##code so the individual functions can be called upon independently. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL 
        set <- function(y) { 
                x <<- y
                i <<- NULL 
        }
        get <- function() x 
        setInverse <- function(solve) i <<- solve 
        getInverse <- function() i 
        list(set = set, get = get, 
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve ## -------------------------------------------------------------
## cacheSolve solves for the inverse of an input matrix, which must already be 
## defined by the makeCacheMatrix function. cacheSolve specifically calls upon
## the getInverse function from makeCacheMatrix, and returns a previously cached
## inverse if one has already been calculated for the specified matrix. If the 
## value of is is NULL (there is no cached inverse specified), then cacheSolve
## calculates the inverse and returns it. 

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
