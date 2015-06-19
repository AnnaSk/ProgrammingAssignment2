## The aim of the functions is to cache the value of the inverse matrix to the given one while it is calculated for the first time, so that this calculation is not repeated unnecessarly any time when the same matrix has to be inverted.

## Function makeCacheMatrix takes as the argument the matrix and creates from it a list with 4 elements. 
## The aim is to create the adequate structure to handle the invertion of matrix by function cacheSolve
## After the function is run, it has the value of the orginal matrix (x) in x$get()

makeCacheMatrix <- function(x = matrix()) {
        slv <- NULL
        set <- function(y) {
                x <<- y
                slv <<- NULL
        }
        get <- function() x
        ## the oryginal matrix x is stored here
        setsolve <- function(solve) slv <<- solve
        getsolve <- function() slv
        # the inverted matrix x is stored here
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function uses the list structure xx created for the matrix by function makeCacheMatrix and checks wheather the inversion of the matrix can be found in x$getsolve()
## if not, it takes the data from x$get() and calculates the inverted matrix. The value is then stored in the x$setsolve so that if the function is run again the value of inverted matrix can be found in x$getsolve()

cacheSolve <- function(xx, ...) {

        slv <- xx$getsolve()
        if(!is.null(slv)) {
                message("getting cached data")
                return(slv)
        }
        data <- xx$get()
        slv <- solve(data, ...)
        xx$setsolve(slv)
        slv
        ## Matrix slv is the inverse of oryginal matrix 'x'
}
