## cachematrix function is use to make solving inverse faster by saving
## the matrix and the inverse in an object, so we can retrieve it significantly
## fast

## makeCacheMatrix is a function containing four function, such as: set(),
## get(), setsolve(), and getsolve(). these function are use to containt
## matrix and its inverse

makeCacheMatrix <- function(x = matrix()) { ## x is initialized as function args
        s <- NULL ## s is initialized ass null, to contain the inverse matrix
        set <- function(y) {
                x <<- y
                s <<- NULL
        } ## set function takes arg. y then assign it to x and clearing s
        get <- function() x ## function to get x
        setsolve <- function(solve) s <<- solve ## function that set the matrix
                                                ## inverse to s
        getsolve <- function() s ## function to retrieve s value
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
        ## list to call the function
}


## cacgeSolve function is use to populate and/or retrieve the inverse

cacheSolve <- function(x, ...) {
        s <- x$getsolve() ## get s from the object and check for inverse,
                          ## if there is inverse, no need calc, return s (cache)
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        mat <- x$get() ## if there is no inverse yet, then getting the matrix
                       ## that saved in the object
        s <- solve(mat) ## solving the inverse
        x$setsolve(s) ## storing the inverse in the object
        s
        ## Return a matrix that is the inverse of 'x'
}
