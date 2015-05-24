## Ivan F. Gonzalez Data Science Programming R, Assigment 2

## This function creates a special "matrix" object that can cache its inverse.
## I copied the MakVector function and renamed variables to improve readability

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x 
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
## I copied cachemean and renamed variable names and changed operation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Computing the inverse of a square matrix can be done with the `solve`
        ## function in R. 
        ## For this assignment, assume that the matrix supplied is always
        ## invertible.


        m <- x$getinv()
        if(!is.null(m)) { # If getinv is not null (already calculated) read inv.
                message("getting cached data")
                return(m)
        }
        data <- x$get() # if innverse is empty then calculate inverse
        m <- solve(data, ...)
        x$setinv(m) # sets the inverse in the cache via the `setinv` function.
        return(m)        
        
}

