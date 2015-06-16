## These two functions will allow a user to recall a cached value of an
## inverted matrix or, if no value has been stored, invert the matrix and
## print the result

## The makeCacheMatrix function creates a matrix that can take an inverse
## of a matrix and cache its value
makeCacheMatrix <- function(x = matrix()) {

    m <- NULL                               # m is an object that will hold the inverse of a matrix
    set <- function(y) {                    # create a new function called "set"
        x <<- y                             # that stores the value of y to the input matrix x
        m <<- NULL                          # restore object m to empty
    }
    
    get <- function() x                     # create a new function called "get" that returns the value of x
    setinverse <- function(solve) m <<- solve      # function "setinverse" stores inverse of a matrix in m
    getinverse <- function() m                     # function "getinverse" prints the value of m
    # store the four subfunctions in the main function
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The cacheSolve function checks whether the inverse of a given matrix has
## been cached. If so, it retrieves the cached value of the inverse. If not,
## it computes the inverse of the input matrix.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()                # assign the cached inverse of the function to the object m
    if(!is.null(m)) {                  # check whether object m is NOT empty
        message("getting cached data") # if m is not empty, print message
        return(m)                      # and print the value of the inverse of the matrix
    }
    
    data <- x$get()                    # if m is null, get value of matrix, assign to object called "data"
  
    m <- solve(data, ...)              # invert the matrix stored in "data"
    x$setinverse(m)                    # store the result in object m
    
    m                                  # Return a matrix that is the inverse of 'x'
     
}
