## Caching the Inverse of a Matrix function

## The first function, makeCacheMatrix creates a special "matrix", 
## Which is really a list containing a function to
## 1. Set value of the matrix
## 2. Get value of the matrix
## 3. Set the value of the solving
## 4. Get the value of the solving

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y){ 
        x <<- y
        s <<- NULL
    }
    get <- function() x 
    setsolve <- function(solve) s <<- solve 
    getsolve <- function() s 
    list(set = set, get = get, 
         setsolve = setsolve, 
         getsolve = getsolve)
}


## The following function calculates the inverse matrix of the special "matrix" 
## created with the above function. 
## However, it first checks to see if the solve has already been calculated. 
## If so, it gets the solve from the cache and skips the computation. 
## Otherwise, it calculates the solve of the data and sets the value 
## of the solve in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)){ ## Verify the value s
        message("getting inversed matrix")
        return(s)
    }
    data <- x$get()
    s <- solve(data)
    x$setsolve(s)
    s ## Return a matrix that is the inverse of 'x'
}


## Cheers! Your friend from the north, Mike
