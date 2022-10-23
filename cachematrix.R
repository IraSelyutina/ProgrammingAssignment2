## To avoid expensive calculation of the inverse matrix we use the same inverted matrix for subsequent computations
## (cache it in memory instead of repeatedly calculating the inverse)

## Сreates an R object that stores a matrix and its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## After inserting an argument that is returned by makeCacheMatrix() in order to retrieve the 
## inverse matrix from the cached value that is stored in the makeCacheMatrix() object's environment

cacheSolve <- function(x, ...) {
 m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
       }
