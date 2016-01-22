##cachematrix.r   1/21/2016 Susan Fraser
## makeCacheMatrix function
##   -- create a special 'matrix' object that can cache its inverse.
##
## cacheSolve function
##   --  computes the inverse of the special "matrix" returned by makeCacheMatrix

##  makeCacheMatrix contains 4 functions: set, get, setmatrix, getmatrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
      # set is a function that changes the vector stored in the main function (makeCacheMatrix)
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
      # get is a function that returns the vector x stored in the main function
        get <- function() x
   
      # store the value of the input in a variable m into the main function makeCacheMtrix
      # (setmean) and return it (getmean)
        setmatrix <- function(x) m <<- solve(x)
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
    }



## cacheSolve will check for the cache of the matrix passed in
## if found it will display 'getting cached data'
## it will then save the matrix passed in

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
            message("getting cached data")
            return(m)
    }
    data <- x$get()
    m <- data           
    r <- x$setmatrix(m)
    r 
    }

