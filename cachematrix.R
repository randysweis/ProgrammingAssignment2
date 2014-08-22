
## Function generates a matrix that caches inverse, then cache the solution to matrix

makeCacheMatrix <- function(x = matrix()) {      ## Generating the matrix 

        m<-NULL
        set<-function(y){                        ## Set value of matrix
                x<<-y
                m<<-NULL
        }
        get<-function() x                        ## Get value of matrix
        setsolve<-function(solve) m<<- solve     ## Set the value of the inverse
        getsolve<-function() m                   ## Get the value of the inverse
        as.matrix(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## list special "matrix" 

cacheSolve <- function(x, ...) {                 ## Computes the inverse of the special "matrix"
        
        m<-x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get
        m<-solve(matrix, ...)
        x$setsolve(m)
        m
}