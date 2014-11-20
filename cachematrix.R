
##function makeCacheMatrix is the function which computates or stores inverse matrix
##

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ##every time when function is used the value for m is set for NULL

        set <- function(y) {
                x <<- y
                m <<- NULL
                
        }
        get <- function(){x} ##function returns the value of original matrix
        setinverse <- function(solve) {m<<-solve} ##if accessed, function stores the the value of inverse matrix
        getinverse <- function(){m}
        list(set=set, get=get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function checks if the inverse matrix was computed before and, if yes, returns the value
## of the matix, or, if no, calls for setinverse method form makeCacheatrix function, which 
##computes the values ofonverse matrix

cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m)){        ##if the mean was computed before(x$getinverse is not empty), the function will return the message and the value that was computed before
                message("getting cached data")
                return(m)
        }
        data<-x$get()##if the inverse matrix was not computed before, function will take the matrix, store it as a variable data, evaluate inversematrix and store it in the function makeCacheMatrix
        m <- solve(data, ...)
        x$setinverse(m)
        m
}## Return a matrix that is the inverse of 'x'
