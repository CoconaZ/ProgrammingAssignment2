## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
       s<-NULL
       set<-function(t){
               x<<-t
               s<<-NULL
       }
       get<-function() x
       setinverse <- function(inverse) s <<- inverse
       getinverse <- function() s
       list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
       
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
B <- matrix(c(5,2,8,9),2,2)
B1<-makeCacheMatrix(B)
cacheSolve(B1)


