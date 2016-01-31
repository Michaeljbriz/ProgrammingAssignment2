## The functions below streamline efforts in computing by storing (Or caching) the inverse of a matrix 
## Will reduce burden and repetition of code 


## Create matrix to store the inverse 

makeCacheMatrix <- function(x = matrix()) {
                inv <-NULL
                set <- function(y){
                        x <<- y
                        inv <<- NULL
                }
                get<-function() x
                setInverse <-function(solve) inv<<- solve
                getInverse <-function() inv
                list(set=set, get=get,
                     setInverse=setInverse,
                     getInverse=getInverse)
}


## Will compute the inverse of the matrix or retrieve the inverse stored from the cache 

cacheSolve <- function(x, ...) {
        inv <-x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        matrix<-x$get()
        inv<-solve(matrix, ...)
        x$setInverse(inv)
        inv
}
