## set the value of the matrix
## get the value of the matrix
##set the value of the inverse
##get the value of the inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y) {
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setinverse<-function(inverse) m<<-solve(inverse)
        getinverse<-function()m
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
                                       
}


## compute the inverse of the matrix created with the above function. If the inverse has already been calculated,it get the inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
        
}
        ## Return a matrix that is the inverse of 'x'

