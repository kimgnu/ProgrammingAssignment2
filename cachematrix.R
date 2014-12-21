## Following functions can calculate the inverse of a matrix, and
## cache the data of the inverse and the matrix so that you don't
## need to calculate again if you already have a cached data.

## makeCacheMatrix receives a matrix as an argument and can store
## the matrix and its inverse by functions contained in the returned
## list. The list also has functions, by which you can set a matrix
## and get the matrix or the stored inverse of it.

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set<-function(y){
        x<<-y
        i<<-NULL
    }
    get<-function(){
        x
    }
    setinverse<-function(inverse){
        i<<-inverse
    }
    getinverse<-function(){
        i
    }
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve receives an argument which is a returned list by 
## makeCacheMatrix function. It checks the argument if it has
## already calculated and cached the inverse, then it returns the 
## cached inverse with a message or newly calculated inverse.

cacheSolve <- function(x, ...) {
    i<-x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data<-x$get()
    i<-solve(data)
    x$setinverse(i)
    i    
}
