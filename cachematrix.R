## The function will reduce the computation time by using lexical scoping of R


## Below function will create sepcial matrix and save rresult in cache

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL
set<-function(y){               #setting value for result variable
        x <<- y
        inv <<- NULL
}
get <- function() x             #get value
setinverse<- function(inverse) inv <<- inverse 
getinverse<- function() inv

list(set = set, get=get,setinverse=setinverse,getinverse=getinverse)
}


## Compute the inverse of the matrix returned by the "makeCacheMatrix" function

cacheSolve <- function(x, ...) {
        inv <- x$getinverse() 
        if(!is.null(inv)){              
                print("getting cached data")
                return(inv)
        }
data<-x$get()
inv<-solve(data) %% data
x$setinverse(inv)
inv               # Return a matrix that is the inverse of 'x'
}
