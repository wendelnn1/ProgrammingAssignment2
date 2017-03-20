## cahematrix.R computes the invers of a square matrix using cache designators to avoid potentially time-consuming computations.


## 1) Sets the value of the matrix, 2) gets the value of matrix, 3) sets the value of the inverse, 4)gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function()x
    setinv<-function(inverse) m<<-inverse
    getinv<-function()m
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}


##calculates inverse of matrix set by makeCacheMatrix function and will get if already been found
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    m<-x$getinv()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
data<-x$get()
m<-solve(data,...)
x$setinv(m)
m
}