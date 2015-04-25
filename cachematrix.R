## Given that finding the inverse of a matrix is a computationally demanding 
## process, it is worth our time to cache the result if you intend to reuse
## the result. The following two functions make us of caching to first test
## if an existing result exists, then to create the inverse if it doesn't.

##makeCacheMatrix is a function that creates a list of 

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinv<-function(solve) m <<-solve
  getinv<-function()m
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function

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
