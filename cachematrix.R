## Given that finding the inverse of a matrix is a computationally demanding 
## process, it is worth our time to cache the result if you intend to reuse
## the result. The following two functions make us of caching to first test
## if an existing result exists, then to create the inverse if it doesn't.

## m"akeCacheMatrix" is a function that creates a list of functions used in the
## next fucntion. The functions stored: "set" changes the data in "x"; "get"
## simply returns the value in "x"; similar to these, "setinv" takes the value
## "solve" and stores it to "m", while "getinv" recalls the value in "m".

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


## "cacheSolve" is where the work is done. First, it calls a result from "getinv"
## and stores it as "m"; second, it tests if this value is NULL or has a result to
## return; third, if there is a NULL value in "m", it proceeds to use other functions 
## in "makeCacheMatrix" to draw in data, find the inverse of the matrix, and set the 
## value into "m", replacing the NULL value; finally it prints the inverse in 
## object "m".

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
