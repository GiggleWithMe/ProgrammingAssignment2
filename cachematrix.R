## Given that finding the inverse of a matrix is a computationally demanding 
## process, it is worth our time to cache the result if you intend to reuse
## the result. The following two functions make us of caching to first test
## if an existing result exists, then to create the inverse if it doesn't.

## "makeCacheMatrix" is a function that creates a list of functions used in the
## next fucntion. The functions stored: "set" changes the data in "x"; "get"
## simply returns the value in "x"; similar to these, "setinv" takes the value
## "solve" and stores it to "m", while "getinv" recalls the value in "m".

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    ## For this to work, it requires storage not in the current environment but
    ## in the environment of the calling function (the parent environment).
    x<<-y
    m<<-NULL
  }
  get<-function() x
  ## See previous comment, the value "m" is stored in the parent environment.
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
  ## This draws data into "m", which will be an existing inverse or NULL.
  m<-x$getinv()
  ## This tests if the value is NULL or a value worth returning. If it already has
  ## a value in m (regardless of it is the sought answer), it will print "getting 
  ## cached data" instead of recomputing an answer.
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  ## The following lines compute the inverse of a matrix, if one hasn't already
  ## been created.
  data<-x$get()
  m<-solve(data,...)
  x$setinv(m)
  m
}
