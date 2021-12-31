
## input x is established as a matrix 
## then set m as a null then all references change to sol 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
  x <<- y
  m <<- NULL
  }
  get <- function()x
  setsol <- function(sol) m <<- sol
  getsol <- function() m
  list(set = set, get = get, 
  setsol = setsol, 
  getsol = getsol)
}

## creates a matrix using the above function references changed to m and sol

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m<- x$getsol()
  if(!is.null(m)){
  message("retrieving cached data")
  return(m)
}
  data <- x$get()
  m<- sol(data,...)
  x$setsol(m)
  m
}


