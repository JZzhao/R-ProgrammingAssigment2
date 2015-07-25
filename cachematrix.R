## makeCacheMatrix: creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                  ##set m as NULL first
  set <- function(y) {
    y<-matrix()
    x <<- y                 ##set() to assign new matrix to x
    m <<- NULL
  }
  get <- function() x       ##cache x
  setsolve <- function(solve) m <<- solve  ##assign new solve() inverse matrix to m
  getsolve <- function() m   ## cache m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)  ## generate a list
  
}


## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the
## cache with"getting cached data". Otherwise, it calculates the inverse of the data and set the value of 
## the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {   ## x is the special "matrix" generated
  m <- x$getsolve()
  if(!is.null(m)) {            
    message("getting cached data")
    return(m)             ## m!=NULL, solve() already calculated and cached
  }                       ## m is the final inverse matrix, just return m
  data<-matrix()          ## m==NULL, new matrix, need caltulate
  data <- x$get()         ## assign new matrix
  m <- solve(data, ...)   ## caltulate inverse
  x$setsolve(m)           ## cache the result
  m
  ## Return a matrix that is the inverse of 'x'
}

