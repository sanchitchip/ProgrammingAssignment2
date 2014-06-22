## Put comments here that give an overall description of what your
## functions do
##The first function, makeVector creates a special "vector", which is really a list containing a function to
##set the value of the Matrix
##get the value of the Matrix
##set the value of the inverse Matrix
##get the value of the inverse Matrix

makeCacheMatrix <- function(x = matrix())
{m <- NULL
 set <- function(y) 
 {
   x <<- y
   m <<- NULL
 }
 get <- function() x
 setinverse <- function(inverse) m <<- inverse
 getinverse <- function() m
 list(set = set, get = get, setinverse = setinverse,getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above.If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  Solve <- function(x)
  { i <- 1
    j <- 1 
    k <- nrow(x)
    for ( i in 1:k)
    { for(j in 1:k)
    {x[i,j] <- (-1)^(i+j)*det(x[-i,-j])
    }
    }
    return(x)
  }
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- Solve(data, ...)
  x$setinverse(m)
  m
}
## Completiondone