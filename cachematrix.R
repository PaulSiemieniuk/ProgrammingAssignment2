## this is a series of two functions that work together to cache inverses for matrices
## makeCacheMatrix is used to create the matrix and it has functions to return the matrix and its inverse


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #set i to NULL in makeCacheMatrix function environment
  set <- function(y) { #start function set
    x <<- y #set matrix x to matrix y in set function environment
    i <<- NULL #reset i in set function environment  
  }
  get <- function() x #returns matrix x 
  setinverse <- function(solve) i <<- solve #sets i to inverse in different environment
  getinverse <- function() i #returns inverse i
  list(set = set, get = get, #returns 'special matrix' containing functions
       setinverse = setinverse, #returns 'special matrix' containing functions
       getinverse = getinverse) #returns 'special matrix' containing functions
}

## this function uses the cached inverse if it exists. otherwise it calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() # calls getinverse function from makeCacheMatrix()
  if(!is.null(i)) { #if i is not null then used cached inverse
    message("getting cached data")
    return(i)
  }
  data <- x$get() #if inverse is not cached, call matrix for inverse calculation
  i <- solve(data, ...) #inverse calculation
  x$setinverse(i) #sets inverse
  i #returns inverse
}
