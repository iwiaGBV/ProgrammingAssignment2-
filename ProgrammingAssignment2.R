#Programming Assignment 2: Lexical Scoping
#Write the following functions makeCacheMatrix and cacheSolve

makeCacheMatrix <- function( m = matrix() ) {
       i <- NULL
       set <- function( matrix )   {
              m <<- matrix
              i <<- NULL
       }
       
       get <- function() {}
       
       setInverse <- function(inverse) {
              i <<- inverse
       }
       
       getInverse <- function() {
              i
       }
       
       list(set = set, get = get,
            setInverse = setInverse,
            getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
       m <- x$getInverse()
       
       if( !is.null(m) ) {
              message("acquiring cached data")
              return(m)
       }
       
       data <- x$get()
       m <- solve(data) %*% data
       x$setInverse(m)
       inv
}