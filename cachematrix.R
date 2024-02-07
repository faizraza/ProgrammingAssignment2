## Below is the function to make cache matrix
makeCacheMatrix <- function(x = matrix()){
        inv <- NULL                 #initialize inverse as NULL
        get <- function()x          #function to get matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
                }
        getinverse <- function() inv
        setinverse <- function (inverse){
                inv <<- inverse
                } 
        return(list(
                set = set, 
                get = get, 
                setinverse = setinverse, 
                getinverse = getinverse
             ))
        }

##Function below is used to get cache data
cacheSolve <- function(x,...) {
        inv <- x$getinverse()
        if(!is.null(inv)){  
                message("getting cached data")
                return(inv)
                }
        m <- solve(x$get())
        x$setinverse(m)
        #return(m)
        }

##Testing
a <- rbind(c(10,11,12),c(11,11,11),c(-14,12,11))
b <- makeCacheMatrix(a)
identical(b$get(), a)
b$getinverse()
cacheSolve(b)
b$getinverse()
