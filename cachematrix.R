## Put comments here that give an overall description of what your
## functions do
##**The purpose of the below function is to cache the inverse of a matrix, It has two functions
##**1.  makeCacheMatrix() which creats an object to cache the inverse and 2. cacheSolve() which
##**does the computing of inverse matrix. the built in function used to find the inverse is the solve()


## Write a short comment describing this function
##**The below function, has two objects (x,m) and 4 functions (set,get,setinverse and getinverse), 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL        #creating an emp
  set <- function(y){                       #*sets the value of matrix
    x<<-y
    m<<-NULL
  }
  get <- function() x                        #* get the value of matrix
  setinverse <- function(inverse) m<<-inverse   #*set the value of the inverse
  getinverse<- function() m                     #* get the value of the inverse
  
  list(set=set, get=get,                          # given names to funtions inorder to ease reference using "$" from cacheSolve()
       setinverse=setinverse,
       getinverse=getinverse)

}


## Write a short comment describing this function
##** The below function excecutes the inverse of the cached matrix returned by makeCacheMatrix() 
##** it first checks to see if the inverse has been calculated and return it if true,
##** otherwise if not calculated it calculates the inverse using the solve()

cacheSolve <- function(x, ...) {
 m <- x$getinverse()
  if(!is.null(m)){
    message("getting catched data")     
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)      ##calculates the inverse of matrix
  x$setinverse(m)
  m        ## Return a matrix that is the inverse of 'x'
       
}
