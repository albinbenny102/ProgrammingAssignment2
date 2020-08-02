makeCacheMatrix <- function(x=matrix())  #function to take input matrix
{
  
  inv<-NULL     #NULL Object to var inv
  set <-function(y){     #set value of matrix using function y
    x<<-y     #assignment operator parent function makeCache matrix have acces to this funtion
    inv<-NULL
  }
  get<-function(){x}  #get value of matrix
  setInverse<-function(inverse){inv<<-inverse}  #set value of inverse to setInverse
  getInverse<-function(){inv}                 #get value of Inverse
  list(set = set, get = get, setnInverse = setInverse, getInverse = getInverse)   #creating list 
}

#Inverse of matrix 
cacheSolve<-function(x,...){
  inv<-x$getInverse()  #returns inverse of matrix and assign to inv
  if(!is.null(inv)){     #if already done computation
    message("is there")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)   #compute inverse 
  x$setInverse(inv)    #set value in cache 
  inv
}
