# Enter the list
x_prob <- scan()


### get the mean  ###
Mean_function <- function(x) {
  z <- 0
  for (s in seq_along(x)) {
    
    z <-   z + x[s]
    
  }
   z/length(x)

}

### get the median ###
Median_function <- function(x){
  n <- length(x)
  Sorted_list <- sort(x)
  index = floor(n / 2)
  if( n %% 2 == 0){
    
    (Sorted_list[index] + Sorted_list[index + 1]) / 2
  }
  else{
    Sorted_list[index + 1]
  }
}
### get the standard deviation ###
st_function <- function(x){
  
  count <- 0
  for( i in x){
    count = count + (i - Mean_function(x)) ^ 2
  }
   sqrt(count / length(x))
}


### get the Range ###

range_function <- function(x){
  minimum = min(x)
  maximum = max(x)
  
  maximum - minimum

}

print(paste("the mean:",Mean_function(x_prob)))
print(paste("the median:",Median_function(x_prob)))
print(paste("the standard deviation:",st_function(x_prob))) 
print(paste("the range:",range_function(x_prob)))


