fib <- function(n=20){
  if (n<3) {
    return(c(1,1))
  } else {
    fib <- c(NA)
    fib[1] <- fib[2] <- 1
    for (i in 3:n) {
      fib[i] = fib[i-1] + fib[i-2]
    }
    return(fib)
  }
}