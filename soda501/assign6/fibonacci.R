factorial <- function(n)
{
    if (n==1) { return(1) }
    else { return(n * factorial(n - 1)) }
}

fibonacci <- function(n)
{
    if(n == 0) { return(0) }
    else if(n == 1) { return(1) }
    else{ return(fibonacci(n - 1) + fibonacci(n - 2)) }
}

f0 <- fibonacci(0)
f1 <- fibonacci(1)
f2 <- fibonacci(2)
f5 <- fibonacci(5)
f10 <- fibonacci(10)
f20 <- fibonacci(20)

tests <- all(f0 == 0, f1 == 1, f2 == 1, f5 == 5, f10 == 55, f20 == 6765)
