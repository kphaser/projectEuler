# Sum of multiples of 3 and 5 below 1000
a <- seq(3,1000,by=3)
b <- seq(5,995,by=5)
c <- seq(15,1000,by=15)
sum(a)+sum(b)-sum(c) # answer: 233168

# Sum of even fibonnaci numbers up to value of 4mil
fib <- numeric()
fib[1] <- 1
fib[2] <- 2
i <- 3
repeat {
    fib[i] <- fib[i-1] + fib[i-2]
    if (fib[i] > 4e6) break
    i <- i +1
}

fib <- fib[-length(fib)]
even <- fib %% 2 == 0
result <- sum(fib[even])
result

# Find largest prime factor of 600851475143
library(gmp)
factorize(600851475143)

# Find largest palindrome product of two 3-digit numbers
get_digit <- function(num,n) {
    (num %% (10 ^ n)) %/% (10 ^ (n-1))
}

palindrome <- function(num) {
    digits <- floor(log(num,10)) + 1
    for (x in 1:((digits %/% 2))) {
        digit1 <- get_digit(num,x)
        digit2 <- get_digit(num,(digits+1)-x)
        if (digit1 != digit2)
            return(FALSE)
    }
    return(TRUE)
}

biggest_palindrome <- function() {
    best <- 0
    for (x in 100:999) {
        for (y in 100:999) {
            candidate <- x*y
            if (candidate > best && palindrome(candidate)) {
                best <- candidate
            }
        }
    }
    best
}
