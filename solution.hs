import Data.List.Ordered (minus)

primes :: Int -> [Int] -- Sieve of Eratosthenes
primes n = eratos [2..n]
	where
		eratos [] = []
		eratos (p:xs) = p : eratos (minus xs [p*p, p*p+p..n])

factor :: Int -> [Int]
factor n = [ m | m <- primes (div n 2), mod n m == 0]

number = 600851475143
answer = factor number

main = print answer
