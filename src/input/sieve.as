--
-- sieve.as: A prime number sieve to count primes <= n.
--
#include "fricas"

N ==> NonNegativeInteger;
import from Boolean, N, Integer;

sieve(n: N): N  == {
        isprime: PrimitiveArray Boolean := new(n+1, true);

        np: N := 0;
        two: N := 2;
        for p in two..n | isprime(p::Integer) repeat {
                np := np + 1;
                for i in two*p..n by p::Integer repeat {
                        isprime(i::Integer) := false;
                }
        }
        np
}
