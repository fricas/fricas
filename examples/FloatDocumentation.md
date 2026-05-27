# FriCAS Float Domain Documentation

## Overview
The `Float` domain implements arbitrary-precision floating-point arithmetic. The number of significant digits can be set to an arbitrary value (the default is 20 decimal digits).

## Internal Representation
The underlying representation for floats is **binary**, not decimal. A float is represented as a record of two integers:
- **Mantissa ($m$)**
- **Exponent ($e$)**

The value is calculated as $m \times 2^e$.

### Implications of Binary Base
1.  **Exactness:** Decimal numbers like `0.3` cannot be represented exactly.
2.  **Conversion Loss:** There is a potential loss of accuracy during conversion to decimal for output.
3.  **Display vs. Equality:** To compensate for conversion loss, extra bits ($1 + \lceil \log_2(10^d) \rceil$ bits for $d$ digits) are used internally. Consequently, two numbers that appear identical on screen might not be equal.
4.  **Efficiency:** The implementation is most efficient on systems where underlying integers are binary-based (standard for most Lisps).

## Accuracy
- **Rounding:** Arithmetic operations are rounded to the nearest unit in the last place (accurate to within $2^{-bits}$).
- **Functions:** Elementary functions and constants are accurate to one unit in the last place.

## Algorithms
For elementary functions, identities are applied to ensure the Taylor series converges quickly (within $O(\sqrt{n})$ steps).
- **Example:** $exp(x) = exp(x/2)^2$ is used for $exp(1/3)$ to reduce the number of calculations required.
- Summing Taylor series terms is done using **integer operations** to avoid floating-point overhead, reaching better efficiency at low precisions.

## Performance and Complexity
*where $n$ is the number of bits of precision:*

| Operation(s) | Complexity |
| :--- | :--- |
| `*`, `/`, `sqrt`, `pi`, `exp1`, `log2`, `log10` | $O(n^2)$ |
| `exp`, `log`, `sin`, `atan` | $O(\sqrt{n} n^2)$ |

*Note: The other elementary functions are derived from those listed above.*
