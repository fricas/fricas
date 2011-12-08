#include <gmp.h>

#if defined(__x86_64__)
  #define BIGNUM_TAG 7
  #define BIT_CNT 64
#else
  #define BIGNUM_TAG 3
  #define BIT_CNT 32
#endif
#define WORD_PTR_TYPE unsigned long *
#define TO_WORD_PTR(x) ((WORD_PTR_TYPE)((x) - BIGNUM_TAG))
#define BIGNUM_LENGTH(x) ((x)[-1] >> 8)

void
gmp_wrap_sb_mul(char * n1l, char * n2l, char * resl)
{
    WORD_PTR_TYPE n1 = TO_WORD_PTR(n1l);
    mp_size_t l1 = BIGNUM_LENGTH(n1);
    WORD_PTR_TYPE n2 = TO_WORD_PTR(n2l);
    mp_size_t l2 = BIGNUM_LENGTH(n2);
    WORD_PTR_TYPE res = TO_WORD_PTR(resl);
    mpn_mul(res, n1, l1, n2, l2);
}

void
gmp_wrap_sb_div_rem(char * n1l, char * n2l, char * quol, char * reml)
{
    WORD_PTR_TYPE n1 = TO_WORD_PTR(n1l);
    mp_size_t l1 = BIGNUM_LENGTH(n1);
    WORD_PTR_TYPE n2 = TO_WORD_PTR(n2l);
    mp_size_t l2 = BIGNUM_LENGTH(n2);
    WORD_PTR_TYPE quo = TO_WORD_PTR(quol);
    WORD_PTR_TYPE rem = TO_WORD_PTR(reml);
    mp_size_t lr = BIGNUM_LENGTH(rem);
    if (n2[l2 - 1] == 0) {
       l2 = l2 - 1;
       rem[lr - 1] = 0;
    }
    mpn_tdiv_qr(quo, rem, 0, n1, l1, n2, l2);
}

void
gmp_wrap_isqrt(mp_limb_t *res, mp_size_t l2, mp_limb_t *n1, mp_size_t l1)
{
    if (n1[l1 - 1] == 0) {
        l1--;
        res[l2 - 1] = 0;
    }
    mpn_sqrtrem(res, 0, n1, l1);
}


void
gmp_wrap_sb_isqrt(char * n1l, char * resl)
{
    WORD_PTR_TYPE n1 = TO_WORD_PTR(n1l);
    mp_size_t l1 = BIGNUM_LENGTH(n1);
    WORD_PTR_TYPE res = TO_WORD_PTR(resl);
    unsigned long l2 = BIGNUM_LENGTH(res);
    gmp_wrap_isqrt(res, l2, n1, l1);
}

static int
count_zeros(mp_limb_t x)
{
    int res = 0;
#if BIT_CNT == 64
    if (!(x & ((1l<<32) - 1))) {
        res = 32;
        x >>= 32;
    }
#endif
    if (!(x & ((1l<<16) - 1))) {
        res += 16;
        x >>= 16;
    }
    if (!(x & ((1l<<8) - 1))) {
        res += 8;
        x >>= 8;
    }
    if (!(x & ((1l<<4) - 1))) {
        res += 4 ;
        x >>= 4;
    }
    if (!(x & ((1l<<2) - 1))) {
        res += 2;
        x >>= 2;
    }
    if (!(x & 1)) {
        res += 1;
    }
    return res;
}

/* More sane version of gmp gcd: arguments are arbitrary positive
   numbers, fills rp with the result and returns the length */
mp_size_t
gmp_wrap_gcd(mp_limb_t * rp, mp_limb_t *s1p,
       mp_size_t s1n, mp_limb_t *s2p, mp_size_t s2n)
{
    mp_limb_t rc;
    mp_size_t res;
    mp_size_t k1;
    mp_size_t k;
    mp_size_t z1l;
    mp_size_t z2l;
    mp_size_t zp;
    int z1;
    int z2;
    k1 = 1;
    while(k1 <= s1n && s1p[s1n - k1] == 0) k1++;
    s1n -= (k1 - 1);
    k1 = 0;
    while(k1 < s1n && s1p[k1] == 0) k1++;
    s1p += k1;
    s1n -= k1;
    z1l = k1*BIT_CNT;
    if (s1n == 0) {
        *rp = 0;
        return 0;
    }
    k1 = 1;
    while(k1 <= s2n && s2p[s2n - k1] == 0) k1++;
    s2n -= (k1 - 1);
    k1 = 0;
    while(k1 < s2n && s2p[k1] == 0) k1++;
    s2p += k1;
    s2n -= k1;
    z2l = k1*BIT_CNT;
    if (s2n == 0) {
        *rp = 0;
        return 0;
    }
    z2 = count_zeros(*s2p);
    if (z2 > 0) { mpn_rshift(s2p, s2p, s2n, z2); };
    if (s2p[s2n - 1] == 0) {
        s2n--;
    }
    z1 = count_zeros(*s1p);
    if (z1 > 0) { mpn_rshift(s1p, s1p, s1n, z1); };
    if (s1p[s1n - 1] == 0) {
        s1n--;
    }
    z1l += z1;
    z2l += z2;
    if (s2n > s1n || ((s2n == s1n) && s2p[s2n - 1] > s1p[s1n -1])) {
        mp_size_t tmp = s2n;
        mp_limb_t * ptmp = s2p;
        s2n = s1n;
        s2p = s1p;
        s1n = tmp;
        s1p = ptmp;
    }
    zp = (z1l < z2l)? z1l : z2l;
    k = zp / BIT_CNT;
    zp = zp % BIT_CNT;
    for(k1 = 0; k1 < k; k1++) {
        rp[k1] = 0;
    }
    rp += k;
    res = mpn_gcd(rp, s1p, s1n, s2p, s2n);
    rc = (zp > 0) ? mpn_lshift(rp, rp, res, zp) : 0;
    if (rc) {
        rp[res] = rc;
        res++;
    }
    if (rp[res - 1] & (1ul << (BIT_CNT - 1))) {
        rp[res] = 0;
        res++;
    }
    return res+k;
}

void
gmp_wrap_sb_gcd(char * n1l, char * n2l, char * resl)
{
    WORD_PTR_TYPE n1 = TO_WORD_PTR(n1l);
    mp_size_t l1 = BIGNUM_LENGTH(n1);
    WORD_PTR_TYPE n2 = TO_WORD_PTR(n2l);
    mp_size_t l2 = BIGNUM_LENGTH(n2);
    WORD_PTR_TYPE res = TO_WORD_PTR(resl);
    mp_size_t rl0 = BIGNUM_LENGTH(res);
    mp_size_t rl1 = gmp_wrap_gcd(res, n1, l1, n2, l2);
    mp_size_t i;
    for(i=rl1; i<rl0; i++) {
        res[i] = 0;
    }
}
