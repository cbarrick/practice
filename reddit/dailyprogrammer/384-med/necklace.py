def product(items, p=1):
    for x in items:
        p *= x
    return p

def primes(n):
    seive = set()
    for x in range(2, n+1):
        for p in seive:
            if x % p == 0:
                break
        else:
            seive.add(x)
            yield x

def phi(n):
    a = [p for p in primes(n) if n % p == 0]
    b = [p-1 for p in a]
    return n * product(b) // product(a)

def necklaces(k, n):
    s = sum(phi(a) * k ** (n//a) for a in range(1, n//2 + 1) if n % a == 0)
    s += phi(n) * k
    return s // n
