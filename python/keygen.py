import random
import os.path
import sys

# Generate two large primes
def largePrime():
    while True:
        p = random.randrange(101, 1000, 2)
        if all(p % n != 0 for n in range(3, int((p ** 0.5) + 1), 2)):
            return p

# Find the greatest common denominator between two integers
def gcd(a, b):
        if b > a:
            if b % a == 0:
                return a
            else:
                return gcd(b % a, a)
        else:
            if a % b == 0:
                return b
            else:
                return gcd(b, a % b)


# Compute e value which satisfies 1 < e < phi(n) && gcd(e, phi(n)) == 1
def find_e(n):
    e = 0
    for i in range(2, n):
        if gcd(n, i) == 1:
            e = i
            break
    return e


# Apply Euler's extended GCD alg
def egcd(a, b):
    if a == 0:
        return (b, 0, 1)
    else:
        g, y, x = egcd(b % a, a)
        return (g, x - (b // a) * y, y)


# Find the modular inverse
def modinv(a, m):
    gcd, x, y = egcd(a, m)
    if gcd != 1:
        return None
    else:
        return x % m


def main():

    # Take in command line arguments
    private = sys.argv[1]
    public = sys.argv[2]

    # Set prime values
    p = largePrime()
    q = largePrime()

    # Get product of primes
    n = p * q

    # Compute totient
    phi_n = (p - 1) * (q - 1)

    # Find appropriate e value
    e = find_e(phi_n)

    # Find the modular inverse
    d = modinv(e, phi_n)

    # Write piblic key
    fw = open(os.getcwd()+"/"+public, 'w')
    fw.write(str(n)+"\n")
    fw.write(str(e))
    fw.close()

    # Write private key
    fw = open(os.getcwd()+"/"+private, 'w')
    fw.write(str(n)+"\n")
    fw.write(str(d))
    fw.close

main()