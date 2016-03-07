import random
import os.path
import sys


# Generate two large primes
def largePrime():
    while True:
        p = random.randrange(2**31+1,  2**32, 2)
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


# Find e value which satisfies 1 < e < phi(n) && gcd(e, phi(n)) == 1
def find_e(n):
    while True:
        e = random.randrange(1, n)
        if (gcd(e, n) == 1):
            return e


# Apply Euler's extended GCD alg
# Taken from https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm
def egcd(a, b):
    if a == 0:
        return (b, 0, 1)
    else:
        g, y, x = egcd(b % a, a)
        return g, x - (b // a) * y, y


# Find the modular inverse
# Referenced from https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm
def modinv(a, m):
    gcd, x, y = egcd(a, m)
    if gcd != 1:
        return None
    else:
        return x % m

def usage():
    print("python keygen.py privName pubName")

def main():

    # Take in command line arguments
    if (len(sys.argv) != 3):
        usage()
        sys.exit(0)
    else:
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