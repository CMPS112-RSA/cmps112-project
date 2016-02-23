import random
import os.path
import sys


def largePrime():
    while True:
        p = random.randrange(101, 1000, 2)
        if all(p % n != 0 for n in range(3, int((p ** 0.5) + 1), 2)):
            return p


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


def find_e(n):
    e = 0
    for i in range(2, n):
        if gcd(n, i) == 1:
            e = i
            break
    return e


def egcd(a, b):
    if a == 0:
        return (b, 0, 1)
    else:
        g, y, x = egcd(b % a, a)
        return (g, x - (b // a) * y, y)


def modinv(a, m):
    gcd, x, y = egcd(a, m)
    if gcd != 1:
        return None
    else:
        return x % m


def main():
    private = sys.argv[1]
    public = sys.argv[2]

    p = largePrime()
    q = largePrime()
    n = p * q
    phi_n = (p - 1) * (q - 1)
    e = find_e(phi_n)
    d = modinv(e, phi_n)

    print("P: "+str(p))
    print("Q: "+str(q))
    print("N: "+str(n))
    print("phi(N): "+str(phi_n))
    print("E: "+str(e))
    print("D: "+str(d))

    fw = open(os.getcwd()+"/"+public, 'w')
    fw.write(str(n)+"\n")
    fw.write(str(e))
    fw.close()
    fw = open(os.getcwd()+"/"+private, 'w')
    fw.write(str(n)+"\n")
    fw.write(str(d))
    fw.close

main()