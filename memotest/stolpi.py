def podvoji(x):
    print("Računam " + str(x))
    return 2 * x

print(podvoji(10))
print(podvoji(10))
print(podvoji(10))

rezultati = {}
def mem_podvoji(x):
    if x in rezultati:
        return rezultati[x]
    else:
        rezultati[x] = (y := podvoji(x))
        return y

print(mem_podvoji(10))
print(mem_podvoji(10))
print(mem_podvoji(10))

def memoiziraj(f):
    rezultati = {}
    def mem_f(x):
        if x in rezultati:
            return rezultati[x]
        else:
            rezultati[x] = (y := f(x))
            return y
    return mem_f

@memoiziraj
def stevilo_stolpov(n):
    if n < 0:
        return 0
    elif n == 0:
        return 1
    else:
        return stevilo_stolpov(n - 1) + stevilo_stolpov(n - 2) + stevilo_stolpov(n - 3)