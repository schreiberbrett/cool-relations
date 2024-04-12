# A038754(n)
def h(n):
    if n % 2 == 0:
        return 3 ** (n // 2)
    else:
        return 2 * (3 ** (n // 2))


# A001045
def J(n):
    return ((2 ** n) - ((-1) ** n)) // 3
    
    

def limit(n):
    return h(n - 3) / J(n - 1)
