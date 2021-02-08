from functools import lru_cache

test = [2, 4, 1, 2, 1, 3, 1, 1, 5]
test2 = [4, 1, 8, 2, 11, 1, 1, 1, 1, 1]

def froggie(lst):
    def jumper(lst, energy):
        return min([1 if i >= len(lst) else 1 + jumper (lst[i:], energy - i) for i in range(1, lst[0])])
    return jumper(lst, 0)