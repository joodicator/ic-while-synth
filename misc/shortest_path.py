m = 4
j = [[0,0,1,0],
     [0,0,0,0],
     [0,0,0,0],
     [0,1,0,0]]

start = 0
goal  = 1

path  = [4,4,4,4]
found = 0

def main():
    global dmax, d, n
    dmax = 0
    while dmax < m:
        d = 0
        n = start
        search()
        if found > 0:
            dmax = m - 1
        dmax = dmax + 1

def search():
    global found, d, n, o, k, c
    path[d] = n
    if n == goal:
        found = 1
    if found == 0 and d < dmax:
        n = 0
        while n < m:
            o = path[d]
            c = j[o][n]
            if c > 0:
                d = d + 1
                search()
                d = d - 1
                if found != 0:
                    n = m - 1
            n = n + 1
        n = path[d]
