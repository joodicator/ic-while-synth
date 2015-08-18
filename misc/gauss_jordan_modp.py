def main():
    A = [[1,2,3, 1,0,0],
         [3,2,1, 0,1,0],
         [1,2,1, 0,0,1]]
    
    m, n = 3, 6
    p = 7
    
    def gauss_jordan_modp():
        i, j = 0, 0
        while i < m:
            x = A[i][j]
            if x == 0:
                k = i
                i, j = find_nonzero(i, j)
                swap_rows(k, i)
                i = k
                x = A[i][j]
                if x == 0:
                    i = m
            if x > 0:
                k = 0
                while k < m:
                    x = A[k][j]
                    if x > 0:
                        normalise_row(i, j)
                        y = p - x
                        scale_row(i, y)
                        add_row(i, k)
                    k = k + 1
                normalise_row(i, j)
            i, j = i+1, j+1
            if j >= n:
                i = m
    
    # Pre:  i = i0, j = j0
    # Post: j = first column >= j0 containing a nonzero element in row >= i0, else j0
    #       i = first row >= i0 for which A[i,j] nonzero, else i0
    def find_nonzero(i, j):
        v = j
        while v < n:
            u = i
            while u < m:
                if A[u][v] != 0:
                    i, j = u, v
                    u, v = m, n
                u = u + 1
            v = v + 1
        return i, j
    
    def swap_rows(k, i):
        v = 0
        while v < n:
            s = A[k][v]
            t = A[i][v]
            A[i][v] = t
            A[k][v] = s
            v = v + 1
    
    def scale_row(i, y):
        v = 0
        while v < n:
            z = A[i][v]
            z = z * y
            z = z % p
            A[i][v] = z
            v = v + 1
    
    def add_row(i, k):
        v = 0
        while v < n:
            x = A[i][v]
            y = A[k][v]
            y = y + x
            y = y % p
            A[k][v] = y
            v = v + 1
    
    def normalise_row(i, j):
        y = A[i][j]
        y = invert(y)
        scale_row(i, y)
    
    def invert(y):
        z = 1
        while z < p:
            w = z * y
            w = w % p
            if w == 1:
                y = z
                z = p
            z = z + 1
        return y
    
    gauss_jordan_modp()
    return A
    
def mmul(A, B, p):
    wA, hA = len(A[0]), len(A)
    wB, hB = len(B[0]), len(B)
    if wA != hB: raise Exception(
        "mmul: incompatible dimensions %d*%d and %d*%d." % (hA,wA,hB,wB))
    return [[sum(A[i][k] * B[k][j]
        for k in range(wA)) % p for i in range(hA)] for j in range(wB)]
