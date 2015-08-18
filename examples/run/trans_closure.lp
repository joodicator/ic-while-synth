%*
    any = 1
    while any > 1:
        any = 0
        a = 0
        while a < n:
            b = 0
            while b < n:
                k = n * a
                k = k + b
                r = m[k]
                if r > 0:
                    c = 0
                    while c < n:
                        k = n * b
                        k = k + c
                        r = m[k]
                        if r > 0:
                            k = n * a
                            k = k + c
                            m[k] = 1
                            any = 1
                        c = c + 1
                    end_wile
                b = b + 1
            end_while
            a = a + 1
        end_while
    end_while
*%
