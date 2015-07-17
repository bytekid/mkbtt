    NAME        LDA002-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   f: ANY ANY -> ANY
                 n2: -> ANY
                 n1: -> ANY
                 n3: -> ANY
                 u: -> ANY
                 u1: -> ANY
                 u2: -> ANY
                 u3: -> ANY
                 uu: -> ANY
                 a: -> ANY
                 b: -> ANY
                 v: -> ANY
    ORDERING    LPO
                f > n2 > n1 > n3 > u > u1 > u2 > u3 > uu > a > b > v
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   f(X,f(Y,Z)) = f(f(X,Y),f(X,Z)) 
                  n2 = f(n1,n1) 
                  n3 = f(n2,n1) 
                  u = f(n2,n2) 
                  u1 = f(u,n1) 
                  u2 = f(u,n2) 
                  u3 = f(u,n3) 
                  uu = f(u,u) 
                  a = f(f(n3,n2),u2) 
                  b = f(u1,u3) 
                  v = f(uu,uu) 
    CONCLUSION