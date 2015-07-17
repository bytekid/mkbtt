    NAME        COL024-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 b: -> ANY
                 m: -> ANY
                 c: -> ANY
    ORDERING    LPO
                apply > b > m > c
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(apply(b,X),Y),Z) = apply(X,apply(Y,Z)) 
                  apply(m,X) = apply(X,X) 
                  apply(apply(apply(c,X),Y),Z) = apply(apply(X,Z),Y) 
    CONCLUSION