    NAME        COL023-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 b: -> ANY
                 n: -> ANY
    ORDERING    LPO
                apply > b > n
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(apply(b,X),Y),Z) = apply(X,apply(Y,Z)) 
                  apply(apply(apply(n,X),Y),Z) = apply(apply(apply(X,Z),Y),Z) 
    CONCLUSION