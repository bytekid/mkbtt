    NAME        COL071-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 n: -> ANY
                 q: -> ANY
    ORDERING    LPO
                apply > n > q
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(apply(n,X),Y),Z) = apply(apply(apply(X,Z),Y),Z) 
                  apply(apply(apply(q,X),Y),Z) = apply(Y,apply(X,Z)) 
    CONCLUSION