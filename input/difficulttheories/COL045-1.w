    NAME        COL045-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 s: -> ANY
                 b: -> ANY
                 m: -> ANY
    ORDERING    LPO
                apply > s > b > m
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(apply(s,X),Y),Z) = apply(apply(X,Z),apply(Y,Z)) 
                  apply(apply(apply(b,X),Y),Z) = apply(X,apply(Y,Z)) 
                  apply(m,X) = apply(X,X) 
    CONCLUSION