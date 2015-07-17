    NAME        COL039-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 b: -> ANY
                 m: -> ANY
                 o: -> ANY
    ORDERING    LPO
                apply > b > m > o
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(apply(b,X),Y),Z) = apply(X,apply(Y,Z)) 
                  apply(m,X) = apply(X,X) 
                  apply(apply(o,X),Y) = apply(Y,apply(X,Y)) 
    CONCLUSION