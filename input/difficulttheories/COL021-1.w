    NAME        COL021-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 b: -> ANY
                 m: -> ANY
                 v: -> ANY
    ORDERING    LPO
                apply > b > m > v
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(apply(b,X),Y),Z) = apply(X,apply(Y,Z)) 
                  apply(m,X) = apply(X,X) 
                  apply(apply(apply(v,X),Y),Z) = apply(apply(Z,X),Y) 
    CONCLUSION