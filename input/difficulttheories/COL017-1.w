    NAME        COL017-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 b: -> ANY
                 m: -> ANY
                 t: -> ANY
    ORDERING    LPO
                apply > b > m > t
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(apply(b,X),Y),Z) = apply(X,apply(Y,Z)) 
                  apply(m,X) = apply(X,X) 
                  apply(apply(t,X),Y) = apply(Y,X) 
    CONCLUSION