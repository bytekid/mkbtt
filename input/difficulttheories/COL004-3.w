    NAME        COL004-3
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 s: -> ANY
                 k: -> ANY
    ORDERING    LPO
                apply > s > k
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(apply(s,X),Y),Z) = apply(apply(X,Z),apply(Y,Z)) 
                  apply(apply(k,X),Y) = X 
    CONCLUSION