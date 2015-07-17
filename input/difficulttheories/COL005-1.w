    NAME        COL005-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 s: -> ANY
                 w: -> ANY
    ORDERING    LPO
                apply > s > w
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(apply(s,X),Y),Z) = apply(apply(X,Z),apply(Y,Z)) 
                  apply(apply(w,X),Y) = apply(apply(X,Y),Y) 
    CONCLUSION