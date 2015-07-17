    NAME        COL027-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 b: -> ANY
                 h: -> ANY
    ORDERING    LPO
                apply > b > h
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(apply(b,X),Y),Z) = apply(X,apply(Y,Z)) 
                  apply(apply(apply(h,X),Y),Z) = apply(apply(apply(X,Y),Z),Y) 
    CONCLUSION