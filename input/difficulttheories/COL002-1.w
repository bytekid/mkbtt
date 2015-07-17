    NAME        COL002-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 s: -> ANY
                 b: -> ANY
                 c: -> ANY
                 i: -> ANY
    ORDERING    LPO
                apply > s > b > c > i
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(apply(s,X),Y),Z) = apply(apply(X,Z),apply(Y,Z)) 
                  apply(apply(apply(b,X),Y),Z) = apply(X,apply(Y,Z)) 
                  apply(apply(apply(c,X),Y),Z) = apply(apply(X,Z),Y) 
                  apply(i,X) = X 
    CONCLUSION