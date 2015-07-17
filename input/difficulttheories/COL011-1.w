    NAME        COL011-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 o: -> ANY
                 q1: -> ANY
    ORDERING    LPO
                apply > o > q1
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(o,X),Y) = apply(Y,apply(X,Y)) 
                  apply(apply(apply(q1,X),Y),Z) = apply(X,apply(Z,Y)) 
    CONCLUSION