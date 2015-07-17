    NAME        COL044-9
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 b: -> ANY
                 n: -> ANY
                 strong_fixed_point: -> ANY
    ORDERING    LPO
                apply > b > n > strong_fixed_point
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(apply(b,X),Y),Z) = apply(X,apply(Y,Z)) 
                  apply(apply(apply(n,X),Y),Z) = apply(apply(apply(X,Z),Y),Z) 
                  strong_fixed_point = apply(apply(b,apply(apply(b,apply(apply(n,apply(n,apply(apply(b,apply(b,b)),apply(n,apply(n,apply(b,b)))))),n)),b)),b) 
    CONCLUSION