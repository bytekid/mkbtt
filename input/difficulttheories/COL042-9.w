    NAME        COL042-9
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 b: -> ANY
                 w1: -> ANY
                 strong_fixed_point: -> ANY
    ORDERING    LPO
                apply > b > w1 > strong_fixed_point
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(apply(b,X),Y),Z) = apply(X,apply(Y,Z)) 
                  apply(apply(w1,X),Y) = apply(apply(Y,X),X) 
                  strong_fixed_point = apply(apply(b,apply(w1,w1)),apply(apply(b,apply(b,w1)),apply(apply(b,b),b))) 
    CONCLUSION