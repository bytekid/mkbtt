    NAME        COL002-4
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 s: -> ANY
                 b: -> ANY
                 c: -> ANY
                 i: -> ANY
                 weak_fixed_point: ANY -> ANY
    ORDERING    LPO
                apply > s > b > c > i > weak_fixed_point
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(apply(s,X),Y),Z) = apply(apply(X,Z),apply(Y,Z)) 
                  apply(apply(apply(b,X),Y),Z) = apply(X,apply(Y,Z)) 
                  apply(apply(apply(c,X),Y),Z) = apply(apply(X,Z),Y) 
                  apply(i,X) = X 
                  weak_fixed_point(X) = apply(apply(apply(s,apply(b,X)),i),apply(apply(s,apply(b,X)),i)) 
    CONCLUSION