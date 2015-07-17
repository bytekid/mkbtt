    NAME        COL001-2
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 s: -> ANY
                 k: -> ANY
                 b: -> ANY
                 i: -> ANY
                 x: -> ANY
    ORDERING    LPO
                apply > s > k > b > i > x
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(apply(s,X),Y),Z) = apply(apply(X,Z),apply(Y,Z)) 
                  apply(apply(k,X),Y) = X 
                  apply(apply(apply(b,X),Y),Z) = apply(X,apply(Y,Z)) 
                  apply(i,X) = X 
                  apply(apply(apply(s,apply(b,X)),i),apply(apply(s,apply(b,X)),i)) = apply(x,apply(apply(apply(s,apply(b,X)),i),apply(apply(s,apply(b,X)),i))) 
    CONCLUSION