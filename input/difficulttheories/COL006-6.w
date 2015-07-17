    NAME        COL006-6
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 s: -> ANY
                 k: -> ANY
                 strong_fixed_point: -> ANY
    ORDERING    LPO
                apply > s > k > strong_fixed_point
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(apply(s,X),Y),Z) = apply(apply(X,Z),apply(Y,Z)) 
                  apply(apply(k,X),Y) = X 
                  strong_fixed_point = apply(apply(s,apply(k,apply(apply(s,apply(apply(s,k),k)),apply(apply(s,k),k)))),apply(apply(s,apply(apply(s,apply(k,s)),k)),apply(k,apply(apply(s,apply(apply(s,k),k)),apply(apply(s,k),k))))) 
    CONCLUSION