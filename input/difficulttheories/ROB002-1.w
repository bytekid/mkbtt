    NAME        ROB002-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   negate: ANY -> ANY
                 add: ANY ANY -> ANY
    ORDERING    LPO
                negate > add
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   negate(negate(X)) = X 
                  add(X,Y) = add(Y,X) 
                  add(add(X,Y),Z) = add(X,add(Y,Z)) 
                  negate(add(negate(add(X,Y)),negate(add(X,negate(Y))))) = X 
    CONCLUSION