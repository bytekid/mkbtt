    NAME        ROB023-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   add: ANY ANY -> ANY
                 negate: ANY -> ANY
    ORDERING    LPO
                add > negate
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   add(X,X) = X 
                  add(X,Y) = add(Y,X) 
                  add(add(X,Y),Z) = add(X,add(Y,Z)) 
                  negate(add(negate(add(X,Y)),negate(add(X,negate(Y))))) = X 
    CONCLUSION