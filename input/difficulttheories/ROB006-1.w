    NAME        ROB006-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   add: ANY ANY -> ANY
                 c: -> ANY
                 d: -> ANY
                 negate: ANY -> ANY
    ORDERING    LPO
                add > c > d > negate
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   add(c,d) = d 
                  add(X,Y) = add(Y,X) 
                  add(add(X,Y),Z) = add(X,add(Y,Z)) 
                  negate(add(negate(add(X,Y)),negate(add(X,negate(Y))))) = X 
    CONCLUSION