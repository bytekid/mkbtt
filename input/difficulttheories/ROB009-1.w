    NAME        ROB009-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   negate: ANY -> ANY
                 add: ANY ANY -> ANY
                 a: -> ANY
                 b: -> ANY
                 c: -> ANY
    ORDERING    LPO
                negate > add > a > b > c
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   negate(add(a,negate(add(b,c)))) = negate(add(b,negate(add(a,c)))) 
                  add(X,Y) = add(Y,X) 
                  add(add(X,Y),Z) = add(X,add(Y,Z)) 
                  negate(add(negate(add(X,Y)),negate(add(X,negate(Y))))) = X 
    CONCLUSION