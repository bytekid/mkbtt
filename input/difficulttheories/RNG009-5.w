    NAME        RNG009-5
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   add: ANY ANY -> ANY
                 additive_identity: -> ANY
                 additive_inverse: ANY -> ANY
                 multiply: ANY ANY -> ANY
    ORDERING    LPO
                add > additive_identity > additive_inverse > multiply
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   add(X,additive_identity) = X 
                  add(X,additive_inverse(X)) = additive_identity 
                  multiply(X,add(Y,Z)) = add(multiply(X,Y),multiply(X,Z)) 
                  multiply(add(X,Y),Z) = add(multiply(X,Z),multiply(Y,Z)) 
                  add(add(X,Y),Z) = add(X,add(Y,Z)) 
                  add(X,Y) = add(Y,X) 
                  multiply(multiply(X,Y),Z) = multiply(X,multiply(Y,Z)) 
                  multiply(X,multiply(X,X)) = X 
    CONCLUSION