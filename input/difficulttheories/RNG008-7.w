    NAME        RNG008-7
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   multiply: ANY ANY -> ANY
                 add: ANY ANY -> ANY
                 additive_identity: -> ANY
                 additive_inverse: ANY -> ANY
    ORDERING    LPO
                multiply > add > additive_identity > additive_inverse
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   multiply(X,X) = X 
                  add(additive_identity,X) = X 
                  add(X,additive_identity) = X 
                  add(additive_inverse(X),X) = additive_identity 
                  add(X,additive_inverse(X)) = additive_identity 
                  add(X,add(Y,Z)) = add(add(X,Y),Z) 
                  add(X,Y) = add(Y,X) 
                  multiply(X,multiply(Y,Z)) = multiply(multiply(X,Y),Z) 
                  multiply(X,add(Y,Z)) = add(multiply(X,Y),multiply(X,Z)) 
                  multiply(add(X,Y),Z) = add(multiply(X,Z),multiply(Y,Z)) 
    CONCLUSION