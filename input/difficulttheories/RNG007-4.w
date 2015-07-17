    NAME        RNG007-4
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
                  add(additive_inverse(X),X) = additive_identity 
                  multiply(X,add(Y,Z)) = add(multiply(X,Y),multiply(X,Z)) 
                  multiply(add(X,Y),Z) = add(multiply(X,Z),multiply(Y,Z)) 
                  additive_inverse(additive_identity) = additive_identity 
                  additive_inverse(additive_inverse(X)) = X 
                  multiply(X,additive_identity) = additive_identity 
                  multiply(additive_identity,X) = additive_identity 
                  additive_inverse(add(X,Y)) = add(additive_inverse(X),additive_inverse(Y)) 
                  multiply(X,additive_inverse(Y)) = additive_inverse(multiply(X,Y)) 
                  multiply(additive_inverse(X),Y) = additive_inverse(multiply(X,Y)) 
                  add(add(X,Y),Z) = add(X,add(Y,Z)) 
                  add(X,Y) = add(Y,X) 
                  multiply(multiply(X,Y),Z) = multiply(X,multiply(Y,Z)) 
    CONCLUSION