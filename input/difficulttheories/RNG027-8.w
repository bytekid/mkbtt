    NAME        RNG027-8
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   add: ANY ANY -> ANY
                 additive_identity: -> ANY
                 multiply: ANY ANY -> ANY
                 additive_inverse: ANY -> ANY
                 associator: ANY ANY ANY -> ANY
                 commutator: ANY ANY -> ANY
    ORDERING    LPO
                add > additive_identity > multiply > additive_inverse > associator > commutator
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   add(additive_identity,X) = X 
                  add(X,additive_identity) = X 
                  multiply(additive_identity,X) = additive_identity 
                  multiply(X,additive_identity) = additive_identity 
                  add(additive_inverse(X),X) = additive_identity 
                  add(X,additive_inverse(X)) = additive_identity 
                  additive_inverse(additive_inverse(X)) = X 
                  multiply(X,add(Y,Z)) = add(multiply(X,Y),multiply(X,Z)) 
                  multiply(add(X,Y),Z) = add(multiply(X,Z),multiply(Y,Z)) 
                  add(X,Y) = add(Y,X) 
                  add(X,add(Y,Z)) = add(add(X,Y),Z) 
                  multiply(multiply(X,Y),Y) = multiply(X,multiply(Y,Y)) 
                  multiply(multiply(X,X),Y) = multiply(X,multiply(X,Y)) 
                  associator(X,Y,Z) = add(multiply(multiply(X,Y),Z),additive_inverse(multiply(X,multiply(Y,Z)))) 
                  commutator(X,Y) = add(multiply(Y,X),additive_inverse(multiply(X,Y))) 
    CONCLUSION