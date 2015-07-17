    NAME        GRP024-5
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   commutator: ANY ANY -> ANY
                 multiply: ANY ANY -> ANY
                 inverse: ANY -> ANY
                 identity: -> ANY
    ORDERING    LPO
                commutator > multiply > inverse > identity
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   commutator(X,Y) = multiply(inverse(X),multiply(inverse(Y),multiply(X,Y))) 
                  commutator(commutator(X,Y),Z) = commutator(X,commutator(Y,Z)) 
                  multiply(identity,X) = X 
                  multiply(inverse(X),X) = identity 
                  multiply(multiply(X,Y),Z) = multiply(X,multiply(Y,Z)) 
    CONCLUSION