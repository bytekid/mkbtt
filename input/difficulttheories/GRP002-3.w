    NAME        GRP002-3
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   commutator: ANY ANY -> ANY
                 multiply: ANY ANY -> ANY
                 inverse: ANY -> ANY
                 identity: -> ANY
    ORDERING    LPO
                commutator > multiply > inverse > identity
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   commutator(X,Y) = multiply(X,multiply(Y,multiply(inverse(X),inverse(Y)))) 
                  multiply(X,multiply(X,X)) = identity 
                  multiply(identity,X) = X 
                  multiply(inverse(X),X) = identity 
                  multiply(multiply(X,Y),Z) = multiply(X,multiply(Y,Z)) 
    CONCLUSION