    NAME        GRP002-4
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   multiply: ANY ANY -> ANY
                 identity: -> ANY
                 inverse: ANY -> ANY
                 commutator: ANY ANY -> ANY
    ORDERING    LPO
                multiply > identity > inverse > commutator
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   multiply(X,identity) = X 
                  multiply(X,inverse(X)) = identity 
                  commutator(X,Y) = multiply(X,multiply(Y,multiply(inverse(X),inverse(Y)))) 
                  multiply(X,multiply(X,X)) = identity 
                  multiply(identity,X) = X 
                  multiply(inverse(X),X) = identity 
                  multiply(multiply(X,Y),Z) = multiply(X,multiply(Y,Z)) 
    CONCLUSION