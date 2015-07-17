    NAME        GRP204-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   multiply: ANY ANY -> ANY
                 identity: -> ANY
                 left_inverse: ANY -> ANY
    ORDERING    LPO
                multiply > identity > left_inverse
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   multiply(identity,X) = X 
                  multiply(left_inverse(X),X) = identity 
                  multiply(multiply(X,multiply(Y,Z)),X) = multiply(multiply(X,Y),multiply(Z,X)) 
    CONCLUSION