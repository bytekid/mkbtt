    NAME        GRP122-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   multiply: ANY ANY -> ANY
                 identity: -> ANY
    ORDERING    LPO
                multiply > identity
    VARIABLES  Y,X,Z: ANY
    EQUATIONS   multiply(Y,multiply(multiply(Y,multiply(multiply(Y,Y),multiply(X,Z))),multiply(Z,multiply(Z,Z)))) = X 
                  multiply(identity,identity) = identity 
    CONCLUSION