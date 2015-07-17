    NAME        GRP196-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   multiply: ANY ANY -> ANY
    ORDERING    LPO
                multiply
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   multiply(X,multiply(Y,multiply(Y,Y))) = multiply(Y,multiply(Y,multiply(Y,X))) 
                  multiply(multiply(X,Y),Z) = multiply(X,multiply(Y,Z)) 
    CONCLUSION