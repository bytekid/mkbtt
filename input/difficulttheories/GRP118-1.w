    NAME        GRP118-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   multiply: ANY ANY -> ANY
                 identity: -> ANY
    ORDERING    LPO
                multiply > identity
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   multiply(X,multiply(multiply(X,multiply(multiply(X,Y),Z)),multiply(identity,multiply(Z,Z)))) = Y 
    CONCLUSION