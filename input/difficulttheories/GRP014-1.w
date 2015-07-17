    NAME        GRP014-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   multiply: ANY ANY -> ANY
                 inverse: ANY -> ANY
    ORDERING    LPO
                multiply > inverse
    VARIABLES  X,Y,W,Z: ANY
    EQUATIONS   multiply(X,inverse(multiply(multiply(inverse(multiply(inverse(Y),multiply(inverse(X),W))),Z),inverse(multiply(Y,Z))))) = W 
    CONCLUSION