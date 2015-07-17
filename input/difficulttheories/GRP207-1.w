    NAME        GRP207-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   multiply: ANY ANY -> ANY
                 inverse: ANY -> ANY
    ORDERING    LPO
                multiply > inverse
    VARIABLES  U,Y,Z: ANY
    EQUATIONS   multiply(U,inverse(multiply(Y,multiply(multiply(multiply(Z,inverse(Z)),inverse(multiply(U,Y))),U)))) = U 
    CONCLUSION