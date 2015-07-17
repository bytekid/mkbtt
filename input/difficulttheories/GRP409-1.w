    NAME        GRP409-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   multiply: ANY ANY -> ANY
                 inverse: ANY -> ANY
    ORDERING    LPO
                multiply > inverse
    VARIABLES  A,B,C: ANY
    EQUATIONS   multiply(multiply(inverse(multiply(A,inverse(multiply(B,C)))),multiply(A,inverse(C))),inverse(multiply(inverse(C),C))) = B 
    CONCLUSION