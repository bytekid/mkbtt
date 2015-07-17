    NAME        GRP430-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   multiply: ANY ANY -> ANY
                 inverse: ANY -> ANY
    ORDERING    LPO
                multiply > inverse
    VARIABLES  A,B,C,D: ANY
    EQUATIONS   multiply(A,inverse(multiply(B,multiply(multiply(multiply(C,inverse(C)),inverse(multiply(D,B))),A)))) = D 
    CONCLUSION