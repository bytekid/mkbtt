    NAME        GRP443-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   inverse: ANY -> ANY
                 multiply: ANY ANY -> ANY
    ORDERING    LPO
                inverse > multiply
    VARIABLES  A,B,C,D: ANY
    EQUATIONS   inverse(multiply(A,multiply(B,multiply(multiply(C,inverse(C)),inverse(multiply(D,multiply(A,B))))))) = D 
    CONCLUSION