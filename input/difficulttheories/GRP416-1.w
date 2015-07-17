    NAME        GRP416-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   inverse: ANY -> ANY
                 multiply: ANY ANY -> ANY
    ORDERING    LPO
                inverse > multiply
    VARIABLES  A,B,C: ANY
    EQUATIONS   inverse(multiply(A,inverse(multiply(inverse(multiply(inverse(multiply(B,A)),multiply(B,inverse(C)))),inverse(multiply(inverse(A),A)))))) = C 
    CONCLUSION