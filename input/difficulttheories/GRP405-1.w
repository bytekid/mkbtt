    NAME        GRP405-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   multiply: ANY ANY -> ANY
                 inverse: ANY -> ANY
    ORDERING    LPO
                multiply > inverse
    VARIABLES  A,B,C: ANY
    EQUATIONS   multiply(A,inverse(multiply(inverse(multiply(inverse(multiply(A,B)),C)),inverse(multiply(B,multiply(inverse(B),B)))))) = C 
    CONCLUSION