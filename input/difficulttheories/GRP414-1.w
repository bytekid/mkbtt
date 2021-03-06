    NAME        GRP414-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   multiply: ANY ANY -> ANY
                 inverse: ANY -> ANY
    ORDERING    LPO
                multiply > inverse
    VARIABLES  A,B,C: ANY
    EQUATIONS   multiply(A,inverse(multiply(multiply(multiply(inverse(B),B),inverse(multiply(inverse(multiply(A,inverse(B))),C))),B))) = C 
    CONCLUSION