    NAME        GRP437-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   multiply: ANY ANY -> ANY
                 inverse: ANY -> ANY
    ORDERING    LPO
                multiply > inverse
    VARIABLES  A,B,C,D: ANY
    EQUATIONS   multiply(A,inverse(multiply(B,multiply(C,multiply(multiply(inverse(C),inverse(multiply(D,B))),A))))) = D 
    CONCLUSION