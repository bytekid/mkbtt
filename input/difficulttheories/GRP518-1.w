    NAME        GRP518-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   multiply: ANY ANY -> ANY
                 inverse: ANY -> ANY
    ORDERING    LPO
                multiply > inverse
    VARIABLES  A,B,C: ANY
    EQUATIONS   multiply(A,multiply(multiply(inverse(multiply(A,B)),C),B)) = C 
    CONCLUSION