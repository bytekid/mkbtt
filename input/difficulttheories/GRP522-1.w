    NAME        GRP522-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   divide: ANY ANY -> ANY
                 multiply: ANY ANY -> ANY
                 inverse: ANY -> ANY
    ORDERING    LPO
                divide > multiply > inverse
    VARIABLES  A,B,C: ANY
    EQUATIONS   divide(A,divide(B,divide(C,divide(A,B)))) = C 
                  multiply(A,B) = divide(A,divide(divide(C,C),B)) 
                  inverse(A) = divide(divide(B,B),A) 
    CONCLUSION