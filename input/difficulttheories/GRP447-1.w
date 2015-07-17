    NAME        GRP447-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   divide: ANY ANY -> ANY
                 multiply: ANY ANY -> ANY
                 inverse: ANY -> ANY
    ORDERING    LPO
                divide > multiply > inverse
    VARIABLES  A,B,C: ANY
    EQUATIONS   divide(A,divide(divide(divide(divide(A,A),B),C),divide(divide(divide(A,A),A),C))) = B 
                  multiply(A,B) = divide(A,divide(divide(C,C),B)) 
                  inverse(A) = divide(divide(B,B),A) 
    CONCLUSION