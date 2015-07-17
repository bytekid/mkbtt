    NAME        GRP500-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   double_divide: ANY ANY -> ANY
                 inverse: ANY -> ANY
                 multiply: ANY ANY -> ANY
    ORDERING    LPO
                double_divide > inverse > multiply
    VARIABLES  A,B,C,D: ANY
    EQUATIONS   double_divide(inverse(A),inverse(double_divide(inverse(double_divide(A,double_divide(B,C))),double_divide(D,double_divide(B,D))))) = C 
                  multiply(A,B) = inverse(double_divide(B,A)) 
    CONCLUSION