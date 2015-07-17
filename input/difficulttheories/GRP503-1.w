    NAME        GRP503-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   double_divide: ANY ANY -> ANY
                 inverse: ANY -> ANY
                 multiply: ANY ANY -> ANY
    ORDERING    LPO
                double_divide > inverse > multiply
    VARIABLES  A,B,C,D: ANY
    EQUATIONS   double_divide(double_divide(A,inverse(double_divide(B,C))),double_divide(inverse(B),inverse(double_divide(D,double_divide(A,D))))) = C 
                  multiply(A,B) = inverse(double_divide(B,A)) 
    CONCLUSION