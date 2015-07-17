    NAME        BOO034-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   multiply: ANY ANY ANY -> ANY
                 inverse: ANY -> ANY
    ORDERING    LPO
                multiply > inverse
    VARIABLES  V,W,X,Y,Z: ANY
    EQUATIONS   multiply(multiply(V,W,X),Y,multiply(V,W,Z)) = multiply(V,W,multiply(X,Y,Z)) 
                  multiply(Y,X,X) = X 
                  multiply(X,X,Y) = X 
                  multiply(inverse(Y),Y,X) = X 
                  multiply(X,Y,inverse(Y)) = X 
    CONCLUSION