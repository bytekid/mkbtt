    NAME        ALG005-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   difference: ANY ANY -> ANY
                 multiply: ANY ANY -> ANY
    ORDERING    LPO
                difference > multiply
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   difference(X,difference(Y,X)) = X 
                  difference(X,difference(X,Y)) = difference(Y,difference(Y,X)) 
                  difference(difference(X,Y),Z) = difference(difference(X,Z),difference(Y,Z)) 
                  multiply(X,Y) = difference(X,difference(X,Y)) 
    CONCLUSION