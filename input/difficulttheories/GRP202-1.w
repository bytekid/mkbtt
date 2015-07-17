    NAME        GRP202-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   multiply: ANY ANY -> ANY
                 identity: -> ANY
                 left_division: ANY ANY -> ANY
                 right_division: ANY ANY -> ANY
                 right_inverse: ANY -> ANY
                 left_inverse: ANY -> ANY
    ORDERING    LPO
                multiply > identity > left_division > right_division > right_inverse > left_inverse
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   multiply(identity,X) = X 
                  multiply(X,identity) = X 
                  multiply(X,left_division(X,Y)) = Y 
                  left_division(X,multiply(X,Y)) = Y 
                  multiply(right_division(X,Y),Y) = X 
                  right_division(multiply(X,Y),Y) = X 
                  multiply(X,right_inverse(X)) = identity 
                  multiply(left_inverse(X),X) = identity 
                  multiply(multiply(multiply(X,Y),X),Z) = multiply(X,multiply(Y,multiply(X,Z))) 
    CONCLUSION