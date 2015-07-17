    NAME        GRP114-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   inverse: ANY -> ANY
                 identity: -> ANY
                 multiply: ANY ANY -> ANY
                 intersection: ANY ANY -> ANY
                 union: ANY ANY -> ANY
                 positive_part: ANY -> ANY
                 negative_part: ANY -> ANY
    ORDERING    LPO
                inverse > identity > multiply > intersection > union > positive_part > negative_part
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   inverse(identity) = identity 
                  inverse(inverse(X)) = X 
                  inverse(multiply(X,Y)) = multiply(inverse(Y),inverse(X)) 
                  intersection(X,X) = X 
                  union(X,X) = X 
                  intersection(X,Y) = intersection(Y,X) 
                  union(X,Y) = union(Y,X) 
                  intersection(X,intersection(Y,Z)) = intersection(intersection(X,Y),Z) 
                  union(X,union(Y,Z)) = union(union(X,Y),Z) 
                  union(intersection(X,Y),Y) = Y 
                  intersection(union(X,Y),Y) = Y 
                  multiply(X,union(Y,Z)) = union(multiply(X,Y),multiply(X,Z)) 
                  multiply(X,intersection(Y,Z)) = intersection(multiply(X,Y),multiply(X,Z)) 
                  multiply(union(Y,Z),X) = union(multiply(Y,X),multiply(Z,X)) 
                  multiply(intersection(Y,Z),X) = intersection(multiply(Y,X),multiply(Z,X)) 
                  positive_part(X) = union(X,identity) 
                  negative_part(X) = intersection(X,identity) 
                  multiply(identity,X) = X 
                  multiply(inverse(X),X) = identity 
                  multiply(multiply(X,Y),Z) = multiply(X,multiply(Y,Z)) 
    CONCLUSION