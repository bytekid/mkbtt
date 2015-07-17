    NAME        GRP167-5
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   inverse: ANY -> ANY
                 least_upper_bound: ANY ANY -> ANY
                 greatest_lower_bound: ANY ANY -> ANY
                 positive_part: ANY -> ANY
                 identity: -> ANY
                 negative_part: ANY -> ANY
                 multiply: ANY ANY -> ANY
    ORDERING    LPO
                inverse > least_upper_bound > greatest_lower_bound > positive_part > identity > negative_part > multiply
    VARIABLES  A,B,X,Y,Z: ANY
    EQUATIONS   inverse(least_upper_bound(A,B)) = greatest_lower_bound(inverse(A),inverse(B)) 
                  positive_part(X) = least_upper_bound(X,identity) 
                  negative_part(X) = greatest_lower_bound(X,identity) 
                  least_upper_bound(X,greatest_lower_bound(Y,Z)) = greatest_lower_bound(least_upper_bound(X,Y),least_upper_bound(X,Z)) 
                  greatest_lower_bound(X,least_upper_bound(Y,Z)) = least_upper_bound(greatest_lower_bound(X,Y),greatest_lower_bound(X,Z)) 
                  multiply(identity,X) = X 
                  multiply(inverse(X),X) = identity 
                  multiply(multiply(X,Y),Z) = multiply(X,multiply(Y,Z)) 
                  greatest_lower_bound(X,Y) = greatest_lower_bound(Y,X) 
                  least_upper_bound(X,Y) = least_upper_bound(Y,X) 
                  greatest_lower_bound(X,greatest_lower_bound(Y,Z)) = greatest_lower_bound(greatest_lower_bound(X,Y),Z) 
                  least_upper_bound(X,least_upper_bound(Y,Z)) = least_upper_bound(least_upper_bound(X,Y),Z) 
                  least_upper_bound(X,X) = X 
                  greatest_lower_bound(X,X) = X 
                  least_upper_bound(X,greatest_lower_bound(X,Y)) = X 
                  greatest_lower_bound(X,least_upper_bound(X,Y)) = X 
                  multiply(X,least_upper_bound(Y,Z)) = least_upper_bound(multiply(X,Y),multiply(X,Z)) 
                  multiply(X,greatest_lower_bound(Y,Z)) = greatest_lower_bound(multiply(X,Y),multiply(X,Z)) 
                  multiply(least_upper_bound(Y,Z),X) = least_upper_bound(multiply(Y,X),multiply(Z,X)) 
                  multiply(greatest_lower_bound(Y,Z),X) = greatest_lower_bound(multiply(Y,X),multiply(Z,X)) 
    CONCLUSION