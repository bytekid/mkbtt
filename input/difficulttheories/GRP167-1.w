    NAME        GRP167-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   positive_part: ANY -> ANY
                 least_upper_bound: ANY ANY -> ANY
                 identity: -> ANY
                 negative_part: ANY -> ANY
                 greatest_lower_bound: ANY ANY -> ANY
                 multiply: ANY ANY -> ANY
                 inverse: ANY -> ANY
    ORDERING    LPO
                positive_part > least_upper_bound > identity > negative_part > greatest_lower_bound > multiply > inverse
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   positive_part(X) = least_upper_bound(X,identity) 
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