    NAME        GRP177-2
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   greatest_lower_bound: ANY ANY -> ANY
                 identity: -> ANY
                 a: -> ANY
                 b: -> ANY
                 c: -> ANY
                 multiply: ANY ANY -> ANY
                 inverse: ANY -> ANY
                 least_upper_bound: ANY ANY -> ANY
    ORDERING    LPO
                greatest_lower_bound > identity > a > b > c > multiply > inverse > least_upper_bound
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   greatest_lower_bound(identity,a) = identity 
                  greatest_lower_bound(identity,b) = identity 
                  greatest_lower_bound(identity,c) = identity 
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