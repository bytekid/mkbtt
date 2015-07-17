    NAME        GRP181-4
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   inverse: ANY -> ANY
                 identity: -> ANY
                 multiply: ANY ANY -> ANY
                 greatest_lower_bound: ANY ANY -> ANY
                 a: -> ANY
                 c: -> ANY
                 b: -> ANY
                 least_upper_bound: ANY ANY -> ANY
    ORDERING    LPO
                inverse > identity > multiply > greatest_lower_bound > a > c > b > least_upper_bound
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   inverse(identity) = identity 
                  inverse(inverse(X)) = X 
                  inverse(multiply(X,Y)) = multiply(inverse(Y),inverse(X)) 
                  greatest_lower_bound(a,c) = greatest_lower_bound(b,c) 
                  least_upper_bound(a,c) = least_upper_bound(b,c) 
                  inverse(greatest_lower_bound(X,Y)) = least_upper_bound(inverse(X),inverse(Y)) 
                  inverse(least_upper_bound(X,Y)) = greatest_lower_bound(inverse(X),inverse(Y)) 
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