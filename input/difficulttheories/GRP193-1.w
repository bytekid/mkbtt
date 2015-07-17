    NAME        GRP193-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   least_upper_bound: ANY ANY -> ANY
                 identity: -> ANY
                 a: -> ANY
                 b: -> ANY
                 c: -> ANY
                 greatest_lower_bound: ANY ANY -> ANY
                 multiply: ANY ANY -> ANY
                 inverse: ANY -> ANY
    ORDERING    LPO
                least_upper_bound > identity > a > b > c > greatest_lower_bound > multiply > inverse
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   least_upper_bound(identity,a) = a 
                  least_upper_bound(identity,b) = b 
                  least_upper_bound(identity,c) = c 
                  greatest_lower_bound(a,b) = identity 
                  least_upper_bound(greatest_lower_bound(a,multiply(b,c)),multiply(greatest_lower_bound(a,b),greatest_lower_bound(a,c))) = multiply(greatest_lower_bound(a,b),greatest_lower_bound(a,c)) 
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