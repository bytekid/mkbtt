    NAME        LAT062-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   join: ANY ANY -> ANY
                 complement: ANY -> ANY
                 n1: -> ANY
                 meet: ANY ANY -> ANY
                 n0: -> ANY
    ORDERING    LPO
                join > complement > n1 > meet > n0
    VARIABLES  A,B,X,Y,Z: ANY
    EQUATIONS   join(complement(A),A) = n1 
                  meet(complement(A),A) = n0 
                  meet(A,B) = complement(join(complement(A),complement(B))) 
                  meet(X,X) = X 
                  join(X,X) = X 
                  meet(X,join(X,Y)) = X 
                  join(X,meet(X,Y)) = X 
                  meet(X,Y) = meet(Y,X) 
                  join(X,Y) = join(Y,X) 
                  meet(meet(X,Y),Z) = meet(X,meet(Y,Z)) 
                  join(join(X,Y),Z) = join(X,join(Y,Z)) 
    CONCLUSION