    NAME        LAT018-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   join: ANY ANY -> ANY
                 complement: ANY -> ANY
                 n1: -> ANY
                 meet: ANY ANY -> ANY
                 n0: -> ANY
    ORDERING    LPO
                join > complement > n1 > meet > n0
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   join(complement(X),X) = n1 
                  meet(complement(X),X) = n0 
                  join(X,meet(X,Y)) = X 
                  meet(X,Y) = meet(Y,X) 
                  join(X,Y) = join(Y,X) 
                  meet(meet(X,Y),Z) = meet(X,meet(Y,Z)) 
                  join(join(X,Y),Z) = join(X,join(Y,Z)) 
                  complement(complement(X)) = X 
                  join(X,join(Y,complement(Y))) = join(Y,complement(Y)) 
                  meet(X,Y) = complement(join(complement(X),complement(Y))) 
    CONCLUSION