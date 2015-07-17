    NAME        COL075-2
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 k: -> ANY
                 abstraction: -> ANY
                 projection1: -> ANY
                 pair: ANY ANY -> ANY
                 projection2: -> ANY
                 eq: -> ANY
    ORDERING    LPO
                apply > k > abstraction > projection1 > pair > projection2 > eq
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(k,X),Y) = X 
                  apply(apply(apply(abstraction,X),Y),Z) = apply(apply(X,apply(k,Z)),apply(Y,Z)) 
                  apply(k(X),Y) = X 
                  apply(projection1,pair(X,Y)) = X 
                  apply(projection2,pair(X,Y)) = Y 
                  pair(apply(projection1,X),apply(projection2,X)) = X 
                  apply(pair(X,Y),Z) = pair(apply(X,Z),apply(Y,Z)) 
                  apply(apply(apply(abstraction,X),Y),Z) = apply(apply(X,k(Z)),apply(Y,Z)) 
                  apply(eq,pair(X,X)) = projection1 
                   projection1 != projection2 
    CONCLUSION