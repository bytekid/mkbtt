    NAME        LCL165-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   implies: ANY ANY -> ANY
                 truth: -> ANY
                 not: ANY -> ANY
                 or: ANY ANY -> ANY
                 and: ANY ANY -> ANY
    ORDERING    LPO
                implies > truth > not > or > and
    VARIABLES  X,Y,Z: ANY
    EQUATIONS  implies(truth,X) = X
                 implies(implies(X,Y),implies(implies(Y,Z),implies(X,Z))) = truth
                 implies(implies(not(X),not(Y)),implies(Y,X)) = truth
                 implies(implies(X,Y),Y) = implies(implies(Y,X),X)
                  or(X,Y) = implies(not(X),Y) 
                  or(or(X,Y),Z) = or(X,or(Y,Z)) 
                  or(X,Y) = or(Y,X) 
                  and(X,Y) = not(or(not(X),not(Y))) 
                  and(and(X,Y),Z) = and(X,and(Y,Z)) 
                  and(X,Y) = and(Y,X) 
    CONCLUSION