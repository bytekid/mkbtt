    NAME        LCL137-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   implies: ANY ANY -> ANY
                 truth: -> ANY
                 not: ANY -> ANY
    ORDERING    LPO
                implies > truth > not
    VARIABLES  X,Y,Z: ANY
    EQUATIONS  implies(truth,X) = X
                 implies(implies(X,Y),implies(implies(Y,Z),implies(X,Z))) = truth
                 implies(implies(not(X),not(Y)),implies(Y,X)) = truth
                 implies(implies(X,Y),Y) = implies(implies(Y,X),X)
    CONCLUSION