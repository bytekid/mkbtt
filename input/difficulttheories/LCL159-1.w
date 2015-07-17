    NAME        LCL159-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   implies: ANY ANY -> ANY
                 truth: -> ANY
                 not: ANY -> ANY
                 or: ANY ANY -> ANY
                 and: ANY ANY -> ANY
                 xor: ANY ANY -> ANY
                 and_star: ANY ANY -> ANY
                 falsehood: -> ANY
    ORDERING    LPO
                implies > truth > not > or > and > xor > and_star > falsehood
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
                  xor(X,Y) = or(and(X,not(Y)),and(not(X),Y)) 
                  xor(X,Y) = xor(Y,X) 
                  and_star(X,Y) = not(or(not(X),not(Y))) 
                  and_star(and_star(X,Y),Z) = and_star(X,and_star(Y,Z)) 
                  and_star(X,Y) = and_star(Y,X) 
                  not(truth) = falsehood 
    CONCLUSION