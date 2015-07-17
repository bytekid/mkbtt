    NAME        LCL162-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   xor: ANY ANY -> ANY
                 and_star: ANY ANY -> ANY
                 not: ANY -> ANY
                 truth: -> ANY
                 falsehood: -> ANY
                 implies: ANY ANY -> ANY
                 or: ANY ANY -> ANY
                 and: ANY ANY -> ANY
    ORDERING    LPO
                xor > and_star > not > truth > falsehood > implies > or > and
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   xor(X,Y) = xor(Y,X) 
                  and_star(and_star(X,Y),Z) = and_star(X,and_star(Y,Z)) 
                  and_star(X,Y) = and_star(Y,X) 
                  not(truth) = falsehood 
                  implies(X,Y) = xor(truth,and_star(X,xor(truth,Y))) 
                  xor(X,Y) = or(and(X,not(Y)),and(not(X),Y)) 
                  xor(X,Y) = xor(Y,X) 
                  and_star(X,Y) = not(or(not(X),not(Y))) 
                  and_star(and_star(X,Y),Z) = and_star(X,and_star(Y,Z)) 
                  and_star(X,Y) = and_star(Y,X) 
                  not(truth) = falsehood 
                  not(X) = xor(X,truth) 
                  xor(X,falsehood) = X 
                  xor(X,X) = falsehood 
                  and_star(X,truth) = X 
                  and_star(X,falsehood) = falsehood 
                  and_star(xor(truth,X),X) = falsehood 
                  xor(X,xor(truth,Y)) = xor(xor(X,truth),Y) 
                  and_star(xor(and_star(xor(truth,X),Y),truth),Y) = and_star(xor(and_star(xor(truth,Y),X),truth),X) 
    CONCLUSION