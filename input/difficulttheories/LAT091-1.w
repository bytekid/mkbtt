    NAME        LAT091-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   join: ANY ANY -> ANY
                 meet: ANY ANY -> ANY
    ORDERING    LPO
                join > meet
    VARIABLES  A,B,C: ANY
    EQUATIONS   join(meet(A,B),meet(A,join(A,B))) = A 
                  join(meet(A,A),meet(B,join(A,A))) = A 
                  join(meet(A,B),meet(B,join(A,B))) = B 
                  meet(meet(join(A,B),join(C,A)),A) = A 
                  join(join(meet(A,B),meet(C,A)),A) = A 
    CONCLUSION