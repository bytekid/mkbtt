    NAME        LAT093-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   join: ANY ANY -> ANY
                 meet: ANY ANY -> ANY
                 joi: -> ANY
    ORDERING    LPO
                join > meet > joi
    VARIABLES  A,B,C,D,E,F: ANY
    EQUATIONS   join(meet(join(meet(A,B),meet(B,join(A,B))),C),meet(join(meet(A,join(join(meet(B,D),meet(E,B)),B)),meet(join(meet(B,meet(meet(join(B,D),join(E,B)),B)),meet(F,join(B,meet(meet(join(B,D),join(E,B)),B)))),join(A,join(join(meet(B,D),meet(E,B)),B)))),join(joi
    CONCLUSION