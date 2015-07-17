    NAME        BOO075-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   nand: ANY ANY -> ANY
    ORDERING    LPO
                nand
    VARIABLES  A,B,C: ANY
    EQUATIONS   nand(nand(A,nand(nand(B,A),A)),nand(B,nand(C,A))) = B 
    CONCLUSION