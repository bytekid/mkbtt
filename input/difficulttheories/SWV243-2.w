    NAME        SWV243-2
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   c_Message_Oanalz: ANY -> ANY
                 c_union: ANY ANY ANY -> ANY
                 c_Message_Osynth: ANY -> ANY
                 tc_Message_Omsg: -> ANY
                 y: -> ANY
                 c_emptyset: -> ANY
                 a: -> ANY
    ORDERING    LPO
                c_Message_Oanalz > c_union > c_Message_Osynth > tc_Message_Omsg > y > c_emptyset > a
    VARIABLES  Message_Oanalz,Message_Osynth,V_G,V_H,Message_Omsg,V_y,T_a: ANY
    EQUATIONS   c_Message_Oanalz(c_union(c_Message_Osynth(V_G),V_H,tc_Message_Omsg)) = c_union(c_Message_Oanalz(c_union(V_G,V_H,tc_Message_Omsg)),c_Message_Osynth(V_G),tc_Message_Omsg) 
                  c_union(V_y,c_emptyset,T_a) = V_y 
    CONCLUSION