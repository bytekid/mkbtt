    NAME        SWV262-2
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   c_Message_Oparts: ANY -> ANY
                 c_union: ANY ANY ANY -> ANY
                 tc_Message_Omsg: -> ANY
                 c_emptyset: -> ANY
                 y: -> ANY
                 a: -> ANY
                 c_insert: ANY ANY ANY -> ANY
    ORDERING    LPO
                c_Message_Oparts > c_union > tc_Message_Omsg > c_emptyset > y > a > c_insert
    VARIABLES  Message_Oparts,V_G,V_H,Message_Omsg,V_y,T_a,V_a,V_B,V_C: ANY
    EQUATIONS   c_Message_Oparts(c_union(V_G,V_H,tc_Message_Omsg)) = c_union(c_Message_Oparts(V_G),c_Message_Oparts(V_H),tc_Message_Omsg) 
                  c_union(c_emptyset,V_y,T_a) = V_y 
                  c_union(c_insert(V_a,V_B,T_a),V_C,T_a) = c_insert(V_a,c_union(V_B,V_C,T_a),T_a) 
    CONCLUSION