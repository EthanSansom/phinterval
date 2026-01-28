# print() and pillar_shaft() work as expected

    Code
      print(phint)
    Output
      <phinterval<UTC>[7]>
      [1] {2020-01-01--2020-02-17}                                                                              
      [2] {2020-01-01 00:00:17.005--2020-02-17 00:07:30.0245}                                                   
      [3] {2020-01-01 00:00:17.005--2020-02-17 00:07:30.0245, 2020-03-10 00:01:29.000--2021-09-15 00:16:45.0000}
      [4] {-Inf--Inf}                                                                                           
      [5] {-Inf--2020-01-01 00:00:17.005, 2020-03-10 00:01:29--Inf}                                             
      [6] <hole>                                                                                                
      [7] <NA>                                                                                                  

---

    Code
      print(phint_tib)
    Output
      # A tibble: 7 x 1
        phint                                                                                                 
        <phint<UTC>>                                                                                          
      1 {2020-01-01--2020-02-17}                                                                              
      2 {2020-01-01 00:00:17.005--2020-02-17 00:07:30.0245}                                                   
      3 {2020-01-01 00:00:17.005--2020-02-17 00:07:30.0245, 2020-03-10 00:01:29.000--2021-09-15 00:16:45.0000}
      4 {-Inf--Inf}                                                                                           
      5 {-Inf--2020-01-01 00:00:17.005, 2020-03-10 00:01:29--Inf}                                             
      6 <hole>                                                                                                
      7 <NA>                                                                                                  

---

    Code
      print(phint)
    Output
      <phinterval<UTC>[7]>
      [1] {2020-01-01--2020-02-17}                                                            
      [2] {2020-01-01 00:00:17--2020-02-17 00:07:30}                                          
      [3] {2020-01-01 00:00:17--2020-02-17 00:07:30, 2020-03-10 00:01:29--2021-09-15 00:16:45}
      [4] {-Inf--Inf}                                                                         
      [5] {-Inf--2020-01-01 00:00:17, 2020-03-10 00:01:29--Inf}                               
      [6] <hole>                                                                              
      [7] <NA>                                                                                

---

    Code
      print(phint_tib)
    Output
      # A tibble: 7 x 1
        phint                                                                               
        <phint<UTC>>                                                                        
      1 {2020-01-01--2020-02-17}                                                            
      2 {2020-01-01 00:00:17--2020-02-17 00:07:30}                                          
      3 {2020-01-01 00:00:17--2020-02-17 00:07:30, 2020-03-10 00:01:29--2021-09-15 00:16:45}
      4 {-Inf--Inf}                                                                         
      5 {-Inf--2020-01-01 00:00:17, 2020-03-10 00:01:29--Inf}                               
      6 <hole>                                                                              
      7 <NA>                                                                                

---

    Code
      print(phint)
    Output
      <phinterval<UTC>[7]>
      [1] {2020-01-01 00:00:00--2020-02-17 00:00:00}   
      [2] {2020-01-01 00:00:17--2020-02-17 00:07:30}   
      [3] {2020-01-01 00:00:17-[2]-2021-09-15 00:16:45}
      [4] {-Inf--Inf}                                  
      [5] {-Inf-[2]-Inf}                               
      [6] <hole>                                       
      [7] <NA>                                         

---

    Code
      print(phint_tib)
    Output
      # A tibble: 7 x 1
        phint                                        
        <phint<UTC>>                                 
      1 {2020-01-01 00:00:00--2020-02-17 00:00:00}   
      2 {2020-01-01 00:00:17--2020-02-17 00:07:30}   
      3 {2020-01-01 00:00:17-[2]-2021-09-15 00:16:45}
      4 {-Inf--Inf}                                  
      5 {-Inf-[2]-Inf}                               
      6 <hole>                                       
      7 <NA>                                         

---

    Code
      print(phint)
    Output
      <phinterval<UTC>[7]>
      [1] <phint[1]> <phint[1]>
      [3] <phint[2]> <phint[1]>
      [5] <phint[2]> <hole>    
      [7] <NA>      

---

    Code
      print(phint_tib)
    Output
      # A tibble: 7 x 1
        phint       
        <phint<UTC>>
      1 <phint[1]>  
      2 <phint[1]>  
      3 <phint[2]>  
      4 <phint[1]>  
      5 <phint[2]>  
      6 <hole>      
      7 <NA>        

# print() and pillar_shaft() emit warning on unrecognized time zone

    Code
      print(phint)
    Output
      <phinterval<badzone>[1]>
    Condition <rlang_warning>
      Warning in `obj_print_data()`:
      x `attr(x, "tzone")` is an unrecognized time zone: "badzone".
      i Unrecognized timezones are formatted using the time zone: "UTC".
      i Run `tzdb_names()` to see recognized timezones.
      This warning is displayed once per session.
    Output
      [1] {1970-01-02--1970-01-03}

---

    Code
      print(phint)
    Output
      <phinterval<badzone>[1]>
      [1] {1970-01-02--1970-01-03}

---

    Code
      print(phint_tib)
    Output
      # A tibble: 1 x 1
    Condition <rlang_warning>
      Warning in `format()`:
      x `attr(x, "tzone")` is an unrecognized time zone: "badzone".
      i Unrecognized timezones are formatted using the time zone: "UTC".
      i Run `tzdb_names()` to see recognized timezones.
      This warning is displayed once per session.
    Output
        phint                   
        <phint<badzone>>        
      1 {1970-01-02--1970-01-03}

---

    Code
      print(phint_tib)
    Output
      # A tibble: 1 x 1
        phint                   
        <phint<badzone>>        
      1 {1970-01-02--1970-01-03}

