# phintervals are formatted as expected

    Code
      print(c(phint1, phint2, na_phint, hole))
    Output
      <phinterval<UTC>[4]>
      [1] {1970-01-01--1970-01-01 00:00:10} <phint[3]>                       
      [3] <NA>                              <hole>                           

---

    Code
      print(phinterval(interval(origin, origin + 86400)))
    Output
      <phinterval<UTC>[1]>
      [1] {1970-01-01--1970-01-02}

---

    Code
      print(phint2, max_width = 9999)
    Output
      <phinterval<UTC>[1]>
      [1] {1970-01-01 00:00:00--1970-01-01 00:00:05, 1970-01-01 00:00:10--1970-01-01 00:00:15, 1970-01-01 00:00:20--1970-01-01 00:00:25}

---

    Code
      print(phinterval(interval(origin, origin + 86400, tzone = "EST")))
    Output
      <phinterval<EST>[1]>
      [1] {1969-12-31 19:00:00--1970-01-01 19:00:00}

---

    Code
      print(phinterval(interval(origin, origin + 86400, tzone = "")))
    Output
      <phinterval<local>[1]>
      [1] {1969-12-31 19:00:00--1970-01-01 19:00:00}

---

    Code
      print(c(phint1, phint2, na_phint, hole))
    Output
      <phinterval<UTC>[4]>
      [1] {1970-01-01--1970-01-01 00:00:10}
       [ Omitted 3 entries ]

