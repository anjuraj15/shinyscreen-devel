# filetag_add_file works properly

    Code
      output1
    Output
      $file
      [1] "file1.mzML" "file2.mzML" "file3.mzML"
      
      $tag
      [1] "F1" "F2" "F3"
      

---

    Code
      output2
    Output
      $file
      [1] "file1.mzML" "file2.mzML" "file3.mzML" "file4.mzML"
      
      $tag
      [1] "F1" "XY" "F3" "F4"
      

---

    Code
      output3
    Output
      $file
      [1] "file1.mzML" "file2.mzML" "file3.mzML" "file4.mzML"
      
      $tag
      [1] "F1" "XY" "F3" "F4"
      

# datatab_update_tags works properly

    Code
      out1
    Output
      $tag
      character(0)
      
      $adduct
      character(0)
      
      $set
      character(0)
      
      $file
      character(0)
      

---

    Code
      out2
    Output
      $tag
      [1] "A" "B"
      
      $adduct
      [1] NA NA
      
      $set
      [1] NA NA
      
      $file
      [1] "f1.f" "f2.f"
      

---

    Code
      out3
    Output
      $tag
      [1] "X" "B"
      
      $adduct
      [1] NA NA
      
      $set
      [1] NA NA
      
      $file
      [1] "f1.f" "f2.f"
      

---

    Code
      out4
    Output
      $tag
      [1] "Y" "Z"
      
      $adduct
      [1] NA NA
      
      $set
      [1] NA NA
      
      $file
      [1] "fx.f" "fy.f"
      

