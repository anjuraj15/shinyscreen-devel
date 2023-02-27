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
      

