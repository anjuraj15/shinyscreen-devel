# narrow_summ

    Code
      ns
    Output
         set ID precid adduct tag       mz ms1_rt  ms1_int      Name
      1: AAs  4      6 [M+H]+  AA 120.0655      4 70481520 Threonine
      2: AAs  4      6 [M+H]+  AA 120.0655      4 70481520 Threonine
      3: AAs  4      6 [M+H]+  AA 120.0655      4 70481520 Threonine
      4: AAs  4     26 [M+H]+  KO 120.0655      4 80851056 Threonine
      5: AAs  4     26 [M+H]+  KO 120.0655      4 80851056 Threonine
      6: AAs  4     26 [M+H]+  KO 120.0655      4 80851056 Threonine
      7: AAs  4     46 [M+H]+  WT 120.0655      5 68987250 Threonine
      8: AAs  4     46 [M+H]+  WT 120.0655      5 68987250 Threonine
      9: AAs  4     46 [M+H]+  WT 120.0655      5 68987250 Threonine
                           SMILES qa_ms1_exists       scan ms2_sel
      1: C[C@@H](O)[C@H](N)C(O)=O          TRUE F1.S000058   FALSE
      2: C[C@@H](O)[C@H](N)C(O)=O          TRUE F1.S000059    TRUE
      3: C[C@@H](O)[C@H](N)C(O)=O          TRUE F1.S000060   FALSE
      4: C[C@@H](O)[C@H](N)C(O)=O          TRUE F1.S000267   FALSE
      5: C[C@@H](O)[C@H](N)C(O)=O          TRUE F1.S000268    TRUE
      6: C[C@@H](O)[C@H](N)C(O)=O          TRUE F1.S000269   FALSE
      7: C[C@@H](O)[C@H](N)C(O)=O          TRUE F1.S000477   FALSE
      8: C[C@@H](O)[C@H](N)C(O)=O          TRUE F1.S000478    TRUE
      9: C[C@@H](O)[C@H](N)C(O)=O          TRUE F1.S000479   FALSE

# make_eic_ms1_plot

    Code
      plt$labels
    Output
      $x
      [1] "retention time"
      
      $title
      [1] "MS1 EIC for ion m/z = 120.0655"
      
      $subtitle
      [1] "Threonine"
      
      $caption
      [1] "set: AAs; ID: 4"
      
      $y
      [1] "intensity"
      
      $colour
      [1] "label"
      

# make_eic_ms2_plot

    Code
      plt$labels
    Output
      $y
      [1] "intensity"
      
      $x
      [1] "retention time"
      
      $title
      [1] "MS2 EIC for ion m/z = 120.0655"
      
      $subtitle
      [1] "Threonine"
      
      $caption
      [1] "set: AAs; ID: 4"
      
      $ymin
      [1] "ymin"
      
      $ymax
      [1] "intensity"
      
      $colour
      [1] "label"
      

# make_spec_ms2_plot

    Code
      plt$labels
    Output
      $x
      [1] "m/z"
      
      $title
      [1] "MS2 spectra for ion m/z = 120.0655"
      
      $subtitle
      [1] "Threonine"
      
      $caption
      [1] "set: AAs; ID: 4"
      
      $ymin
      [1] "ymin"
      
      $ymax
      [1] "intensity"
      
      $colour
      [1] "label"
      

