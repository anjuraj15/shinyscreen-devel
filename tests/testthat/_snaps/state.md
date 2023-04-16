# Test empty project creation.

    Code
      res
    Output
      $conf
      $project
      [1] NA
      
      $compounds
      $compounds$lists
      list()
      
      $compounds$sets
      [1] ""
      
      
      $debug
      [1] FALSE
      
      $tolerance
      $tolerance$`ms1 coarse`
      [1] "0.5 Da"
      
      $tolerance$`ms1 fine`
      [1] "5 ppm"
      
      $tolerance$eic
      [1] "0.001 Da"
      
      $tolerance$rt
      [1] "0.5 min"
      
      
      $extract
      $extract$missing_precursor_info
      [1] "do_nothing"
      
      
      $prescreen
      $prescreen$ms1_int_thresh
      [1] 1e+05
      
      $prescreen$ms2_int_thresh
      [1] 2500
      
      $prescreen$s2n
      [1] 3
      
      $prescreen$ret_time_shift_tol
      [1] "0.5 min"
      
      
      $figures
      $figures$rt_min
      [1] "NA_real_ min"
      
      $figures$rt_max
      [1] "NA_real_ min"
      
      $figures$ext
      [1] "pdf"
      
      
      $metfrag
      $metfrag$db_file
      [1] ""
      
      $metfrag$param
      $metfrag$param$MetFragDatabaseType
      [1] ""
      
      $metfrag$param$FragmentPeakMatchAbsoluteMassDeviation
      [1] 0.001
      
      $metfrag$param$FragmentPeakMatchRelativeMassDeviation
      [1] 5
      
      $metfrag$param$DatabaseSearchRelativeMassDeviation
      [1] 5
      
      $metfrag$param$MetFragCandidateWriter
      [1] "CSV"
      
      $metfrag$param$SampleName
      [1] "sample"
      
      $metfrag$param$MaximumTreeDepth
      [1] 2
      
      $metfrag$param$MetFragPreProcessingCandidateFilter
      [1] "UnconnectedCompoundFilter,IsotopeFilter"
      
      $metfrag$param$MetFragPostProcessingCandidateFilter
      [1] "InChIKeyFilter"
      
      
      $metfrag$intrinsic_scores
      $metfrag$intrinsic_scores$FragmenterScore
      [1] 1
      
      $metfrag$intrinsic_scores$OfflineIndividualMoNAScore
      [1] 1
      
      
      $metfrag$database_scores
      list()
      
      $metfrag$cand_parameters
      character(0)
      
      $metfrag$collect_candidates
      character(0)
      
      $metfrag$nproc
      [1] 1
      
      
      attr(,"class")
      [1] "conf" "list"
      
      $out
      $out$tab
      $out$tab$comp
      Empty data.table (0 rows and 11 cols): ID,mz,rt,adduct,tag,set...
      
      
      
      $input
      $input$tab
      $input$tab$mzml
      Empty data.table (0 rows and 4 cols): file,tag,adduct,set
      
      $input$tab$lists
      list()
      
      $input$tab$L0
      Empty data.table (0 rows and 9 cols): ID,SMILES,Name,Formula,RT,mz...
      
      
      
      $run
      $run$project
      [1] "test_project"
      
      $run$paths
             project           data 
      "test_project"             "" 
      
      $run$metfrag
      $run$metfrag$cando_local
      [1] FALSE
      
      $run$metfrag$cando_remote
      [1] TRUE
      
      $run$metfrag$cando_metfrag
      [1] TRUE
      
      $run$metfrag$path
      [1] ""
      
      $run$metfrag$subpaths
      $run$metfrag$subpaths$results
      [1] ""
      
      $run$metfrag$subpaths$config
      [1] ""
      
      $run$metfrag$subpaths$spec
      [1] ""
      
      $run$metfrag$subpaths$log
      [1] ""
      
      
      $run$metfrag$runtime
      [1] "metfrag.jar"
      
      $run$metfrag$db_file
      [1] ""
      
      $run$metfrag$param
      NULL
      
      $run$metfrag$java_bin
      [1] "java"
      
      
      
      attr(,"class")
      [1] "state"

# pack_project

    Code
      fls
    Output
      [1] "test_project/file1.csv"             "test_project/file2.csv"            
      [3] "test_project/subdir/filesubdir.csv"

# process_cmpd_sets

    Code
      x
    Output
         ID   ORIG  set
      1:  1 f1.csv   f1
      2:  2 f1.csv   f1
      3:  3 f2.csv  aks
      4:  4 f3.csv se31
      5:  5 f3.csv se32

---

    Code
      x
    Output
         ID         ORIG  set
      1:  1     b/f2.csv f2_1
      2:  2     a/f1.csv   f1
      3:  3 q/f/g/f2.csv  aks
      4:  4 d/e/f/f3.csv se31
      5:  5 m/n/q/f3.csv se32

---

    Code
      x
    Output
         ID     ORIG  set
      1:  1 a/fn.csv fn_1
      2:  2 a/fn.csv fn_1
      3:  3 a/fn.csv  set
      4:  4 a/fn.csv set2
      5:  5 a/fn.csv fn_1
      6:  6 b/fn.csv fn_2
      7:  7 b/fn.csv set2
      8:  8 b/fn.csv set2
      9:  9 b/fn.csv fn_2

---

    Code
      x
    Output
      Empty data.table (0 rows and 9 cols): ID,SMILES,Name,Formula,RT,mz...

