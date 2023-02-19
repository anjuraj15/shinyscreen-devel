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
      Empty data.table (0 rows and 8 cols): ID,SMILES,Name,Formula,RT,mz...
      
      
      
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
      
      $run$metfrag$db_dir
      [1] ""
      
      $run$metfrag$param
      NULL
      
      $run$metfrag$java_bin
      [1] "java"
      
      
      
      attr(,"class")
      [1] "state"

# pack_ms2_w_summ

    Code
      res[1]
    Output
         adduct tag  ID   an      mz
      1: [M+H]+  WT 103 4656 127.073
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ms2_spectrum
      1: 55.0553245544434:2729.2705078125;56.0505180358887:1269.41748046875;57.0345306396484:1185.73815917969;57.0708847045898:2073.21337890625;57.5013008117676:904.784362792969;58.0297393798828:3069.81298828125;60.0565567016602:1773.03698730469;65.0367965698242:965.617248535156;68.0254364013672:2071.56469726562;68.0505218505859:1228.7607421875;69.0344924926758:2780.85595703125;69.0709075927734:3080.65991210938;70.0662078857422:1433.41735839844;70.5652694702148:1031.4853515625;71.0502624511719:1328.03637695312;72.04541015625:1446.455078125;74.0975341796875:976.407348632812;80.5268020629883:903.040588378906;81.045768737793:15966.4443359375;81.0707244873047:3006.37255859375;82.0296325683594:1390.12121582031;82.0661010742188:21125.375;83.0500411987305:2234.1494140625;83.0613174438477:1575.17407226562;83.0865173339844:4149.6015625;84.0451507568359:2479.02978515625;84.0818328857422:1646.0302734375;85.0518188476562:43653.81640625;86.060791015625:5526.36279296875;86.0974578857422:2208.45166015625;100.039741516113:1168.80969238281;100.076499938965:10643.6787109375;108.045013427734:2149.33618164062;109.040176391602:2858.38037109375;109.065277099609:1368.20178222656;109.076728820801:4338.21337890625;109.101722717285:1770.88696289062;110.046905517578:1130.73400878906;110.060676574707:1666.49230957031;110.071846008301:1194.94689941406;110.096809387207:1337.48522949219;111.044853210449:1951.84326171875;111.056098937988:5151.44970703125;112.076545715332:2555.54321289062;114.87516784668:1056.15844726562;125.071563720703:4535.6982421875;125.083320617676:1376.74731445312;125.10807800293:1415.62915039062;126.055450439453:5190.27294921875;126.066467285156:3053.17065429688;126.09147644043:5563.90673828125;127.051559448242:31754.853515625;127.073188781738:442951.8125;128.045745849609:1565.146484375;128.055435180664:1807.43249511719;128.069152832031:19987.1328125;128.077987670898:4699.71044921875;128.107467651367:57947.6953125;128.144165039062:4561.85009765625;129.066253662109:4863.453125;129.102523803711:7756.666015625;129.110992431641:1575.72912597656;143.601974487305:1347.22827148438

---

    Code
      res[2]
    Output
         adduct tag  ID   an       mz
      1: [M+H]+  AA 105 5200 205.0976
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ms2_spectrum
      1: 60.5048713684082:19345.2421875;74.0248489379883:37208.40625;113.136558532715:23108.2421875;114.001686096191:26036.02734375;117.057220458984:32610.748046875;118.065780639648:115168.796875;130.064971923828:55615.4765625;132.081100463867:248073.703125;143.073318481445:37559.25;144.044708251953:32569.009765625;144.081192016602:481554.625;145.08512878418:71837.0546875;146.060394287109:3291220;147.064086914062:303875.53125;159.092056274414:784477.0625;160.076583862305:37691.265625;160.095474243164:73295.8515625;170.060119628906:184498.125;174.452697753906:24287.78515625;188.071014404297:18407934;189.074432373047:2457461.75;190.076156616211:145566.625;205.097793579102:497672.25;205.116928100586:29461.1640625;206.100082397461:77902.703125

