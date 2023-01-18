# Do adducts affect MetFrag config generation correctly?

    Code
      cfconf
    Output
       [1] "MetFragDatabaseType = \nFragmentPeakMatchAbsoluteMassDeviation = 0.001\nFragmentPeakMatchRelativeMassDeviation = 5\nDatabaseSearchRelativeMassDeviation = 5\nMetFragCandidateWriter = CSV\nSampleName = sample_a_[M+H]+_a\nMaximumTreeDepth = 2\nMetFragPreProcessingCandidateFilter = UnconnectedCompoundFilter,IsotopeFilter\nMetFragPostProcessingCandidateFilter = InChIKeyFilter\nMetFragScoreTypes = FragmenterScore,OfflineIndividualMoNAScore\nMetFragScoreWeights = 1,1\nIonizedPrecursorMass = 777.7789\nIsPositiveIonMode = True\nPrecursorIonMode = 1\nResultsPath = results\nPeakListPath = spec/sample_a_[M+H]+_a.csv\n\n"             
       [2] "MetFragDatabaseType = \nFragmentPeakMatchAbsoluteMassDeviation = 0.001\nFragmentPeakMatchRelativeMassDeviation = 5\nDatabaseSearchRelativeMassDeviation = 5\nMetFragCandidateWriter = CSV\nSampleName = sample_a_[M+NH4]+_a\nMaximumTreeDepth = 2\nMetFragPreProcessingCandidateFilter = UnconnectedCompoundFilter,IsotopeFilter\nMetFragPostProcessingCandidateFilter = InChIKeyFilter\nMetFragScoreTypes = FragmenterScore,OfflineIndividualMoNAScore\nMetFragScoreWeights = 1,1\nIonizedPrecursorMass = 777.7789\nIsPositiveIonMode = True\nPrecursorIonMode = 18\nResultsPath = results\nPeakListPath = spec/sample_a_[M+NH4]+_a.csv\n\n"        
       [3] "MetFragDatabaseType = \nFragmentPeakMatchAbsoluteMassDeviation = 0.001\nFragmentPeakMatchRelativeMassDeviation = 5\nDatabaseSearchRelativeMassDeviation = 5\nMetFragCandidateWriter = CSV\nSampleName = sample_a_[M+Na]+_a\nMaximumTreeDepth = 2\nMetFragPreProcessingCandidateFilter = UnconnectedCompoundFilter,IsotopeFilter\nMetFragPostProcessingCandidateFilter = InChIKeyFilter\nMetFragScoreTypes = FragmenterScore,OfflineIndividualMoNAScore\nMetFragScoreWeights = 1,1\nIonizedPrecursorMass = 777.7789\nIsPositiveIonMode = True\nPrecursorIonMode = 23\nResultsPath = results\nPeakListPath = spec/sample_a_[M+Na]+_a.csv\n\n"          
       [4] "MetFragDatabaseType = \nFragmentPeakMatchAbsoluteMassDeviation = 0.001\nFragmentPeakMatchRelativeMassDeviation = 5\nDatabaseSearchRelativeMassDeviation = 5\nMetFragCandidateWriter = CSV\nSampleName = sample_a_[M+K]+_a\nMaximumTreeDepth = 2\nMetFragPreProcessingCandidateFilter = UnconnectedCompoundFilter,IsotopeFilter\nMetFragPostProcessingCandidateFilter = InChIKeyFilter\nMetFragScoreTypes = FragmenterScore,OfflineIndividualMoNAScore\nMetFragScoreWeights = 1,1\nIonizedPrecursorMass = 777.7789\nIsPositiveIonMode = True\nPrecursorIonMode = 39\nResultsPath = results\nPeakListPath = spec/sample_a_[M+K]+_a.csv\n\n"            
       [5] "MetFragDatabaseType = \nFragmentPeakMatchAbsoluteMassDeviation = 0.001\nFragmentPeakMatchRelativeMassDeviation = 5\nDatabaseSearchRelativeMassDeviation = 5\nMetFragCandidateWriter = CSV\nSampleName = sample_a_[M+CH3OH+H]+_a\nMaximumTreeDepth = 2\nMetFragPreProcessingCandidateFilter = UnconnectedCompoundFilter,IsotopeFilter\nMetFragPostProcessingCandidateFilter = InChIKeyFilter\nMetFragScoreTypes = FragmenterScore,OfflineIndividualMoNAScore\nMetFragScoreWeights = 1,1\nIonizedPrecursorMass = 777.7789\nIsPositiveIonMode = True\nPrecursorIonMode = 33\nResultsPath = results\nPeakListPath = spec/sample_a_[M+CH3OH+H]+_a.csv\n\n"
       [6] "MetFragDatabaseType = \nFragmentPeakMatchAbsoluteMassDeviation = 0.001\nFragmentPeakMatchRelativeMassDeviation = 5\nDatabaseSearchRelativeMassDeviation = 5\nMetFragCandidateWriter = CSV\nSampleName = sample_a_[M+ACN+H]+_a\nMaximumTreeDepth = 2\nMetFragPreProcessingCandidateFilter = UnconnectedCompoundFilter,IsotopeFilter\nMetFragPostProcessingCandidateFilter = InChIKeyFilter\nMetFragScoreTypes = FragmenterScore,OfflineIndividualMoNAScore\nMetFragScoreWeights = 1,1\nIonizedPrecursorMass = 777.7789\nIsPositiveIonMode = True\nPrecursorIonMode = 42\nResultsPath = results\nPeakListPath = spec/sample_a_[M+ACN+H]+_a.csv\n\n"    
       [7] "MetFragDatabaseType = \nFragmentPeakMatchAbsoluteMassDeviation = 0.001\nFragmentPeakMatchRelativeMassDeviation = 5\nDatabaseSearchRelativeMassDeviation = 5\nMetFragCandidateWriter = CSV\nSampleName = sample_a_[M+ACN+Na]+_a\nMaximumTreeDepth = 2\nMetFragPreProcessingCandidateFilter = UnconnectedCompoundFilter,IsotopeFilter\nMetFragPostProcessingCandidateFilter = InChIKeyFilter\nMetFragScoreTypes = FragmenterScore,OfflineIndividualMoNAScore\nMetFragScoreWeights = 1,1\nIonizedPrecursorMass = 777.7789\nIsPositiveIonMode = True\nPrecursorIonMode = 64\nResultsPath = results\nPeakListPath = spec/sample_a_[M+ACN+Na]+_a.csv\n\n"  
       [8] "MetFragDatabaseType = \nFragmentPeakMatchAbsoluteMassDeviation = 0.001\nFragmentPeakMatchRelativeMassDeviation = 5\nDatabaseSearchRelativeMassDeviation = 5\nMetFragCandidateWriter = CSV\nSampleName = sample_a_[M+2ACN+H]+_a\nMaximumTreeDepth = 2\nMetFragPreProcessingCandidateFilter = UnconnectedCompoundFilter,IsotopeFilter\nMetFragPostProcessingCandidateFilter = InChIKeyFilter\nMetFragScoreTypes = FragmenterScore,OfflineIndividualMoNAScore\nMetFragScoreWeights = 1,1\nIonizedPrecursorMass = 777.7789\nIsPositiveIonMode = True\nPrecursorIonMode = 83\nResultsPath = results\nPeakListPath = spec/sample_a_[M+2ACN+H]+_a.csv\n\n"  
       [9] "MetFragDatabaseType = \nFragmentPeakMatchAbsoluteMassDeviation = 0.001\nFragmentPeakMatchRelativeMassDeviation = 5\nDatabaseSearchRelativeMassDeviation = 5\nMetFragCandidateWriter = CSV\nSampleName = sample_a_[M-H]-_a\nMaximumTreeDepth = 2\nMetFragPreProcessingCandidateFilter = UnconnectedCompoundFilter,IsotopeFilter\nMetFragPostProcessingCandidateFilter = InChIKeyFilter\nMetFragScoreTypes = FragmenterScore,OfflineIndividualMoNAScore\nMetFragScoreWeights = 1,1\nIonizedPrecursorMass = 777.7789\nIsPositiveIonMode = False\nPrecursorIonMode = -1\nResultsPath = results\nPeakListPath = spec/sample_a_[M-H]-_a.csv\n\n"           
      [10] "MetFragDatabaseType = \nFragmentPeakMatchAbsoluteMassDeviation = 0.001\nFragmentPeakMatchRelativeMassDeviation = 5\nDatabaseSearchRelativeMassDeviation = 5\nMetFragCandidateWriter = CSV\nSampleName = sample_a_[M+Cl]-_a\nMaximumTreeDepth = 2\nMetFragPreProcessingCandidateFilter = UnconnectedCompoundFilter,IsotopeFilter\nMetFragPostProcessingCandidateFilter = InChIKeyFilter\nMetFragScoreTypes = FragmenterScore,OfflineIndividualMoNAScore\nMetFragScoreWeights = 1,1\nIonizedPrecursorMass = 777.7789\nIsPositiveIonMode = False\nPrecursorIonMode = 35\nResultsPath = results\nPeakListPath = spec/sample_a_[M+Cl]-_a.csv\n\n"         
      [11] "MetFragDatabaseType = \nFragmentPeakMatchAbsoluteMassDeviation = 0.001\nFragmentPeakMatchRelativeMassDeviation = 5\nDatabaseSearchRelativeMassDeviation = 5\nMetFragCandidateWriter = CSV\nSampleName = sample_a_[M+HCOO]-_a\nMaximumTreeDepth = 2\nMetFragPreProcessingCandidateFilter = UnconnectedCompoundFilter,IsotopeFilter\nMetFragPostProcessingCandidateFilter = InChIKeyFilter\nMetFragScoreTypes = FragmenterScore,OfflineIndividualMoNAScore\nMetFragScoreWeights = 1,1\nIonizedPrecursorMass = 777.7789\nIsPositiveIonMode = False\nPrecursorIonMode = 45\nResultsPath = results\nPeakListPath = spec/sample_a_[M+HCOO]-_a.csv\n\n"     
      [12] "MetFragDatabaseType = \nFragmentPeakMatchAbsoluteMassDeviation = 0.001\nFragmentPeakMatchRelativeMassDeviation = 5\nDatabaseSearchRelativeMassDeviation = 5\nMetFragCandidateWriter = CSV\nSampleName = sample_a_[M+CH3COO]-_a\nMaximumTreeDepth = 2\nMetFragPreProcessingCandidateFilter = UnconnectedCompoundFilter,IsotopeFilter\nMetFragPostProcessingCandidateFilter = InChIKeyFilter\nMetFragScoreTypes = FragmenterScore,OfflineIndividualMoNAScore\nMetFragScoreWeights = 1,1\nIonizedPrecursorMass = 777.7789\nIsPositiveIonMode = False\nPrecursorIonMode = 49\nResultsPath = results\nPeakListPath = spec/sample_a_[M+CH3COO]-_a.csv\n\n" 
      [13] "MetFragDatabaseType = \nFragmentPeakMatchAbsoluteMassDeviation = 0.001\nFragmentPeakMatchRelativeMassDeviation = 5\nDatabaseSearchRelativeMassDeviation = 5\nMetFragCandidateWriter = CSV\nSampleName = sample_a_[M+]+_a\nMaximumTreeDepth = 2\nMetFragPreProcessingCandidateFilter = UnconnectedCompoundFilter,IsotopeFilter\nMetFragPostProcessingCandidateFilter = InChIKeyFilter\nMetFragScoreTypes = FragmenterScore,OfflineIndividualMoNAScore\nMetFragScoreWeights = 1,1\nIonizedPrecursorMass = 777.7789\nIsPositiveIonMode = True\nPrecursorIonMode = 0\nResultsPath = results\nPeakListPath = spec/sample_a_[M+]+_a.csv\n\n"               
      [14] "MetFragDatabaseType = \nFragmentPeakMatchAbsoluteMassDeviation = 0.001\nFragmentPeakMatchRelativeMassDeviation = 5\nDatabaseSearchRelativeMassDeviation = 5\nMetFragCandidateWriter = CSV\nSampleName = sample_a_[M-]-_a\nMaximumTreeDepth = 2\nMetFragPreProcessingCandidateFilter = UnconnectedCompoundFilter,IsotopeFilter\nMetFragPostProcessingCandidateFilter = InChIKeyFilter\nMetFragScoreTypes = FragmenterScore,OfflineIndividualMoNAScore\nMetFragScoreWeights = 1,1\nIonizedPrecursorMass = 777.7789\nIsPositiveIonMode = False\nPrecursorIonMode = 0\nResultsPath = results\nPeakListPath = spec/sample_a_[M-]-_a.csv\n\n"              

# function metfrag_run returns correct result.

    Code
      ftab
    Output
          ID adduct tag   an CE
      1: 100 [M+H]+  AA 5413 10
      2: 100 [M+H]+  KO 5434 10
                                                                f_conf
      1: config/testsample_setNTs_ID100_adduct[M+H]+_tagAA_an5413.conf
      2: config/testsample_setNTs_ID100_adduct[M+H]+_tagKO_an5434.conf
                                                             f_log
      1: log/testsample_setNTs_ID100_adduct[M+H]+_tagAA_an5413.log
      2: log/testsample_setNTs_ID100_adduct[M+H]+_tagKO_an5434.log
                                                             f_spec
      1: spec/testsample_setNTs_ID100_adduct[M+H]+_tagAA_an5413.csv
      2: spec/testsample_setNTs_ID100_adduct[M+H]+_tagKO_an5434.csv
                                           stag
      1: setNTs_ID100_adduct[M+H]+_tagAA_an5413
      2: setNTs_ID100_adduct[M+H]+_tagKO_an5434
                                                         f_res
      1: testsample_setNTs_ID100_adduct[M+H]+_tagAA_an5413.csv
      2: testsample_setNTs_ID100_adduct[M+H]+_tagKO_an5434.csv

---

    Code
      x
    Output
          ID adduct tag   an CE                                   stag num_poss_IDs
      1: 100 [M+H]+  AA 5413 10 setNTs_ID100_adduct[M+H]+_tagAA_an5413           86
      2: 100 [M+H]+  KO 5434 10 setNTs_ID100_adduct[M+H]+_tagKO_an5434           86
         max_Score n_Score_GE4 n_Score_GE3 n_Score_GE2 Identifier
      1:  4.938569           1           1           1       6057
      2:  4.753741           1           1           1       6057
                                           CompoundName Max_FragmenterScore
      1: (2S)-2-amino-3-(4-hydroxyphenyl)propanoic acid            485.2443
      2: (2S)-2-amino-3-(4-hydroxyphenyl)propanoic acid            679.6480
         Max_OfflineIndividualMoNAScore Max_PubMed_Count Max_Patent_Count
      1:                        0.99441           107881            76367
      2:                        0.83380           107881            76367
         Max_AnnoTypeCount
      1:                 9
      2:                 9
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   All_Identifier
      1: 6057;6950578;73562;91482;562171;440311;3135546;25418841;17607993;22397448;14451762;72214162;47003404;9442556;447184;602411;45167;20826205;4123006;168705;438;2734814;89843;6422115;12919999;13052275;10678961;7436;201474;223277;73705;220089;72878;113330;7186;175593;18605013;824655;23548;6861469;40148016;2798463;117636;71327507;90407;15480193;89978;67455263;49866537;19756142;81446487;89905;65711231;231242;6871292;10726079;12315638;4105139;3080594;7275229;59323545;18625738;62234616;44600733;21885916;232526;71756776;55266573;139835;122651682;251544;24820110;16218074;59176;21191822;528966;602319;13384341;836717;519003;153707036;602288;81657;597077;197180;13672729
      2: 6057;6950578;73562;91482;562171;440311;438;14451762;72214162;22397448;45167;602411;3135546;47003404;17607993;9442556;25418841;7436;10678961;6422115;7186;201474;168705;89843;4123006;447184;13052275;20826205;2734814;220089;12919999;223277;73705;113330;72878;175593;824655;23548;6861469;67455263;18605013;40148016;19756142;89905;81446487;59176;2798463;90407;13384341;15480193;6871292;49866537;71327507;231242;12315638;65711231;117636;71756776;4105139;89978;3080594;59323545;18625738;13672729;251544;55266573;24820110;10726079;232526;16218074;21885916;44600733;7275229;62234616;122651682;139835;528966;836717;602319;153707036;602288;519003;21191822;597077;197180;81657

