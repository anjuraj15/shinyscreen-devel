mk_shiny_control <- function(name,fun,args) {
    content = do.call(what=fun,args=c(list(inputId=name),args))
    res = list(name=name,
               fun=fun,
               content=content)
    res
}

inject_inputs<- function(collection,which) {
    do.call(shiny::tagList,lapply(which,function(nm) collection[[nm]]$content))
}

INPUTS_METFRAG_NUMERIC = list(mk_shiny_control(name="mf_database_search_relative_mass_deviation",
                                               fun="numericInput",
                                               args=list(label="Database search relative mass deviation",
                                                         value=5)),
                              mk_shiny_control(name="mf_fragment_peak_match_absolute_mass_deviation",
                                               fun="numericInput",
                                               args=list(label="Fragment peak match absolute mass deviation",
                                                         value=METFRAG_DEFAULT_ABSMASSDEV)),
                              mk_shiny_control(name="mf_fragment_peak_match_relative_mass_deviation",
                                               fun="numericInput",
                                               args=list(label="Fragment peak match relative mass deviation",
                                                         value=METFRAG_DEFAULT_RELMASSDEV)),
                              mk_shiny_control(name="mf_maximum_tree_depth",
                                               fun="numericInput",
                                               args=list(label="MaximumTreeDepth",
                                                         value=METFRAG_DEFAULT_MAX_TREE_DEPTH)))

INPUTS_METFRAG_SELECT = list(mk_shiny_control(name="mf_pre_processing_candidate_filter",
                                              fun="selectInput",
                                              args=list(label="Preprocessing candidate filter",
                                                        choices=shinyscreen:::METFRAG_PREPFLT_CHOICES,
                                                        selected=shinyscreen:::METFRAG_PREPFLT_DEFAULT,
                                                        multiple=T)),
                             mk_shiny_control(name="mf_post_processing_candidate_filter",
                                              fun="selectInput",
                                              args=list(label="Postprocessing candidate filter",
                                                        choices=shinyscreen:::METFRAG_POSTPFLT_CHOICES,
                                                        selected=shinyscreen:::METFRAG_POSTPFLT_DEFAULT,
                                                        multiple=T)),
                             mk_shiny_control(name="mf_metfrag_candidate_writer",
                                              fun="selectInput",
                                              args=list(label="MetFrag Candidate Writer",
                                                        choices=shinyscreen:::METFRAG_WRITER_CHOICES,
                                                        selected=shinyscreen:::METFRAG_DEFAULT_WRITER)),
                             mk_shiny_control(name="mf_database_type",
                                              fun="selectInput",
                                              args=list(label="Database type",
                                                        choices=METFRAG_DATABASE_TYPE,
                                                        selected=METFRAG_DEFAULT_DATABASE_TYPE)),
                             mk_shiny_control(name="mf_local_database",
                                              fun="selectInput",
                                              args=list(label="Local Database",
                                                        choices=character(0))),
                             mk_shiny_control(name="mf_local_db_col_ident",
                                              fun="selectInput",
                                              args=list(label="Select Identifiers",
                                                        multiple = T,
                                                        choices=character(0))),
                             mk_shiny_control(name="mf_local_db_col_coll",
                                              fun="selectInput",
                                              args=list(label="Identifiers for sets of results",
                                                        multiple = T,
                                                        choices=character(0))),
                             mk_shiny_control(name="mf_scores_intrinsic",
                                              fun="selectInput",
                                              args=list(label="Select Scoring Types",
                                                        choices = METFRAG_INTRINSIC_SCORES,
                                                        multiple = T,
                                                        selected = names(METFRAG_DEFAULT_SCORES))),
                             mk_shiny_control(name="mf_local_db_col_scores",
                                              fun="selectInput",
                                              args=list(label="Select local scoring terms",
                                                        multiple = T,
                                                        choices=character(0))))


INPUTS_METFRAG = c(INPUTS_METFRAG_NUMERIC, INPUTS_METFRAG_SELECT)

names(INPUTS_METFRAG) = sapply(INPUTS_METFRAG, function(x) x[["name"]])
