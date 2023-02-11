mk_shiny_input <- function(name,fun,args) {
    content = do.call(what=fun,args=c(list(inputId=name),args))
    res = list(name=name,
               fun=fun,
               args=args,
               content=content)
    res
}

inject_inputs<- function(collection,which) {
    do.call(shiny::tagList,lapply(which,function(nm) collection[[nm]]$content))
}

inputs_label <- function(collection,which) {
    collection[[which]]$args$label
}

inputs_html_out <- function(collection, which, inline=T,...) {
    txt = inputs_label(collection, which)
    paste0('<code>',txt,'</code>')
}

INPUTS_METFRAG_NUMERIC = list(mk_shiny_input(name="mf_database_search_relative_mass_deviation",
                                               fun="numericInput",
                                               args=list(label="Database search relative mass deviation",
                                                         value=5)),
                              mk_shiny_input(name="mf_fragment_peak_match_absolute_mass_deviation",
                                               fun="numericInput",
                                               args=list(label="Fragment peak match absolute mass deviation",
                                                         value=METFRAG_DEFAULT_ABSMASSDEV)),
                              mk_shiny_input(name="mf_fragment_peak_match_relative_mass_deviation",
                                               fun="numericInput",
                                               args=list(label="Fragment peak match relative mass deviation",
                                                         value=METFRAG_DEFAULT_RELMASSDEV)),
                              mk_shiny_input(name="mf_maximum_tree_depth",
                                               fun="numericInput",
                                               args=list(label="MaximumTreeDepth",
                                                         value=METFRAG_DEFAULT_MAX_TREE_DEPTH)),
                              mk_shiny_input(name="mf_proc",
                                             fun="numericInput",
                                             args=list(label="Number of MetFrag Processes",
                                                       value=METFRAG_DEFAULT_PROC)))

INPUTS_METFRAG_SELECT_STANDARD = list(mk_shiny_input(name="mf_pre_processing_candidate_filter",
                                                     fun="selectInput",
                                                     args=list(label="Preprocessing candidate filter",
                                                               choices=shinyscreen:::METFRAG_PREPFLT_CHOICES,
                                                               selected=shinyscreen:::METFRAG_PREPFLT_DEFAULT,
                                                               multiple=T)),
                                      mk_shiny_input(name="mf_post_processing_candidate_filter",
                                                     fun="selectInput",
                                                     args=list(label="Postprocessing candidate filter",
                                                               choices=shinyscreen:::METFRAG_POSTPFLT_CHOICES,
                                                               selected=shinyscreen:::METFRAG_POSTPFLT_DEFAULT,
                                                               multiple=T)),
                                      mk_shiny_input(name="mf_metfrag_candidate_writer",
                                                     fun="selectInput",
                                                     args=list(label="MetFrag Candidate Writer",
                                                               choices=shinyscreen:::METFRAG_WRITER_CHOICES,
                                                               selected=shinyscreen:::METFRAG_DEFAULT_WRITER)),
                                      mk_shiny_input(name="mf_database_type",
                                                     fun="selectInput",
                                                     args=list(label="Database type",
                                                               choices=METFRAG_DATABASE_TYPE,
                                                               selected=METFRAG_DEFAULT_DATABASE_TYPE)),
                                      mk_shiny_input(name="mf_scores_intrinsic",
                                                  fun="selectInput",
                                                  args=list(label="Select Scoring Types",
                                                            choices = METFRAG_INTRINSIC_SCORES,
                                                            multiple = T,
                                                            selected = names(METFRAG_DEFAULT_SCORES))))

names(INPUTS_METFRAG_SELECT_STANDARD) = sapply(INPUTS_METFRAG_SELECT_STANDARD,function(x) x$name)

INPUTS_METFRAG_SELECT_LOCAL_DBCH = list(mk_shiny_input(name="mf_local_db_col_ident",
                                                       fun="selectInput",
                                                       args=list(label="Select Identifiers",
                                                                 multiple = T,
                                                                 choices=character(0))),
                                        mk_shiny_input(name="mf_local_db_col_coll",
                                                       fun="selectInput",
                                                       args=list(label="Identifiers for sets of results",
                                                                 multiple = T,
                                                                 choices=character(0))),
                                        mk_shiny_input(name="mf_local_db_col_scores",
                                                       fun="selectInput",
                                                       args=list(label="Select local scoring terms",
                                                                 multiple = T,
                                                                 choices=character(0))))

names(INPUTS_METFRAG_SELECT_LOCAL_DBCH) = sapply(INPUTS_METFRAG_SELECT_LOCAL_DBCH,function(x) x$name)

INPUTS_METFRAG_SELECT_LOCAL_OTHER = list(mk_shiny_input(name="mf_local_database",
                                                    fun="selectInput",
                                                    args=list(label="Local Database",
                                                              choices=character(0))))

names(INPUTS_METFRAG_SELECT_LOCAL_OTHER) = sapply(INPUTS_METFRAG_SELECT_LOCAL_OTHER,function(x) x$name)

INPUTS_METFRAG_SELECT_LOCAL = c(INPUTS_METFRAG_SELECT_LOCAL_OTHER,
                                INPUTS_METFRAG_SELECT_LOCAL_DBCH)

names(INPUTS_METFRAG_SELECT_LOCAL) = sapply(INPUTS_METFRAG_SELECT_LOCAL,function(x) x$name)


INPUTS_METFRAG = c(INPUTS_METFRAG_NUMERIC, INPUTS_METFRAG_SELECT_STANDARD, INPUTS_METFRAG_SELECT_LOCAL)

names(INPUTS_METFRAG) = sapply(INPUTS_METFRAG, function(x) x[["name"]])
