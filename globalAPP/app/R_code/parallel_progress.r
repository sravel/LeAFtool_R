# progress information in parallel processes (that use the same filesystem)

# the master function sets up a tempfile for each process, spawns processes, and
# passes the corresponding tempfile location to each; each process dumps
# progress information into its tempfile; the master function polls those files
# for the progress information and returns it to the screen; the previous line
# is overwritten, as for progress bars
library (future)

# an environment to stash file info in, to hack around scoping issues. A package
# namespace could be used instead, but there's probably a more elegant solution.
mock_namespace <- new.env()
mock_namespace$file <- ""

# users can insert this in their code to send out progress information. Ideally
# this would be replaced with a progress bar.
update_parallel_progress <- function (i, n) {
  progress_text <- sprintf("%i%%\n", round(100 * i / n))
  cat(progress_text, file = mock_namespace$file)
}

run_job <- function (job_info) {
  # use a mock namespace to make the communication file visible to
  # update_parallel_progress on this run
  mock_namespace$file <- job_info$file
  eval(job_info$expression)
}

all_resolved <- function (futures) {
  each_resolved <- vapply(futures, resolved, FALSE)
  all(each_resolved)
}

# replicate expr in parallel across n_cores processes, and print live progress
# information for all the processes
future_replicate <- function (n, expr, simplify = "array") {

  jobs <- seq_len(n)

  # create tempfiles for communication and populate them with something
  files <- replicate(n, tempfile())

  lapply(files,
         function(file) {
           cat("0%\n", file = file)
         })

  # dispatch the jobs
  futures <- list()
  for (job in jobs) {
    mock_namespace$file <- files[[job]]
    expression <- substitute(expr)
    futures[[job]] <- future(eval.parent(expression))
  }

  # poll the files until all the jobs are complete
  while (!all_resolved(futures)) {

    # get and print the progress information
    job_text <- paste("job", jobs)
    progress_text <- vapply(files, readLines, "")
    all_text <- paste0(job_text, ": ", progress_text,
                       collapse = "    ")
    cat("\r", all_text)
    flush.console()

  }

  cat("\n")

  # get the values optionally simplify, and return
  results <- lapply(futures, value)

  if (!identical(simplify, FALSE) && length(results) > 0) {
    results <- simplify2array(results,
                              higher = (simplify == "array"))
  }

  results

}

# # demo of a user-defined function, with parallel progress information
#
# library (future)
# 
# foo <- function (n) {
#   for (i in seq_len(n)) {
#     update_parallel_progress(i, n)
#     Sys.sleep(runif(1))
#   }
#   "success!"
# }
# 
# plan(multiprocess)
# future_replicate(4, foo(30))
