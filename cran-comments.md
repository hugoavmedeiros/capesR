## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

* This is a new release.

## Fom Benjamin Altmann

### Comment 1

> Please provide a link to the used webservices (CAPES) to the description
> field of your DESCRIPTION file in the form
> <http:...> or <https:...>
> with angle brackets for auto-linking and no space after 'http:' and
> 'https:'.
> For more details:
> <https://contributor.r-project.org/cran-cookbook/description_issues.html#references>

Link added to Description field: [...]  (CAPES, <https://catalogodeteses.capes.gov.br>) [...]. Thanks.

### Comment 2

> \dontrun{} should only be used if the example really cannot be executed
> (e.g. because of missing additional software, missing API keys, ...) by
> the user. That's why wrapping examples in \dontrun{} adds the comment
> ("# Not run:") as a warning for the user. Does not seem necessary.
> Please replace \dontrun with \donttest.
> Please put functions which download data in \donttest{}.
> For more details:
> <https://contributor.r-project.org/cran-cookbook/general_issues.html#structuring-of-examples>

 I replaced the \dontrun{} block with \donttest{} for the examples. Thanks.


### Comment 3

> Please make sure that you do not change the user's options, par or
> working directory. If you really have to do so within functions, please
> ensure with an *immediate* call of on.exit() that the settings are reset
> when the function is exited.
> e.g.:
> ...
> old <- options() # code line i
> on.exit(options(old)) # code line i+1

Modified the download_capes_data: 

```r
 # Save the current timeout and restore it on exit
  original_timeout <- getOption("timeout")
  on.exit(options(timeout = original_timeout))
  
  # Set the new timeout
  options(timeout = timeout)
```


