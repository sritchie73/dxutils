# DNAnexus command line utility wrappers and extensions

Utilities for working with DNAnexus in R. Provides wrapper functions for common 
DNAnexus command line utilities and extends their functionality in ways that 
enhance user convenience and facilitate check-pointing for low-priority jobs.
Intended to be used as part of scripts run by the 
[run_script applet](https://github.com/sritchie73/dxapplet-run_script), which
provides the missing mechanism that enables running arbitrary scripts as DNA
nexus jobs while also provided upload access to DNAnexus project storage, which
makes it possible to implement check-pointing to reduce costs of long-running 
jobs by making it possible to run them on the much cheaper low-priority spot VMs
instead of the higher cost on-demand VMs that guarantee your VM won't be killed
and restarted by AWS ([learn more](https://dnanexus.gitbook.io/uk-biobank-rap/working-on-the-research-analysis-platform/managing-jobs/managing-job-priority))

- `dx_upload()`: Wrapper for the `dx upload` command line tool that: 

  (1) Assumes you only want to keep one copy of each file on DNAnexus, so 
      removes old copies before uploading.
      
  (2) Alternatively, can be set to skip uploads of files that already exist on
      DNAnexus (set `exists="skip"`), which makes it possible to resume 
      interrupted uploads of directories
      
  (3) When run as part of a DNAnexus job, attaches the DNAnexus job ID as a
      property to each file, making it possible for low-priority jobs that were 
      restarted mid-upload detect and clean up these incomplete files.
      
- `dx_download()`: Wrapper for the `dx download` command line tool that:
 
  (1) When run as part of a DNAnexus job and encountering an incomplete file, 
      compares the DNAnexus job ID to the one attached by `dx_upload()` to the 
      file, deleting the file if it matches the current DNAnexus job ID (i.e. 
      indicating this job was restarted mid-upload) or waits for the upload to
      finish before initiating the download (if the DNAnexus job ID property 
      does not match, indicating that another job is in the process of uploading
      the file).
      
  (2) Skips files that already exist on the local machine.
  
  (3) Can optionally skip or wait for files that do not exist yet on DNAnexus
      (default is to throw an error).

- `dx_rm()`: Wrapper for `dx rm` that also gracefully handles projects where
  the user has permissions to create and move files, but not delete them, by
  moving target files to a folder named "trash/" in the project.
    
- `dx_exists()`: check whether a file or folder exists on DNAnexus, optionally
  returning `FALSE` when encountering an incomplete file. Deletes incomplete 
  files if they match the current DNAnexus job ID (i.e. indicating that 
  current job is a low-priority job that was interuppted mid-upload).
    
- `assert_dx_exists()`: wrapper for `dx_exists()` that throws an error if 
  `dx_exists()` returns `FALSE`.

# Installation

On linux of Mac OS:

```
remotes::install_github("sritchie73/dxutils")
```

Windows is not currently supported.
