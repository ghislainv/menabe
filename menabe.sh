#!/bin/sh
echo "Executing menabe.R script in the background"
Rscript --vanilla menabe.R > menabe.log 2>&1 &
echo "Check the progress with command 'tail -f menabe.log'"
echo "Check the processor usage with command 'top'"
## End of script