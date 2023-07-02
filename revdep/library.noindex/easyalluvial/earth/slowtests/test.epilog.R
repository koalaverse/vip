# test.epilog.R

if(!interactive()) {
    dev.off()         # finish postscript plot
    q(runLast=FALSE)  # needed else R prints the time on exit
                      # (R2.5 and higher) which messes up the diffs
}
