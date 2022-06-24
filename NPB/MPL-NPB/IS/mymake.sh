#!/bin/bash
mpl -default-ann 'allowFFI true' -export-header ffi-help.h -export-header wtime.h isS.sml is.c wtime.c c_timers.c c_print_results.c

