#!/bin/bash
mpl -default-ann 'allowFFI true' -export-header ffi-help.h -export-header wtime.h v2.sml is_ffi.c wtime.c c_timers.c c_print_results.c

