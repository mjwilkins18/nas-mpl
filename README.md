Hello! This repository contains the Constellation project initiative to translate the NASA Parallell Benchmark Suite kernels to Parallel ML.

To get started, run the following commands:
```
git submodule init
git submodule update
make setup
```

Only run the above commands the first time you set up the repository.

For every subsequent session, run:
```
make ENV
```
Then, you may proceed to the individual benchmarks of interest under `NPB`.
