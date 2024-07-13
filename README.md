# Compile server for GNU R bytecode
This is implementation of compile server of GNU R bytecode done as a proof of concept. To run the server itself there should not be any additional dependencies other then those handled by Cargo. To run the test and benchmark you have to have GNU R interpreter installed and be able to run the `Rscipt` command. To be sure that the test can be run all the R scripts in root folder should be executable.


# Running
To run tests
```bash
mkdir temp # not necessary if it allready exists
cargo test
```

To run benchmark
```
cargo run -r -- -b
```

To run server
```
cargo run -r -- -s
```

Compile closure from RDS file
```
cargo run -r -- [file] -c
```

Output the representation of SEXP serialized in RDS file
```
cargo run -r -- [file] -d
```

