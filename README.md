
hash-circuits
=============

copyright: (c) 2023 Faulhorn Labs  
author: Balazs Komuves  
license: MIT  
disclaimer: Experimental software, use at your own risk!  

What's this about?
------------------

This repository contains [`circom`](https://docs.circom.io/) implementations 
of some popular hash functions:

- SHA2: SHA224 / SHA256 / SHA384 / SHA512
- Keccak / SHA3: Keccak-224, Keccak-256, Keccak-384, Keccak-512 + the SHA3 versions
- SHA3 XOFs: SHAKE128, SHAKE256
- Poseidon2 (for the bn128 scalar field and t=3)

Circom is a programming language describing arithmetic circuits and/or R1CS 
constraints. These are used in some constructions of zero-knowledge proofs
and related technologies.

Circuit sizes
-------------

The author believes that the R1CS / arithmetic circuit format allows very limited
opportunities for optimizing hash function circuits, nevertheless some effort 
was put into optimization.

Approximate counts of nonlinear R1CS constraints:

- Permutation:
    - Keccak-f[1600]: 146000
    - Poseidon2:      240
- Compression (two-to-one):
    - SHA256:         26170
    - Keccak256:      144830
    - Poseidon2:      240
- Hashing (per native chunk):
    - SHA256:         26200 (about 13% less than the one in `circomlib`)
    - Keccak256:      147000
    - Poseidon2:      250
- Hashing (per byte):
    - SHA256:         410
    - Keccak256:      1100
    - Poseidon2:      8

Tests and reference implementation
----------------------------------

Reference implementations and tests are written in [Haskell](https://www.haskell.org/). 
The tests are using the [`r1cs-solver`](https://bitbucket.org/faulhorn/r1cs-solver) 
testing framework (though we mostly only use to trivial part of that, automating the 
`circom` workflow).

TODO
----

- more detailed tests for the individual components (permutation, compression, etc)
- more comprehensive testing (using the reference implementations)
- implement reference Poseidon2 for general parameter settings 
- implement the original Poseidon
- test the R1CS equations (if possible in practice)
- Haskell reference implementation for Keccak sponge
- add more Keccak constructions and variants
- implement other hash functions
