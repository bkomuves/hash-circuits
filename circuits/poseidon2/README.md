
Poseidon2 hash function in circom (for the bn128 curve)
-------------------------------------------------------

See the Poseidon2 paper here: <https://eprint.iacr.org/2023/323>

The implementation is hardcoded for the bn128 (aka bn254 aka bn256...) 
curve's scalar field and `t=3`; that is, the state consists of 3 field elements 
of the prime field with 

    p = 21888242871839275222246405745257275088548364400416034343698204186575808495617

TODO: 

- implement the [SAFE (Sponge API for Field Elements)](https://hackmd.io/bHgsH6mMStCVibM_wYvb2w)
  version of the sponge construction too
- implement other fields and state sizes

