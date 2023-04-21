
Keccak implementation in circom
-------------------------------

- `keccak-p.circom`: implementation of the Keccak permutation
- `sponge.circom`: the sponge construction
- `sha3_bits.circom`: the NIST standard SHA3 hash + XOF functions, with the input being an array of bits
- `sha3_bytes.circom`: the SHA3 hash functions, with the input being an array of bytes
- `keccak_bits.circom`: the original Keccak hash, bits version
- `keccak_bytes.circom`: the original Keccak hash, bytes version

Remark: The difference between the NIST standard and the original Keccak 
specification is some additional "domain separation" bits appended to the end 
of the message.
