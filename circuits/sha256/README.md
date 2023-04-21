
SHA2-256 implementation in circom
---------------------------------

- `sha_common.circom`: some helper templates
- `sha_compress.circom`: inner loop of the compression functions
- `sha_rounds_words.circom`: the `n`-round compression function, where the hash state is 8 dwords 
- `sha_rounds_bits.circom`: the `n`-round compression function, where the hash state is 256 bits
- `sha_schedule_words.circom`: the "message schedule", where the message chunk is 16 dwords
- `sha_schedule_bits.circom`: the "message schedule", where the message chunk is 512 bits
- `sha_hash_pair.circom`: hash a pair of field elements
- `sha_hash_bytes.circom`: SHA256 hash of a sequence of bytes

Note: the "bit" versions are a little bit more efficient. The "word" version
are kept here for the possibility of comparison.
