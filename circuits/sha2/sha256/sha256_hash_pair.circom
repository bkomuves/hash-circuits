pragma circom 2.0.0;

include "sha256_common.circom";
include "sha256_schedule_bits.circom";
include "sha256_rounds_bits.circom";

//------------------------------------------------------------------------------
// Hashes a pair of field elements (assumed to be at most 256 bits),
// WITHOUT applying the padding described in the standard.
//
// Then maps the resulting 256 bit hash back into the field.
// 
// This can be useful for building Merkle trees.

template Sha256_hash_field_pair() {
  signal input  pair[2];
  signal output out_hash;

  component bits0 = ToBits(256);
  component bits1 = ToBits(256);

  bits0.inp <== pair[0];
  bits1.inp <== pair[1];

  component iv  = Sha256_initial_value();
  component sch = SHA2_224_256_schedule();
  component rds = SHA2_224_256_rounds(64); 

  iv.out         ==> rds.inp_hash;
  sch.out_words  ==> rds.words;
  rds.out_hash   ==> out_hash;

  for(var i=0; i<256; i++) {
    sch.chunk_bits[(i\32)  ][i%32] <== bits0.out[i];
    sch.chunk_bits[(i\32)+8][i%32] <== bits1.out[i];
  }

  sch.out_words ==> rds.words;

  for(var k=0; k<8; k++) {
    for(var i=0; i<32; i++) {
      rds.hash_bits[ k*32 + i ] <== (initial_state[k] >> i) & 1;
    }
  }

  var sum  = 0;
  var mult = 1;
  for(var i=0; i<256; i++) {
    sum  += rds.out_hash_bits[i] * mult;   // NOTE: `(1<<i)` does not work correctly if `i>log(p)`
    mult *= 2;                              
  }
  out_hash <== sum;

}
