pragma circom 2.0.0;

include "sha256_common.circom";
include "sha256_schedule_bits.circom";
include "sha256_rounds_bits.circom";

//------------------------------------------------------------------------------
// hashes 512 bits into 256 bits, without applying any padding
// this can be useful for constructing a Merkle tree

template Sha256_hash_chunk() {

  signal input  inp[512];              // 512 bits
  signal output out_hash[256];         // 256 bits (little-endian, kind of nonstandard order!)

  var initial_state[8] =  
        [ 0x6a09e667
        , 0xbb67ae85
        , 0x3c6ef372
        , 0xa54ff53a
        , 0x510e527f
        , 0x9b05688c
        , 0x1f83d9ab
        , 0x5be0cd19
        ];

  component sch = Sha256_schedule_bits();
  component rds = Sha256_rounds_bits(64); 

  for(var k=0; k<8; k++) {
    for(var i=0; i<32; i++) {
      rds.hash_bits[ k*32 + i ] <== (initial_state[k] >> i) & 1;
    }
  }

  for(var k=0; k<16; k++) {
    for(var i=0; i<32; i++) {
      sch.chunk_bits[ k*32 + i ] <== inp[ k*32 + (31-i) ];
    }
  }

  sch.out_words     ==> rds.words;
  rds.out_hash_bits ==> out_hash;

/*
  for(var k=0; k<32; k++) {
    var sum = 0;
    for(var i=0; i<8; i++) {
      sum += out_hash[ k*8 + i ] * (1<<i);
    }
    out_hash_bytes[ (k\4)*4 + (3-(k%4)) ] <== sum;
  }
*/

}

//------------------------------------------------------------------------------
