pragma circom 2.0.0;

include "sha512_common.circom";
include "sha512_schedule.circom";
include "sha512_rounds.circom";

//------------------------------------------------------------------------------
// hashes 1024 bits into 512 bits, without applying any padding
// this can be useful for constructing a Merkle tree

template Sha512_hash_chunk() {

  signal input  inp[1024];              // 1024 bits
  signal output out_hash[512];          // 512 bits (little-endian, kind of nonstandard order!)

  var initial_state[8] =  
        [ 0x6a09e667f3bcc908
        , 0xbb67ae8584caa73b
        , 0x3c6ef372fe94f82b
        , 0xa54ff53a5f1d36f1
        , 0x510e527fade682d1
        , 0x9b05688c2b3e6c1f
        , 0x1f83d9abfb41bd6b
        , 0x5be0cd19137e2179
        ]

  component sch = Sha512_schedule_bits();
  component rds = Sha512_rounds_bits(80); 

  for(var k=0; k<8; k++) {
    for(var i=0; i<64; i++) {
      rds.hash_bits[ k*64 + i ] <== (initial_state[k] >> i) & 1;
    }
  }

  for(var k=0; k<16; k++) {
    for(var i=0; i<64; i++) {
      sch.chunk_bits[ k*64 + i ] <== inp[ k*64 + (63-i) ];
    }
  }

  sch.out_words     ==> rds.words;
  rds.out_hash_bits ==> out_hash;

}

//------------------------------------------------------------------------------
