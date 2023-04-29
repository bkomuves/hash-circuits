pragma circom 2.0.0;

include "sha512_common.circom";
include "sha512_schedule.circom";
include "sha512_rounds.circom";

//------------------------------------------------------------------------------
// Computes the SHA512 hash of a sequence of bits

template Sha512_hash_bits(len) {

  signal input  inp[len];              // `n` bits
  signal output out_hash[512];         // 512 bits (little-endian, kind of nonstandard order!)

  var nchunks = ((len + 1 + 128) + 1023) \ 1024;
  var nbits   = nchunks * 512;
  var pad_k   = nbits - 128 - 1 - len;

  signal input_bits[nchunks*1024];
  signal states[nchunks+1][512];

  for(var i=0; i<len; i++) {
    inp[i] ==> input_bits[i];
   }

  input_bits[len] <== 1;
  for(var i=len+1; i<nbits-128; i++) { input_bits[i] <== 0; }

  component len_tb = ToBits(128);
  len_tb.inp <== len;
  for(var i=0; i<128; i++) { input_bits[nbits-1-i] <== len_tb.out[i]; }

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

  for(var k=0; k<8; k++) {
    for(var i=0; i<64; i++) {
      states[0][ k*64 + i ] <== (initial_state[k] >> i) & 1;
    }
  }

  component sch[nchunks]; 
  component rds[nchunks]; 

  for(var m=0; m<nchunks; m++) { 

    sch[m] = Sha512_schedule_bits();
    rds[m] = Sha512_rounds_bits(80); 

    for(var k=0; k<16; k++) {
      for(var i=0; i<64; i++) {
        sch[m].chunk_bits[ k*64 + i ] <== input_bits[ m*1024 + k*64 + (63-i) ];
      }
    }

    sch[m].out_words ==> rds[m].words;

    rds[m].hash_bits     <== states[m];
    rds[m].out_hash_bits ==> states[m+1];
  }

  out_hash <== states[nchunks];

/*
  for(var k=0; k<64; k++) {
    var sum = 0;
    for(var i=0; i<8; i++) {
      sum += out_hash[ k*8 + i ] * (1<<i);
    }
    out_hash_bytes[ (k\8)*8 + (7-(k%8)) ] <== sum;
  }
*/

}

//------------------------------------------------------------------------------
