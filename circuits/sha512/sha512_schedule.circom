pragma circom 2.0.0;
  
include "sha512_common.circom";

//------------------------------------------------------------------------------
// message schedule
//
// NOTE: the 512 bits are in little-endian order here!
//

template Sha512_schedule_bits() {
  
  signal input  chunk_bits[1024];  // 1024 bits  = 16 qwords = 128 bytes
  signal output out_words[80];     // 80 words

  signal out_bits[5120];           // 5120 bits = 80 qwords = 640 bytes

  for(var i=0; i<1024; i++) {
    out_bits[i] <== chunk_bits[i];
  }

  for(var k=0; k<16; k++) {
    var sum = 0;
    for(var i=0; i<64; i++) { sum += (1<<i) * out_bits[k*64+i]; }
    out_words[k] <== sum;
  }

  component s0xor[80-16][64];
  component s1xor[80-16][64];
  component modulo[80-16];

  for(var m=16; m<80; m++) {
    var r = m-16;
    var k = m-15;
    var l = k + (15-2);

    var s0_sum = 0;
    var s1_sum = 0;
  
    for(var i=0; i<64; i++) {

      // note: with XOR3_v2, circom optimizes away the constant zero `z` thing
      // with XOR3_v1, it does not. But otherwise it's the same number of constraints.

      s0xor[r][i] = XOR3_v2();
      s0xor[r][i].x <==               out_bits[ 64*k + (i +  1) % 64 ]     ;
      s0xor[r][i].y <==               out_bits[ 64*k + (i +  8) % 64 ]     ;
      s0xor[r][i].z <== (i < 64- 7) ? out_bits[ 64*k + (i +  7)      ] : 0 ;
      s0_sum += (1<<i) * s0xor[r][i].out;
   
      s1xor[r][i] = XOR3_v2();
      s1xor[r][i].x <==               out_bits[ 64*l + (i + 19) % 64 ]     ;
      s1xor[r][i].y <==               out_bits[ 64*l + (i + 61) % 64 ]     ;
      s1xor[r][i].z <== (i < 64- 6) ? out_bits[ 64*l + (i +  6)      ] : 0 ;
      s1_sum += (1<<i) * s1xor[r][i].out;

    }

    var tmp = s1_sum + out_words[m-7] + s0_sum + out_words[m-16] ;

    modulo[r] = Bits66();
    modulo[r].inp <== tmp;
    for(var i=0; i<64; i++) { modulo[r].out[i] ==> out_bits[m*64+i]; }
    out_words[m] <== modulo[r].out_word;

  }
}

//------------------------------------------------------------------------------
