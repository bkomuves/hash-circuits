pragma circom 2.0.0;
  
include "sha256_common.circom";

//------------------------------------------------------------------------------
// message schedule
//
// NOTE: the 512 bits are in little-endian order here!
//

template Sha256_schedule_bits() {
  
  signal input  chunk_bits[512];   // 512 bits  = 16 words = 64 bytes
  signal output out_words[64];     // 64 words

  signal out_bits[2048];           // 2048 bits = 64 words = 256 bytes

  for(var i=0; i<512; i++) {
    out_bits[i] <== chunk_bits[i];
  }

  for(var k=0; k<16; k++) {
    var sum = 0;
    for(var i=0; i<32; i++) { sum += (1<<i) * out_bits[k*32+i]; }
    out_words[k] <== sum;
  }

  component s0xor[48][32];
  component s1xor[48][32];
  component modulo[48];

  for(var m=16; m<64; m++) {
    var r = m-16;
    var k = m-15;
    var l = k + (15-2);

    var s0_sum = 0;
    var s1_sum = 0;
  
    for(var i=0; i<32; i++) {

      // note: with XOR3_v2, circom optimizes away the constant zero `z` thing
      // with XOR3_v1, it does not. But otherwise it's the same number of constraints.

      s0xor[r][i] = XOR3_v2();
      s0xor[r][i].x <==               out_bits[ 32*k + (i +  7) % 32 ]     ;
      s0xor[r][i].y <==               out_bits[ 32*k + (i + 18) % 32 ]     ;
      s0xor[r][i].z <== (i < 32- 3) ? out_bits[ 32*k + (i +  3)      ] : 0 ;
      s0_sum += (1<<i) * s0xor[r][i].out;
   
      s1xor[r][i] = XOR3_v2();
      s1xor[r][i].x <==               out_bits[ 32*l + (i + 17) % 32 ]     ;
      s1xor[r][i].y <==               out_bits[ 32*l + (i + 19) % 32 ]     ;
      s1xor[r][i].z <== (i < 32-10) ? out_bits[ 32*l + (i + 10)      ] : 0 ;
      s1_sum += (1<<i) * s1xor[r][i].out;

    }

    var tmp = out_words[m-16] + s0_sum + out_words[m-7] + s1_sum;

    modulo[r] = Mod34();
    modulo[r].inp <== tmp;
    var out_sum = 0;
    for(var i=0; i<32; i++) {
      modulo[r].bit[i] ==> out_bits[m*32+i];
      out_sum +=  (1<<i) * out_bits[m*32+i];
    }
    out_words[m] <== out_sum;

  }
}

//------------------------------------------------------------------------------

/*
    Extend the first 16 words into the remaining 48 words w[16..63] of the message schedule array:
    for i from 16 to 63
        s0 := (w[i-15] rightrotate  7) xor (w[i-15] rightrotate 18) xor (w[i-15] rightshift  3)
        s1 := (w[i-2]  rightrotate 17) xor (w[i-2]  rightrotate 19) xor (w[i-2]  rightshift 10)
        w[i] := w[i-16] + s0 + w[i-7] + s1
*/

//------------------------------------------------------------------------------
