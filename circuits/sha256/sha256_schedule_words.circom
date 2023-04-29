pragma circom 2.0.0;
  
include "sha256_common.circom";

//------------------------------------------------------------------------------

// `i`-th bit of `arr` rotated right by `by`
function rotateR(arr, by, i) {
  return (i + by < 32) ? arr[i + by] : arr[i + by - 32];
}

// `i`-th bit of `arr` shifted right by `by`
function shiftR(arr, by, i) {
  return (i + by < 32) ? arr[i + by] : 0;
}

//------------------------------------------------------------------------------
// message schedule

template Sha256_schedule_words() {
  
  signal input  chunk_words[16];   // 16 words = 512 bits = 64 bytes
  signal output out_words[64];     // 64 words

  for(var i=0; i<16; i++) {
    out_words[i] <== chunk_words[i];
  }

  // for rotation and xor, we will need to bit-decompose
  signal bits[61][32];
  component tobits[15];

  // decompose the input words, except the very first, we don't need that
  for(var m=1; m<16; m++) {
    tobits[m-1] = ToBits(32);
    tobits[m-1].inp <== out_words[m];
    tobits[m-1].out ==> bits[m-1];  
  }

  component s0xor[48][32];
  component s1xor[48][32];
  component modulo[48];

  for(var m=16; m<64; m++) {
    var k = m-16;
    var l = k + (15-2);

    var s0_sum = 0;
    var s1_sum = 0;
  
    for(var i=0; i<32; i++) {

      // note: with XOR3_v2, circom optimizes away the constant zero `z` thing
      // with XOR3_v1, it does not. But otherwise it's the some number of constraints.

      s0xor[k][i] = XOR3_v2();
      s0xor[k][i].x <== rotateR( bits[k],  7, i);
      s0xor[k][i].y <== rotateR( bits[k], 18, i);
      s0xor[k][i].z <== shiftR ( bits[k],  3, i);
      s0_sum += (1<<i) * s0xor[k][i].out;
   
      s1xor[k][i] = XOR3_v2();
      s1xor[k][i].x <== rotateR( bits[l], 17, i);
      s1xor[k][i].y <== rotateR( bits[l], 19, i);
      s1xor[k][i].z <== shiftR ( bits[l], 10, i);
      s1_sum += (1<<i) * s1xor[k][i].out;

    }

    var tmp = out_words[m-16] + s0_sum + out_words[m-7] + s1_sum;

    modulo[k] = Mod34();
    modulo[k].inp <== tmp;
    modulo[k].out ==> out_words[m];

    if (m <= 61) {
      bits[m-1] <== modulo[k].bit;
    }

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
