pragma circom 2.0.0;
  
include "sha256_common.circom";

//------------------------------------------------------------------------------
// sha256 compression function inner loop
//
// note: the d,h,inp,key inputs (and outputs) are 32 bit numbers;
// the rest are little-endian bit vectors.

template Sha256_compress_inner() {
  
  signal input inp;
  signal input key;

  signal input a[32];    
  signal input b[32];
  signal input c[32];
  signal input dd;
  signal input e[32];
  signal input f[32];
  signal input g[32];
  signal input hh;

  signal output out_a[32];
  signal output out_b[32];
  signal output out_c[32];
  signal output out_dd;
  signal output out_e[32];
  signal output out_f[32];
  signal output out_g[32];
  signal output out_hh;

  var d_sum = 0;
  var h_sum = 0;
  for(var i=0; i<32; i++) {
    out_g[i] <== f[i];
    out_f[i] <== e[i];
    out_c[i] <== b[i];
    out_b[i] <== a[i];
    d_sum += (1<<i) * c[i];
    h_sum += (1<<i) * g[i];
  }
  out_dd <== d_sum;
  out_hh <== h_sum;
  
  signal chb[32];

  component major[32];
  component s0xor[32];
  component s1xor[32];

  var s0_sum = 0;
  var s1_sum = 0;
  var mj_sum = 0;
  var ch_sum = 0;

  for(var i=0; i<32; i++) {

    // ch(e,f,g) = if e then f else g = e(f-g)+g
    chb[i] <== e[i] * (f[i] - g[i]) + g[i];    
    ch_sum += (1<<i) * chb[i];

    // maj(a,b,c) = at least two of them is 1 = second bit of the sum
    major[i] = Bits2();
    major[i].xy <== a[i] + b[i] + c[i];
    mj_sum += (1<<i) * major[i].hi;

    s0xor[i] = XOR3_v1();
    s0xor[i].x <== a[ rotIdxR(i, 2) ];
    s0xor[i].y <== a[ rotIdxR(i,13) ];
    s0xor[i].z <== a[ rotIdxR(i,22) ];
    s0_sum += (1<<i) * s0xor[i].out;

    s1xor[i] = XOR3_v1();
    s1xor[i].x <== e[ rotIdxR(i, 6) ]; 
    s1xor[i].y <== e[ rotIdxR(i,11) ];
    s1xor[i].z <== e[ rotIdxR(i,25) ];
    s1_sum += (1<<i) * s1xor[i].out;

  }

  // // === debugging only ===
  // signal output s0 <== s0_sum;
  // signal output s1 <== s1_sum;
  // signal output mj <== mj_sum;
  // signal output ch <== ch_sum;

  signal overflow_e <== dd + hh + s1_sum + ch_sum + key + inp;
  signal overflow_a <==      hh + s1_sum + ch_sum + key + inp + s0_sum + mj_sum;

  component decompose_e = Bits35();
  decompose_e.inp <== overflow_e;
  decompose_e.out ==> out_e;

  component decompose_a = Bits35();
  decompose_a.inp <== overflow_a;
  decompose_a.out ==> out_a;

}

//------------------------------------------------------------------------------

/*

compress :: Word32 -> Word32 -> State -> State
compress key input (State [a,b,c,d,e,f,g,h]) = State [a',b',c',d',e',f',g',h'] where

  s0   = (a `rotateR` 2) `xor` (a `rotateR` 13) `xor` (a `rotateR` 22)
  maj  = (a  .&. b) `xor` (a  .&. c) `xor` (b .&. c)
  tmp2 = s0 + maj

  s1   = (e `rotateR` 6) `xor` (e `rotateR` 11) `xor` (e `rotateR` 25)
  ch   = (e  .&. f) `xor` ((complement e)  .&. g)
  tmp1 = h + s1 + ch + key + input
  
  h' = g
  g' = f
  f' = e
  e' = d + tmp1
  d' = c
  c' = b
  b' = a
  a' = tmp1 + tmp2

*/

//------------------------------------------------------------------------------

