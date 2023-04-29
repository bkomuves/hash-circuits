pragma circom 2.0.0;
  
include "sha512_common.circom";
include "sha512_compress.circom";

//------------------------------------------------------------------------------
// NOTE: hash_bits are little-endian!

template Sha512_rounds_bits(n) {
 
  signal input  words[n];               // round words (64-bit words)
  signal input  hash_bits[512];         // initial state
  signal output out_hash_bits[512];     // final state after n rounds (n <= 64)

  signal  a[64][n+1];
  signal  b[64][n+1];
  signal  c[64][n+1];
  signal  dd[n+1];
  signal  e[64][n+1];
  signal  f[64][n+1];
  signal  g[64][n+1];
  signal  hh[n+1];

  var round_keys[80] =
        [ 0x428a2f98d728ae22 , 0x7137449123ef65cd , 0xb5c0fbcfec4d3b2f , 0xe9b5dba58189dbbc
        , 0x3956c25bf348b538 , 0x59f111f1b605d019 , 0x923f82a4af194f9b , 0xab1c5ed5da6d8118
        , 0xd807aa98a3030242 , 0x12835b0145706fbe , 0x243185be4ee4b28c , 0x550c7dc3d5ffb4e2
        , 0x72be5d74f27b896f , 0x80deb1fe3b1696b1 , 0x9bdc06a725c71235 , 0xc19bf174cf692694
        , 0xe49b69c19ef14ad2 , 0xefbe4786384f25e3 , 0x0fc19dc68b8cd5b5 , 0x240ca1cc77ac9c65
        , 0x2de92c6f592b0275 , 0x4a7484aa6ea6e483 , 0x5cb0a9dcbd41fbd4 , 0x76f988da831153b5
        , 0x983e5152ee66dfab , 0xa831c66d2db43210 , 0xb00327c898fb213f , 0xbf597fc7beef0ee4
        , 0xc6e00bf33da88fc2 , 0xd5a79147930aa725 , 0x06ca6351e003826f , 0x142929670a0e6e70
        , 0x27b70a8546d22ffc , 0x2e1b21385c26c926 , 0x4d2c6dfc5ac42aed , 0x53380d139d95b3df
        , 0x650a73548baf63de , 0x766a0abb3c77b2a8 , 0x81c2c92e47edaee6 , 0x92722c851482353b
        , 0xa2bfe8a14cf10364 , 0xa81a664bbc423001 , 0xc24b8b70d0f89791 , 0xc76c51a30654be30
        , 0xd192e819d6ef5218 , 0xd69906245565a910 , 0xf40e35855771202a , 0x106aa07032bbd1b8
        , 0x19a4c116b8d2d0c8 , 0x1e376c085141ab53 , 0x2748774cdf8eeb99 , 0x34b0bcb5e19b48a8
        , 0x391c0cb3c5c95a63 , 0x4ed8aa4ae3418acb , 0x5b9cca4f7763e373 , 0x682e6ff3d6b2b8a3
        , 0x748f82ee5defb2fc , 0x78a5636f43172f60 , 0x84c87814a1f0ab72 , 0x8cc702081a6439ec
        , 0x90befffa23631e28 , 0xa4506cebde82bde9 , 0xbef9a3f7b2c67915 , 0xc67178f2e372532b
        , 0xca273eceea26619c , 0xd186b8c721c0c207 , 0xeada7dd6cde0eb1e , 0xf57d4f7fee6ed178
        , 0x06f067aa72176fba , 0x0a637dc5a2c898a6 , 0x113f9804bef90dae , 0x1b710b35131c471b
        , 0x28db77f523047d84 , 0x32caab7b40c72493 , 0x3c9ebe0a15c9bebc , 0x431d67c49c100d4c
        , 0x4cc5d4becb3e42b6 , 0x597f299cfc657e2a , 0x5fcb6fab3ad6faec , 0x6c44198c4a475817
        ];

  var sum_dd = 0;
  var sum_hh = 0;
  for(var i=0; i<64; i++) {
    a[i][0] <== hash_bits [ 0*64 + i ];
    b[i][0] <== hash_bits [ 1*64 + i ];
    c[i][0] <== hash_bits [ 2*64 + i ];
    sum_dd  +=  hash_bits [ 3*64 + i ] * (1<<i);  
    e[i][0] <== hash_bits [ 4*64 + i ];
    f[i][0] <== hash_bits [ 5*64 + i ];
    g[i][0] <== hash_bits [ 6*64 + i ];
    sum_hh  +=  hash_bits [ 7*64 + i ] * (1<<i);  
  }
  dd[0] <== sum_dd;
  hh[0] <== sum_hh;

  signal hash_words[8];
  for(var j=0; j<8; j++) {
    var sum = 0;
    for(var i=0; i<64; i++) {
      sum += (1<<i) * hash_bits[ j*64 + i ];
    }
    hash_words[j] <== sum;
  }

  component compress[n];  

  for(var k=0; k<n; k++) {

    compress[k] = Sha512_compress_inner();

    compress[k].inp <== words[k];
    compress[k].key <== round_keys[k];

    for(var i=0; i<64; i++) {
      compress[k].a[i] <== a[i][k];
      compress[k].b[i] <== b[i][k];
      compress[k].c[i] <== c[i][k];
    
      compress[k].e[i] <== e[i][k];
      compress[k].f[i] <== f[i][k];
      compress[k].g[i] <== g[i][k];
    }

    compress[k].dd <== dd[k];
    compress[k].hh <== hh[k];

    for(var i=0; i<64; i++) {
      compress[k].out_a[i] ==> a[i][k+1];
      compress[k].out_b[i] ==> b[i][k+1];
      compress[k].out_c[i] ==> c[i][k+1];
    
      compress[k].out_e[i] ==> e[i][k+1];
      compress[k].out_f[i] ==> f[i][k+1];
      compress[k].out_g[i] ==> g[i][k+1];
    }

    compress[k].out_dd ==> dd[k+1];
    compress[k].out_hh ==> hh[k+1];
  }

  component modulo[8];
  for(var j=0; j<8; j++) {
    modulo[j] = Bits65();
  }

  var sum_a = 0;
  var sum_b = 0;
  var sum_c = 0;
  var sum_e = 0;
  var sum_f = 0;
  var sum_g = 0;
  for(var i=0; i<64; i++) {
    sum_a += (1<<i) * a[i][n];
    sum_b += (1<<i) * b[i][n];
    sum_c += (1<<i) * c[i][n];
    sum_e += (1<<i) * e[i][n];
    sum_f += (1<<i) * f[i][n];
    sum_g += (1<<i) * g[i][n];
  }
  
  modulo[0].inp <== hash_words[0] + sum_a;
  modulo[1].inp <== hash_words[1] + sum_b;
  modulo[2].inp <== hash_words[2] + sum_c;
  modulo[3].inp <== hash_words[3] + dd[n];
  modulo[4].inp <== hash_words[4] + sum_e;
  modulo[5].inp <== hash_words[5] + sum_f;
  modulo[6].inp <== hash_words[6] + sum_g;
  modulo[7].inp <== hash_words[7] + hh[n];

  for(var j=0; j<8; j++) {
    for(var i=0; i<64; i++) {
      modulo[j].out[i] ==> out_hash_bits[j*64+i];
    }
  }

}

// -----------------------------------------------------------------------------
