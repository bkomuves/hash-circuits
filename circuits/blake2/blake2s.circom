pragma circom 2.0.0;

include "blake2_common.circom";

//------------------------------------------------------------------------------

template IV() {
  signal output out[8];

  var initializationVector[8] = 
    [ 0x6A09E667 , 0xBB67AE85 , 0x3C6EF372 , 0xA54FF53A
    , 0x510E527F , 0x9B05688C , 0x1F83D9AB , 0x5BE0CD19
    ];

  for(var j=0; j<8; j++) { out[j] <== initializationVector[j]; }
}

//------------------------------------------------------------------------------
// XOR-s two 32-bit vectors and then rotates the result right by the given amount of bits

template RotXorBits(R) {
  signal input  inp1_bits[32];
  signal input  inp2_bits[32];
  signal output out_bits[32];
  signal output out_word;

  signal aux[32];
  for(var i=0; i<32; i++) {
    aux[i] <== inp1_bits[i] + inp2_bits[i] - 2 * inp1_bits[i] * inp2_bits[i];
  }

  var acc = 0;
  for(var i=0; i<32; i++) {
    out_bits[i] <== aux[ (i+R) % 32 ];
    acc += out_bits[i] * (2**i);
  }

  out_word <== acc;
}

//--------------------------------------
// XOR-s a 32-bit word with a bit-vector
// and then rotates the result right by the given amount of bits

template RotXorWordBits(R) {
  signal input  inp1_word;
  signal input  inp2_bits[32];
  signal output out_bits[32];
  signal output out_word;

  component tb = ToBits(32);
  component rx = RotXorBits(R);

  tb.inp    <== inp1_word;
  tb.out    ==> rx.inp1_bits;
  inp2_bits ==> rx.inp2_bits;
  out_bits  <== rx.out_bits;
  out_word  <== rx.out_word;
}

//------------------------------------------------------------------------------

template HalfFunG(a,b,c,d, R1,R2) {
  signal input  v[16];
  signal input  xy;
  signal output out[16];

  for(var i=0; i<16; i++) {
    if ((i!=a) && (i!=b) && (i!=c) && (i!=d)) {
      out[i] <== v[i];
    }
  }

  component add1 = Bits34();        // sum of three words needs 34 bits
  component add3 = Bits33();        // sum of two words only needs 33 bits

  component rxor2 = RotXorWordBits(R1);
  component rxor4 = RotXorWordBits(R2);

  add1.inp      <== v[a] + v[b] + xy;
  v[d]          ==> rxor2.inp1_word;
  add1.out_bits ==> rxor2.inp2_bits;
  add3.inp      <== v[c] + rxor2.out_word;
  v[b]          ==> rxor4.inp1_word;
  add3.out_bits ==> rxor4.inp2_bits;

  out[a] <== add1.out_word;
  out[d] <== rxor2.out_word;
  out[c] <== add3.out_word;
  out[b] <== rxor4.out_word;

/*
       |   v[a] := (v[a] + v[b] + xy) mod 2**w
       |   v[d] := (v[d] ^ v[a]) >>> R1
       |   v[c] := (v[c] + v[d])     mod 2**w
       |   v[b] := (v[b] ^ v[c]) >>> R2
*/

}

//------------------------------------------------------------------------------
// the mixing function G

// inputs and output and x,y consists of 32 bit words
template MixFunG(a,b,c,d) {
  signal input  inp[16];
  signal output out[16];
  signal input x;
  signal input y;

  component half1 = HalfFunG(a,b,c,d, 16,12);
  component half2 = HalfFunG(a,b,c,d,  8, 7);

  half1.v   <== inp;
  half1.xy  <== x;
  half1.out ==> half2.v;
  half2.xy  <== y;
  half2.out ==> out;

/*         
       |   v[a] := (v[a] + v[b] + x) mod 2**w
       |   v[d] := (v[d] ^ v[a]) >>> R1
       |   v[c] := (v[c] + v[d])     mod 2**w
       |   v[b] := (v[b] ^ v[c]) >>> R2

       |   v[a] := (v[a] + v[b] + y) mod 2**w
       |   v[d] := (v[d] ^ v[a]) >>> R3
       |   v[c] := (v[c] + v[d])     mod 2**w
       |   v[b] := (v[b] ^ v[c]) >>> R4
*/


//  for(var i=0; i<16; i++) {
//    log("mixfun_output[", i, "] = ", out[i]);
//  }

}

//------------------------------------------------------------------------------
// a single round

template SingleRound(round_idx) {
  signal input  inp[16];
  signal input  msg[16];
  signal output out[16];

  var s[16];
  s = Sigma(round_idx);

  component GS[8];

  signal vs[9][16];

  inp ==> vs[0];

  GS[0] = MixFunG(  0 ,  4 ,  8 , 12 ) ; GS[0].x <== msg[s[ 0]] ; GS[0].y <== msg[s[ 1]] ;
  GS[1] = MixFunG(  1 ,  5 ,  9 , 13 ) ; GS[1].x <== msg[s[ 2]] ; GS[1].y <== msg[s[ 3]] ;
  GS[2] = MixFunG(  2 ,  6 , 10 , 14 ) ; GS[2].x <== msg[s[ 4]] ; GS[2].y <== msg[s[ 5]] ;
  GS[3] = MixFunG(  3 ,  7 , 11 , 15 ) ; GS[3].x <== msg[s[ 6]] ; GS[3].y <== msg[s[ 7]] ; 
 
  GS[4] = MixFunG(  0 ,  5 , 10 , 15 ) ; GS[4].x <== msg[s[ 8]] ; GS[4].y <== msg[s[ 9]] ;
  GS[5] = MixFunG(  1 ,  6 , 11 , 12 ) ; GS[5].x <== msg[s[10]] ; GS[5].y <== msg[s[11]] ;
  GS[6] = MixFunG(  2 ,  7 ,  8 , 13 ) ; GS[6].x <== msg[s[12]] ; GS[6].y <== msg[s[13]] ;
  GS[7] = MixFunG(  3 ,  4 ,  9 , 14 ) ; GS[7].x <== msg[s[14]] ; GS[7].y <== msg[s[15]] ;

  for(var i=0; i<8; i++) {
    GS[i].inp <== vs[i];
    GS[i].out ==> vs[i+1];
  }

  out <== vs[8];

//  for(var i=0; i<16; i++) {
//    log("round_output[", i, "] = ", out[i]);
//  }

}

//------------------------------------------------------------------------------
// the compression function F
//
// t is the offset counter
// f should be 1 for the final block and 0 otherwise
//
template CompressionF(t,f) {
  signal input  h[8];         // the state (8 words)
  signal input  m[16];        // the message block (16 words)
  signal output out[8];       // new state

  component iv = IV();
  signal init[16];

  for(var i=0; i<8; i++) { init[i  ] <== h[i];      }
  for(var i=0; i<8; i++) { init[i+8] <== iv.out[i]; }

  signal vs[11][16];

  component xor1 = XorWordConst( 32 , t &  0xFFFFFFFF         );
  component xor2 = XorWordConst( 32 , t >> 32                 );
  component xor3 = XorWordConst( 32 , (f==0) ? 0 : 0xFFFFFFFF );

  for(var i=0; i<12; i++) { vs[0][i] <== init[i]; }
  xor1.inp_word <== init[12]; xor1.out_word ==> vs[0][12];
  xor2.inp_word <== init[13]; xor2.out_word ==> vs[0][13];
  xor3.inp_word <== init[14]; xor3.out_word ==> vs[0][14];
  vs[0][15] <== init[15];

  component rounds[10];

  for(var i=0; i<10; i++) {
    rounds[i] = SingleRound(i);
    rounds[i].msg <== m;
    rounds[i].inp <== vs[i];
    rounds[i].out ==> vs[i+1];
  }

  component fin[8];
  for(var i=0; i<8; i++) {
    fin[i] = XorWord3(32);
    fin[i].x        <== h[i];
    fin[i].y        <== vs[10][i];
    fin[i].z        <== vs[10][i+8];
    fin[i].out_word ==> out[i];
  }

//  for(var i=0; i<8; i++) {
//    log("compress_out[", i, "] = ", out[i]);
//  }

}

//------------------------------------------------------------------------------
// hash a sequence of `ll` bytes

template Blake2s_bytes(ll) {
  signal input  inp_bytes[ll];
  signal output hash_words[8];
  signal output hash_bytes[32];
  signal output hash_bits[256];

  var kk = 0;                 // key size in bytes
  var nn = 32;                // final hash size in bytes
  var dd = (ll + 63) \ 64;    // number of message blocks

  signal blocks[dd][16];      // message blocks

  var p0 = 0x01010000 ^ (kk << 8) ^ nn;

  signal hs[dd+1][8];

  component iv = IV();
  hs[0][0] <== (0x6A09E667 ^ p0);
  for(var i=1; i<8; i++) { hs[0][i] <== iv.out[i]; }

  component compr[dd];

  for(var k=0; k<dd; k++) {

    var f = (k == dd-1);                  // is it the final block?
    var t = (f) ? (ll) : ((k+1)*64);      // offset counter
    compr[k] = CompressionF( t , f );

    for(var j=0; j<16; j++) { 
      var acc = 0;
      for(var q=0; q<4; q++) {
        var u = k*64 + j*4 + q;
        if (u < ll) { acc += inp_bytes[u] * (256**q); }
      }
      blocks[k][j] <== acc;
    }

    compr[k].h   <== hs[k];
    compr[k].m   <== blocks[k];
    compr[k].out ==> hs[k+1];
  }

  hs[dd] ==> hash_words;

  component tbs[8];
  for(var j=0; j<8; j++) {
    tbs[j] = ToBits(32);
    tbs[j].inp <== hash_words[j];
    for(var i=0; i<32; i++) {
      tbs[j].out[i] ==> hash_bits[j*32+i];
    }    
  }

  for(var j=0; j<32; j++) {
    var acc = 0;
    for(var i=0; i<8; i++) { acc += hash_bits[j*8+i] * (2**i); }
    hash_bytes[j] <== acc;
  }

//  for(var i=0; i<32; i++) {
//    log("hash[",i,"] = ",hash_bytes[i]);
//  }

}

//------------------------------------------------------------------------------

