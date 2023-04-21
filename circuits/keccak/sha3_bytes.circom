pragma circom 2.0.0;

include "sha3_bits.circom";

//------------------------------------------------------------------------------

template UnpackBytes(n) {
  signal input  bytes[n];
  signal output bits[8*n];

  component tobits[n];

  for(var j=0; j<n; j++) {
    tobits[j] = ToBits(8);
    tobits[j].inp <== bytes[j];
    for(var i=0; i<8; i++) {
      tobits[j].out[i] ==> bits[ j*8 + i ];
    }
  }
}

//--------------------------------------

template PackBytes(n) {
  signal input  bits[8*n];
  signal output bytes[n];

  for(var k=0; k<n; k++) {
    var sum = 0;
    for(var i=0; i<8; i++) {
      sum += bits[ 8*k + i ] * (1<<i);
    }
    bytes[k] <== sum;
  }
}
  
//------------------------------------------------------------------------------
// NIST SHA3 hash functions

template SHA3_224_bytes(input_len) {
  signal input  inp_bytes[input_len];
  signal output out_bytes[28];

  component unpack = UnpackBytes(input_len);
  component sha3   = SHA3_224(input_len*8);
  component pack   = PackBytes(28);

  inp_bytes   ==> unpack.bytes;
  unpack.bits ==> sha3.inp;
  sha3.out    ==> pack.bits;
  pack.bytes  ==> out_bytes;
}

//--------------------------------------

template SHA3_256_bytes(input_len) {
  signal input  inp_bytes[input_len];
  signal output out_bytes[32];

  component unpack = UnpackBytes(input_len);
  component sha3   = SHA3_256(input_len*8);
  component pack   = PackBytes(32);

  inp_bytes   ==> unpack.bytes;
  unpack.bits ==> sha3.inp;
  sha3.out    ==> pack.bits;
  pack.bytes  ==> out_bytes;
}

//--------------------------------------

template SHA3_384_bytes(input_len) {
  signal input  inp_bytes[input_len];
  signal output out_bytes[48];

  component unpack = UnpackBytes(input_len);
  component sha3   = SHA3_384(input_len*8);
  component pack   = PackBytes(48);

  inp_bytes   ==> unpack.bytes;
  unpack.bits ==> sha3.inp;
  sha3.out    ==> pack.bits;
  pack.bytes  ==> out_bytes;
}

//--------------------------------------

template SHA3_512_bytes(input_len) {
  signal input  inp_bytes[input_len];
  signal output out_bytes[64];

  component unpack = UnpackBytes(input_len);
  component sha3   = SHA3_512(input_len*8);
  component pack   = PackBytes(64);

  inp_bytes   ==> unpack.bytes;
  unpack.bits ==> sha3.inp;
  sha3.out    ==> pack.bits;
  pack.bytes  ==> out_bytes;
}

//------------------------------------------------------------------------------
// NIST SHA3 Extendable-Output Functions

template SHAKE128_bytes(input_len, output_len) {
  signal input  inp_bytes[input_len];
  signal output out_bytes[output_len];

  component unpack = UnpackBytes(input_len);
  component sha3   = SHAKE128(input_len*8, output_len*8);
  component pack   = PackBytes(output_len);

  inp_bytes   ==> unpack.bytes;
  unpack.bits ==> sha3.inp;
  sha3.out    ==> pack.bits;
  pack.bytes  ==> out_bytes;
}

//--------------------------------------

template SHAKE256_bytes(input_len, output_len) {
  signal input  inp_bytes[input_len];
  signal output out_bytes[output_len];

  component unpack = UnpackBytes(input_len);
  component sha3   = SHAKE256(input_len*8, output_len*8);
  component pack   = PackBytes(output_len);

  inp_bytes   ==> unpack.bytes;
  unpack.bits ==> sha3.inp;
  sha3.out    ==> pack.bits;
  pack.bytes  ==> out_bytes;
}

//------------------------------------------------------------------------------

