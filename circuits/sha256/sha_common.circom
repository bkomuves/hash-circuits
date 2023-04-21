pragma circom 2.0.0;

//------------------------------------------------------------------------------
// index rotation function for [0..31] bit indices
//
// note: we lookup the "rotated index" in the original bit vectors, 
// so the direction is reversed!

// index lookup for left rotation
function rotIdxL(i,by) {
  return  (i >= by) ? (i - by) : (i - by + 32);
}

// index lookup for right rotation
function rotIdxR(i,by) {
  return  (i + by < 32) ? (i + by) : (i + by - 32);
}

//------------------------------------------------------------------------------
// decompose a 2-bit number into a high and a low bit

template Bits2() {
  signal input  xy;
  signal output lo;
  signal output hi;

  lo <--  xy     & 1;
  hi <-- (xy>>1) & 1;

  lo*(1-lo) === 0;
  hi*(1-hi) === 0;

  xy === 2*hi + lo;
}

//------------------------------------------------------------------------------
// XOR 3 bits together

template XOR3_v1() {
  signal input  x;
  signal input  y;
  signal input  z;
  signal output out;

  component bs = Bits2();
  bs.xy <== x + y + z;
  bs.lo ==> out;
}

template XOR3_v2() {
  signal input  x;
  signal input  y;
  signal input  z;
  signal output out;

  signal tmp <== y*z;
  out <== x * (1 - 2*y - 2*z + 4*tmp) + y + z - 2*tmp;
}

template XOR23_v2( triple_flag ) {
  signal input  x;
  signal input  y;
  signal input  z;
  signal output out;

  signal tmp;

  if (triple_flag) {
    tmp <== y*z;
    out <== x * (1 - 2*y - 2*z + 4*tmp) + y + z - 2*tmp;
  }
  else {
    // we assume z=0
    out <== x + y - 2*x*y;
  }
}

//------------------------------------------------------------------------------
// decompose an n-bit number into bits

template ToBits(n) {
  signal input  inp;
  signal output out[n];

  var sum = 0;
  for(var i=0; i<n; i++) {
    out[i] <-- (inp >> i) & 1;
    out[i] * (1-out[i]) === 0;
    sum += (1<<i) * out[i];
  }

  inp === sum;
}

//------------------------------------------------------------------------------
// decompose a 33-bit number into the low 32 bits and the remaining 1 bit

template Bits33() {
  signal input  inp;
  signal output out[32];
  signal u;

  var sum = 0;
  for(var i=0; i<32; i++) {
    out[i] <-- (inp >> i) & 1;
    out[i] * (1-out[i]) === 0;
    sum += (1<<i) * out[i];
  }

  u <-- (inp >> 32) & 1;
  u*(1-u) === 0;
  sum += (1<<32) * u;

  inp === sum;
}

//--------------------------------------
// modulo 2^32 of a 33-bit number

template Mod33() {
  signal input  inp;
  signal output out;
  signal output bit[32];

  signal u;

  var sum = 0;
  for(var i=0; i<32; i++) {
    bit[i] <-- (inp >> i) & 1;
    bit[i] * (1-bit[i]) === 0;
    sum += (1<<i) * bit[i];
  }

  u <-- (inp >> 32) & 1;
  u*(1-u) === 0;

  inp === sum + (1<<32) * u;
  out <== sum;
}

//------------------------------------------------------------------------------
// modulo 2^32 of a 34-bit number

template Mod34() {
  signal input  inp;
  signal output out;
  signal output bit[32];

  signal u;
  signal v;

  var sum = 0;
  for(var i=0; i<32; i++) {
    bit[i] <-- (inp >> i) & 1;
    bit[i] * (1-bit[i]) === 0;
    sum += (1<<i) * bit[i];
  }

  u <-- (inp >> 32) & 1;
  v <-- (inp >> 33) & 1;

  u*(1-u) === 0;
  v*(1-v) === 0;

  inp === sum + (1<<32)*u + (1<<33)*v;
  out <== sum;
}

//------------------------------------------------------------------------------
// decompose a 35-bit number into the low 32 bits and the remaining 3 bits

template Bits35() {
  signal input  inp;
  signal output out[32];
  signal u,v,w;

  var sum = 0;
  for(var i=0; i<32; i++) {
    out[i] <-- (inp >> i) & 1;
    out[i] * (1-out[i]) === 0;
    sum += (1<<i) * out[i];
  }

  u <-- (inp >> 32) & 1;
  v <-- (inp >> 33) & 1;
  w <-- (inp >> 34) & 1;

  u*(1-u) === 0;
  v*(1-v) === 0;
  w*(1-w) === 0;

  sum += (1<<32) * u;
  sum += (1<<33) * v; 
  sum += (1<<34) * w;

  inp === sum;
}

//------------------------------------------------------------------------------
