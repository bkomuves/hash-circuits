
pragma circom 2.0.0;

//------------------------------------------------------------------------------

function Sigma(i0) {

  var out[16];

  var sigma[160] =
    [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 
    , 14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3 
    , 11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4 
    , 7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8 
    , 9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13 
    , 2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9 
    , 12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11 
    , 13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10 
    , 6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5 
    , 10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0 
    ];

  var i = i0 % 10;

  for(var j=0; j<16; j++) { out[j] = sigma[i*16+j]; }

  return out;
}

//------------------------------------------------------------------------------
// XOR 3 bits together

template XOR3() {
  signal input  x;
  signal input  y;
  signal input  z;
  signal output out;

  signal tmp <== y*z;
  out <== x * (1 - 2*y - 2*z + 4*tmp) + y + z - 2*tmp;
}

//------------------------------------------------------------------------------
// XOR 3 words together

template XorWord3(n) {
  signal input  x;
  signal input  y;
  signal input  z;
  signal output out_bits[n];
  signal output out_word;

  component tb_x = ToBits(n); 
  component tb_y = ToBits(n);
  component tb_z = ToBits(n);

  tb_x.inp <== x;
  tb_y.inp <== y;  
  tb_z.inp <== z;

  component xor[n];

  var acc = 0;
  for(var i=0; i<n; i++) { 
    xor[i] = XOR3();
    xor[i].x   <== tb_x.out[i];
    xor[i].y   <== tb_y.out[i];
    xor[i].z   <== tb_z.out[i];
    xor[i].out ==> out_bits[i];
    acc += out_bits[i] * (2**i);
  }

  out_word <== acc;
}

//------------------------------------------------------------------------------
// XOR a word with a constant

template XorWordConst(n, kst_word) {
  signal input  inp_word;
  signal output out_bits[n];
  signal output out_word;

  component tb = ToBits(n);
  tb.inp <== inp_word;

  var acc = 0;
  for(var i=0; i<n; i++) {
    var x = tb.out[i];
    var y = (kst_word >> i) & 1;
    out_bits[i] <== x + y - 2*x*y;
    acc += out_bits[i] * (2**i);
  }

  out_word <== acc;  
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
  signal output out_bits[32];
  signal output out_word;
  signal u;

  var sum = 0;
  for(var i=0; i<32; i++) {
    out_bits[i] <-- (inp >> i) & 1;
    out_bits[i] * (1-out_bits[i]) === 0;
    sum += (1<<i) * out_bits[i];
  }

  u <-- (inp >> 32) & 1;
  u*(1-u) === 0;

  inp === sum + (1<<32)*u;
  out_word <== sum;
}

//------------------------------------------------------------------------------
// decompose a 34-bit number into the low 32 bits and the remaining 2 bits

template Bits34() {
  signal input  inp;
  signal output out_bits[32];
  signal output out_word;
  signal u,v;

  var sum = 0;
  for(var i=0; i<32; i++) {
    out_bits[i] <-- (inp >> i) & 1;
    out_bits[i] * (1-out_bits[i]) === 0;
    sum += (1<<i) * out_bits[i];
  }

  u <-- (inp >> 32) & 1;
  v <-- (inp >> 33) & 1;
  u*(1-u) === 0;
  v*(1-v) === 0;

  inp === sum + (1<<32)*u + (1<<33)*v;
  out_word <== sum;
}

//------------------------------------------------------------------------------
// decompose a 65-bit number into the low 64 bits and the remaining 1 bit

template Bits65() {
  signal input  inp;
  signal output out_bits[64];
  signal output out_word;
  signal u;

  var sum = 0;
  for(var i=0; i<64; i++) {
    out_bits[i] <-- (inp >> i) & 1;
    out_bits[i] * (1-out_bits[i]) === 0;
    sum += (1<<i) * out_bits[i];
  }

  u <-- (inp >> 64) & 1;
  u*(1-u) === 0;

  inp === sum + (1<<64)*u;
  out_word <== sum;
}

//------------------------------------------------------------------------------
// decompose a 66-bit number into the low 64 bits and the remaining 2 bit

template Bits66() {
  signal input  inp;
  signal output out_bits[64];
  signal output out_word;
  signal u,v;

  var sum = 0;
  for(var i=0; i<64; i++) {
    out_bits[i] <-- (inp >> i) & 1;
    out_bits[i] * (1-out_bits[i]) === 0;
    sum += (1<<i) * out_bits[i];
  }

  u <-- (inp >> 64) & 1;
  v <-- (inp >> 65) & 1;
  u*(1-u) === 0;
  v*(1-v) === 0;

  inp === sum + (1<<64)*u + (1<<65)*v;
  out_word <== sum;
}

//------------------------------------------------------------------------------