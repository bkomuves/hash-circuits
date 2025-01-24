pragma circom 2.0.0;

//
// The Griffin permutation for bn128 and t=3 
//

//------------------------------------------------------------------------------
// The S-box

template Pow5() {
  signal input  inp;
  signal output out;

  signal x2 <== inp*inp;
  signal x4 <== x2*x2;

  out <== inp*x4;
}

template PowInv5() {
  signal input  inp;
  signal output out;

  var d_inv = 0x26b6a528b427b35493736af8679aad17535cb9d394945a0dcfe7f7a98ccccccd;
  out <-- (inp ** d_inv);

  // log("x = input   = ",inp);
  // log("y = x^d_inv = ",out);
  // log("z = y^5     = ",out ** 5);

  component pow5 = Pow5();
  pow5.inp <== out;
  pow5.out === inp;          // note: x -> x^5 is a permutation, so this is enough
}

template Horst() {
  signal input  inp;
  signal input  y0;
  signal input  y1;
  signal output out;

  var alpha = 0x146ecffb34a66316fae66609f78d1310bc14ad7208082ca7943afebb1da4aa4a;
  var beta  = 0x2b568115d544c7e941eff6ccc935384619b0fb7d2c5ba6c078c34cf81697ee1c;

  signal u  <== y0 + y1;
  signal u2 <== u*u;

  signal mult <== u2 + alpha*u + beta;
  out <== inp * mult;
}

template SBox() {
  signal input  inp[3];
  signal output out[3];

  component nlin0 = PowInv5();
  component nlin1 = Pow5();
  component nlin2 = Horst();

  nlin0.inp <== inp[0];
  nlin0.out ==> out[0];

  nlin1.inp <== inp[1];
  nlin1.out ==> out[1];

  nlin2.inp <== inp[2];
  nlin2.y0  <== out[0];
  nlin2.y1  <== out[1];
  nlin2.out ==> out[2];
}

//------------------------------------------------------------------------------
// the linear stuff

template AddRC(i) {

  signal input  inp[3];
  signal output out[3];

  var round_consts[36] =
    [ 0
    , 0
    , 0 
    , 0x2fb30cafdb1f76156dfabf0cd0af4b895e764ac2a84386c9d0d7aed6a7f4eac9
    , 0x282927892ce324572f19abb14871d2b539a80d8a5800cdb87a81e1697a94b6c9
    , 0x03d0f3f2711dd59e3d97fc797261300cd3fee33b95cf710a32edf42aa2bc0905
    , 0x036a8b3eb9ef35c74ea5a367ed279ee6d043d4ff69817f192c7251b91dcbb03d
    , 0x2a626d396e7fa8ce8d6339bb37bd48491d56db0c7ac0afb5008a7464d5776a26
    , 0x0cc9dfabbeaef7982543453ea3ac37ef2bfefd35a7e7070aa39b021035852d5b
    , 0x2a1951149e2568ab28e972a2ceddc49eff0cae8e1cddcf4b0684a73a1b4ef61b
    , 0x2d0ff8e9158b2fd7ae3afe01cf09d4ce9ff81e6127e441eb6cbc79d21f22be9e
    , 0x1cc315b7ea0c1efb538f0c3248a7da062309a9e41af5a555c9ea9e8a10930cb5
    , 0x03cb10093ea62fb3f6e5680a128d07112ee566f1b424558f2ec9d86892e13a80
    , 0x12e7bb50ae7e9e90f1765c073eb61c4be4956c424930233ce497d2722a458868
    , 0x006b1367547937ae71e2e9b55d2f90c90131f9e6784ce3de0eb314ec748871e7
    , 0x1ffff572c53442c58809aeca02287839b11df1420deb0e99fde2baad8b86fa9c
    , 0x13aefd685e7739f9a8b4ccdbfc5ef9e566149af4d54d6b746058ea44cb422840
    , 0x1ea6c3ea93fe6f4ed0186941650de76ff94ab0e6e8a583996b67ba026dd2b7a5
    , 0x288f120288f9225643de833c5c15e22aadd358132bbdc12c75109048a158c9f4
    , 0x0f638114cd7c781ab299e5233338b00cf2996df962347a00146a22103d9ad91a
    , 0x14eeca5fa2c18999ea25ddf44237d6ac3cb8757ea452f67e2590a46f7d5b1e4f
    , 0x102d1a099e8cd107dc056e72370e340b0316d237b72d99ef6261761f7eb2d61c
    , 0x0ef741fc2fcda50f207c759dbd844a4d630cc0e4062ca80f3ffba2cce2d3f51d
    , 0x0989b9f642485692a1f91a4b207db64f38ae545bf3e0622f3862967d27f563db
    , 0x1eb4d812c80ce04784a80c89fbcc5aab89db274c62602bdd30f3223655e6cf8a
    , 0x0124a9400253731facd46e21f41016aed69a79087f81665bc5d29a34e4e924dd
    , 0x2520bfa6b70e6ba7ad380aaf9015b71983868a9c53e66e685ed6e48692c185a8
    , 0x1bd62b5bfa02667ac08d51d9e77bb3ab8dbd19e7a701442a20e23f7d3d6b28b4
    , 0x1ae2f0d09fffc6bb869ebc639484a7c2084cfa3c1f88a7440713b1b154e5f952
    , 0x0cd06e16a0d570c3799d800d92a25efbd44a795ed5b9114a28f5f869a57d9ba1
    , 0x00691740e313922521fe8c4843355eff8de0f93d4f62df0fe48755b897881c39
    , 0x19903aa449fe9c27ee9c8320e6915b50c2822e61ce894be72b47a449c5705762
    , 0x126e801aae44016a35deceaa3eba6ccc341fa3c2a65ab3d021fcd39abd170e1b
    , 0x1b0a98be27b54ac9d5d72b94187c991c1872cb2c7777c0e880f439c133971e8d
    , 0x1e10a35afda2e5a173d4f3edecf29dacf51d8fac33d6bfb4088cc787ec647605
    , 0x1793cda85abe2782ea8e911ce92bab59a8c68e0dd561a57b064bb233f109cc57
    ];

  out[0] <== inp[0] + round_consts[3*i+0];
  out[1] <== inp[1] + round_consts[3*i+1];
  out[2] <== inp[2] + round_consts[3*i+2];
}

template LinearLayer() {
  signal input  inp[3];
  signal output out[3];
  out[0] <== 2*inp[0] +   inp[1] +   inp[2];
  out[1] <==   inp[0] + 2*inp[1] +   inp[2];
  out[2] <==   inp[0] +   inp[1] + 2*inp[2]; 
}

//------------------------------------------------------------------------------
// full round function

template RoundFun(i) {
  signal input  inp[3];
  signal output out[3];

  signal aux1[3];
  signal auz2[3];

  component rc   = AddRC(i);
  component sbox = SBox();
  component mds  = LinearLayer();

  rc.inp   <== inp;
  rc.out   ==> sbox.inp;
  sbox.out ==> mds.inp;
  mds.out  ==> out;
}

//------------------------------------------------------------------------------

// the Griffin permutation
template Permutation() {
  signal input  inp[3];
  signal output out[3];

  signal aux[13][3];

  component lin = LinearLayer();
  lin.inp <== inp;
  lin.out ==> aux[0];

  component rfun[12];
  for(var i=0; i<12; i++) {
    rfun[i] = RoundFun(i);
    rfun[i].inp <== aux[i];
    rfun[i].out ==> aux[i+1];
  }

  out <== aux[12];
}

//------------------------------------------------------------------------------

// the "compression function" takes 2 field elements as input and produces
// 1 field element as output. It is a trivial application of the permutation.
template Compression() {
  signal input  inp[2];
  signal output out;

  component perm = Permutation();
  perm.inp[0] <== inp[0];
  perm.inp[1] <== inp[1];
  perm.inp[2] <== 0;

  perm.out[0] ==> out;
}

//------------------------------------------------------------------------------

