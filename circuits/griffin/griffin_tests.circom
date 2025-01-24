
// these are only wrappers to be compatible with right now very hackish
// and limited test framework

pragma circom 2.0.0;

include "griffin_perm.circom";

//------------------------------------------------------------------------------

template Test_Griffin_permutation(dummy) {

  signal input  inp[3];
  signal output out;

  signal tmp[3];

  component perm = Permutation();
  perm.inp <== inp;
  perm.out ==> tmp;

  out <== tmp[0] + tmp[1] + tmp[2];
}

//------------------------------------------------------------------------------

template Test_Griffin_compression(dummy) {

  signal input  inp[2];
  signal output out;

  component comp = Compression();
  comp.inp <== inp;
  comp.out ==> out;
}

//------------------------------------------------------------------------------

template Test_Griffin_iterated_permutation(dummy) {

  signal input  inp[3];
  signal output out;

  signal aux[101][3];

  component perm[100];

  aux[0] <== inp;
  for(var i=0; i<100; i++) {
    perm[i] = Permutation();
    perm[i].inp <== aux[i];
    perm[i].out ==> aux[i+1];
  }

  signal tmp[3];
  tmp <== aux[100];

  out <== tmp[0] + tmp[1] + tmp[2];
}

//------------------------------------------------------------------------------
