
// these are only wrappers to be compatible with right now very hackish
// and limited test framework

pragma circom 2.0.0;

include "poseidon2_merkle.circom";

//------------------------------------------------------------------------------

template Test_Poseidon2_permutation(dummy) {

  signal input  inp[3];
  signal output out;

  signal tmp[3];

  component perm = Permutation();
  perm.inp <== inp;
  perm.out ==> tmp;

  out <== tmp[0] + tmp[1] + tmp[2];
}

//------------------------------------------------------------------------------

template Test_Poseidon2_compression(dummy) {

  signal input  inp[2];
  signal output out;

  component comp = Compression();
  comp.inp <== inp;
  comp.out ==> out;
}

//------------------------------------------------------------------------------

// compute (compile time) the log2 of a number

function FloorLog2(n) {
  return (n==0) ? -1 : (1 + FloorLog2(n>>1));
}

function CeilLog2(n) {
  return (n==0) ? 0 : (1 + FloorLog2(n-1));
}

//------------------------------------------------------------------------------

template Test_Poseidon2_merkle_tree(n) {
  
  var log2n = CeilLog2(n);

  // for now this only works for power-of-two sized inputs 
  assert( (1<<log2n) == n );

  signal input  inp[n];
  signal output out;

  component merkle = PoseidonMerkle(log2n);
  merkle.inp      <== inp;
  merkle.out_root ==> out;

}

//------------------------------------------------------------------------------
