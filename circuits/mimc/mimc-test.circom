
// simple wrappers for compatibility with the crappy hackish test framework

pragma circom 2.0.0;

include "mimc-p-p.circom";
include "mimc-feistel-2p-p.circom";

//------------------------------------------------------------------------------

template Test_MiMC_permutation(dummy) {
  signal input  inp[1];
  signal output out;
  component perm = MiMC_p$p_permutation();
  perm.inp <== inp[0];
  perm.out ==> out;
}

template Test_MiMC_inverse_permutation(dummy) {
  signal input  inp[1];
  signal output out;
  component perm = MiMC_p$p_inverse_permutation();
  perm.inp <== inp[0];
  perm.out ==> out;
}

//------------------------------------------------------------------------------

template Test_MiMC_Feistel_permutation(dummy) {
  signal input  inp[2];
  signal output out;
  component perm = MiMC_Feistel_2p$p_permutation();
  inp ==> perm.inp;
  out <== perm.out[0] + perm.out[1];
}

template Test_MiMC_Feistel_inverse_permutation(dummy) {
  signal input  inp[2];
  signal output out;
  component perm = MiMC_Feistel_2p$p_inverse_permutation();
  inp ==> perm.inp;
  out <== perm.out[0] + perm.out[1];
}

//------------------------------------------------------------------------------

template Test_MiMC_compression(dummy) {
  signal input  inp[2];
  signal output out;
  component comp = MiMC_p$p_compression();
  comp.inp <== inp;
  comp.out ==> out;
}

//------------------------------------------------------------------------------
  
template Test_MiMC_encrypt(dummy) {
  signal input  inp[2];
  signal output out;
  component enc = MiMC_p$p_encrypt_block();
  enc.key <== inp[0];
  enc.inp <== inp[1];
  enc.out ==> out; 
}

template Test_MiMC_decrypt(dummy) {
  signal input  inp[2];
  signal output out;
  component dec = MiMC_p$p_decrypt_block();
  dec.key <== inp[0];
  dec.inp <== inp[1];
  dec.out ==> out; 
}

//------------------------------------------------------------------------------
  
template Test_MiMC_Feistel_encrypt(dummy) {
  signal input  inp[3];
  signal output out;
  component enc = MiMC_Feistel_2p$p_encrypt_block();
  inp[0] ==> enc.key;
  inp[1] ==> enc.inp[0];
  inp[2] ==> enc.inp[1];
  out    <== enc.out[0] + enc.out[1];
}

template Test_MiMC_Feistel_decrypt(dummy) {
  signal input  inp[3];
  signal output out;
  component dec = MiMC_Feistel_2p$p_decrypt_block();
  inp[0] ==> dec.key;
  inp[1] ==> dec.inp[0];
  inp[2] ==> dec.inp[1];
  out    <== dec.out[0] + dec.out[1];
}

//------------------------------------------------------------------------------
