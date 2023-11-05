pragma circom 2.0.0;

include "mimc-roundconst.circom";
include "mimc-roundfun.circom";

//------------------------------------------------------------------------------

//
// MiMC-Feistel-2p/p block cipher encryption
//

template MiMC_Feistel_2p$p_encrypt_block() {
  signal input  key;
  signal input  inp[2];
  signal output out[2];

  signal rc[220];
  component RC = MiMC_round_consts(220);
  rc <== RC.out;

  signal auxL[221];
  signal auxR[221];
  auxL[0] <== inp[0];
  auxR[0] <== inp[1];

  component pow[220];
  for(var i=0; i<220; i++) {
    pow[i] = pow5();
    pow[i].inp <== auxL[i] + key + rc[i];
    auxL[i+1]  <== pow[i].out + auxR[i];
    auxR[i+1]  <== auxL[i];
  }

  out[0] <== auxL[220] + key;
  out[1] <== auxR[220];
}


//--------------------------------------

//
// MiMC-Feistel-2p/p block cipher decryption
//

template MiMC_Feistel_2p$p_decrypt_block() {
  signal input  key;
  signal input  inp[2];
  signal output out[2];

  signal rc[220];
  component RC = MiMC_round_consts(220);
  rc <== RC.out;

  signal auxL[221];
  signal auxR[221];
  auxL[220] <== inp[0] - key;
  auxR[220] <== inp[1];

  component pow[220];
  for(var i=219; i>=0; i--) {
    pow[i] = pow5();
    auxL[i]    <== auxR[i+1];
    pow[i].inp <== auxL[i] + key + rc[i];
    auxR[i]    <== auxL[i+1] - pow[i].out;
  }

  out[0] <== auxL[0];
  out[1] <== auxR[0];
}

//------------------------------------------------------------------------------

//
// This is a permutation of F^2 where F is the scalar field.
//

template MiMC_Feistel_2p$p_permutation() {
  signal input  inp[2];
  signal output out[2];

  component encrypt = MiMC_Feistel_2p$p_encrypt_block();
  encrypt.key <== 0;
  encrypt.inp <== inp;
  encrypt.out ==> out;
}

//--------------------------------------

//
// The inverse permutation. 
//

template MiMC_Feistel_2p$p_inverse_permutation() {
  signal input  inp[2];
  signal output out[2];

  component decrypt = MiMC_Feistel_2p$p_decrypt_block();
  decrypt.key <== 0;
  decrypt.inp <== inp;
  decrypt.out ==> out;
}

//------------------------------------------------------------------------------

//
// Hash using the sponge construction, with rate=capacity=1
//
// It should in theory provide approx 127 bit preimage and collision security
//

template MiMC_Feistel_2p$p_hash_sponge(n) {

  signal input  inp[n];
  signal output out;  

  signal state[n+1][2];

  state[0][0] <== 0;        // we use the length to initialize the capacity
  state[0][1] <== n;        // as an extra safety measure (prolly unnecessary as we have rate=1)

  component perm[n];

  for(var i=0; i<n; i++) {
    perm[i] = MiMC_Feistel_2p$p_permutation();
    perm[i].inp[0] <== state[i][0] + inp[i];
    perm[i].inp[1] <== state[i][1];
    perm[i].out    ==> state[i+1];
  }

  out <== state[n][0];

}

//------------------------------------------------------------------------------
