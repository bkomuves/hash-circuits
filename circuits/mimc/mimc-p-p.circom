pragma circom 2.0.0;

include "mimc-roundconst.circom";
include "mimc-roundfun.circom";

//------------------------------------------------------------------------------

//
// MiMC-p/p block cipher encryption
//

template MiMC_p$p_encrypt_block() {
  signal input  key;
  signal input  inp;
  signal output out;

  signal rc[110];
  component RC = MiMC_round_consts(110);
  rc <== RC.out;

  signal aux[111];
  aux[0] <== inp;
  component pow[110];
  for(var i=0; i<110; i++) {
    pow[i] = pow5();
    pow[i].inp <== aux[i] + key + rc[i];
    pow[i].out ==> aux[i+1];
  }

  out <== aux[110] + key;
}

//--------------------------------------

//
// MiMC-p/p block cipher decryption
//

template MiMC_p$p_decrypt_block() {
  signal input  key;
  signal input  inp;
  signal output out;

  signal rc[110];
  component RC = MiMC_round_consts(110);
  rc <== RC.out;

  signal aux[111];
  aux[110] <== inp - key;
  component inv[110];
  for(var i=109; i>=0; i--) {
    inv[i] = inv_pow5();
    inv[i].inp <== aux[i+1];
    aux[i]     <== inv[i].out - key - rc[i];
  }

  out <== aux[0];
}

//------------------------------------------------------------------------------

//
// This is a permutation of the scalar field.
//

template MiMC_p$p_permutation() {
  signal input  inp;
  signal output out;

  component encrypt = MiMC_p$p_encrypt_block();
  encrypt.key <== 0;
  encrypt.inp <== inp;
  encrypt.out ==> out;
}

//--------------------------------------

//
// The inverse permutation. 
//
// Note: this is _much slower_ to compute (though not much slower to prove)
//       than the forward permutations!
//

template MiMC_p$p_inverse_permutation() {
  signal input  inp;
  signal output out;

  component decrypt = MiMC_p$p_decrypt_block();
  decrypt.key <== 0;
  decrypt.inp <== inp;
  decrypt.out ==> out;
}

//------------------------------------------------------------------------------

// 
// `F^2 -> F` compression function using Davies-Meyer
// 

template MiMC_p$p_compression() {
  signal input  inp[2];
  signal output out;

  component encrypt = MiMC_p$p_encrypt_block();
  encrypt.key <== inp[0];
  encrypt.inp <== inp[1];
  out         <== encrypt.out + inp[1];   

  // NOTE: The `+inp[1]` is called the "Davies-Meyer construction", and 
  // apparently is very important. We could also argue that secretly this
  // is the "Miyaguchi–Preneel construction", as the `inp[0]=key` is already
  // added in the encrypt function.
}

//------------------------------------------------------------------------------

//
// Merkel-Damgard hash built from the compression function above.
//
// NOTE: Consider using the 2p/p sponge hash instead of this one; 
//       that is supposed to be a safer choice (?)
//

template MiMC_p$p_hash_MerkleDamgard(n) {
  signal input  inp[n];
  signal output out;

  signal aux[n+1];
  aux[0] <== n;         // we use the length as IV, so different lengths won't collide

  component compr[n];
  for(var i=0; i<n; i++) {
    compr[i] = MiMC_p$p_compression();
    compr[i].inp[0] <== aux[i];
    compr[i].inp[1] <== inp[i];
    compr[i].out    ==> aux[i+1];
  }

  out <== aux[n]; 
}

//------------------------------------------------------------------------------

