
// stream ciphers based on the MiMC-p/p block cipher

pragma circom 2.0.0;

include "mimc-p-p.circom";

//------------------------------------------------------------------------------

//
// encrypt a sequence of `n` field elements using CFB (Cipher feedback) mode
//

template MiMC_p$p_CFB_encrypt(n) {
  signal input  key;     // encryption key
  signal input  iv;      // initialization vector (should be random and non-repeating)
  signal input  inp[n];
  signal output out[n];

  component enc[n];
  for(var i=0; i<n; i++) {
    enc[i] = MiMC_p$p_encrypt_block();
    if (i==0) { enc[i].inp <== iv; } else { enc[i].inp <== out[i-1]; }
    key    ==> enc[i].key;
    out[i] <== enc[i].out + inp[i];
  }
}

//--------------------------------------

//
// decrypt a sequence of `n` field elements encrypted using CFB (Cipher feedback) mode
//

template MiMC_p$p_CFB_decrypt(n) {  
  signal input  key;     // encryption key
  signal input  iv;      // initialization vector (should be random and non-repeating)
  signal input  inp[n];
  signal output out[n];

  component enc[n];
  for(var i=0; i<n; i++) {
    enc[i] = MiMC_p$p_encrypt_block();
    if (i==0) { enc[i].inp <== iv; } else { enc[i].inp <== inp[i-1]; }
    key    ==> enc[i].key;
    out[i] <== inp[i] - enc[i].out;
  }
}

//------------------------------------------------------------------------------
