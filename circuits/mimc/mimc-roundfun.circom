pragma circom 2.0.0;

include "mimc-roundconst.circom";
include "mimc-roundfun.circom";

//------------------------------------------------------------------------------

//
// the function `x -> x^5`
//
template pow5() {
  signal input  inp;
  signal output out;

  signal tmp2, tmp4;
  tmp2 <== inp  * inp ;      // x^2 
  tmp4 <== tmp2 * tmp2;      // x^4
  out  <== tmp4 * inp ;      // x^5
}

//--------------------------------------

//
// inverse of `x -> x^5`, naive (large proof) implementation
//
template inv_pow5_naive() {
  signal input  inp;
  signal output out;

  // (a^5)^invExpo === a (mod prime)
  var invExpo = 17510594297471420177797124596205820070838691520332827474958563349260646796493;

  signal pow[254];    // x,x^2,x^4,..x^(2^253)
  signal aux[255];

  aux[0] <== 1;
  for(var i=0; i<254; i++) {
    if (i==0) { pow[i] <== inp; } else { pow[i] <== pow[i-1] * pow[i-1]; } 
    if ((invExpo>>i)&1) {
      aux[i+1] <== aux[i] * pow[i];
    }
    else {
      aux[i+1] <== aux[i];
    }    
  }

  out <== aux[254];
}

//--------------------------------------

function func_inv_pow5(x) {
  // (a^5)^invExpo === a (mod prime)
  var invExpo = 17510594297471420177797124596205820070838691520332827474958563349260646796493;
  var pow = x;
  var aux = 1;
  for(var i=0; i<254; i++) {
    if ((invExpo>>i)&1) { aux = aux * pow; }
    pow = pow*pow;
  }
  return aux;  
}

//--------------------------------------

template inv_pow5() {
  signal input  inp;
  signal output out;

  out <-- func_inv_pow5(inp);     // first we guess the result = inp^invExpo

  component pow = pow5();          
  pow.inp <== out;             
  pow.out === inp;                // then we check that result^5 == inp

  // since `x -> x^5` is a permutation, this proves our guess.
}

//------------------------------------------------------------------------------

