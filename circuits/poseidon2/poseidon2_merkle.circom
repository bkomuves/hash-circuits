pragma circom 2.0.0;

include "poseidon2_perm.circom";

//------------------------------------------------------------------------------
// Merkle tree built using the Poseidon2 permutation
//
// The number of leaves is `2**nlevels`
//

template PoseidonMerkle(nlevels) {

  var nleaves = 2**nlevels;

  signal input  inp[nleaves];
  signal output out_root;

  component hsh[  nleaves-1];
  signal    aux[2*nleaves-1];

  for(var k=0; k<nleaves; k++) { aux[k] <== inp[k]; }

  var a = 0;      // aux[a..b) = input layer
  var u = 0;      // hsh[u..v) = hash compression components

  for(var lev=0; lev<nlevels; lev++) {

    var b = a + 2**(nlevels-lev  );
    var v = u + 2**(nlevels-lev-1);

    var ncherries = 2**(nlevels-lev-1);
    for(var k=0; k<ncherries; k++) {
      hsh[u+k] = Compression();
      hsh[u+k].inp[0] <== aux[a+2*k  ];
      hsh[u+k].inp[1] <== aux[a+2*k+1];
      hsh[u+k].out    ==> aux[b+  k  ];
    }

    a = b;
    u = v;
  }

  aux[2*nleaves-2] ==> out_root;
}

//------------------------------------------------------------------------------
