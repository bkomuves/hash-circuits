
module Vectors.SHA2 where

--------------------------------------------------------------------------------

data HashFun
  = SHA2_224
  | SHA2_256
  | SHA2_384
  | SHA2_512
  deriving (Eq,Show)

--------------------------------------------------------------------------------

testVectorsSHA2 :: HashFun -> [(String,String)]
testVectorsSHA2 hashfun = case hashfun of
  SHA2_224 -> sha2_224_vectors
  SHA2_256 -> sha2_256_vectors
  SHA2_384 -> sha2_384_vectors
  SHA2_512 -> sha2_512_vectors

--------------------------------------------------------------------------------

infix 1 ~>
(~>) :: a -> b -> (a,b)
(~>) x y = (x,y)

--------------------------------------------------------------------------------

sha2_224_vectors :: [(String,String)]
sha2_224_vectors =
  [ ""          ~> "d14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f" 
  , "a"         ~> "abd37534c7d9a2efb9465de931cd7055ffdb8879563ae98078d6d6d5"
  , "foo"       ~> "0808f64e60d58979fcb676c96ec938270dea42445aeefcd3a4e6f8db"
  , "alma"      ~> "20abd0d88f5df0ae7f0f2ee747c8f08dfffcade5c633d9b2219fd87a"
  , "almakorte" ~> "047a17d7886816ad6feca7200b4a1c1bf63291415ee035789254ed46"
  ]

sha2_256_vectors :: [(String,String)]
sha2_256_vectors =
  [ ""          ~> "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
  , "a"         ~> "ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb"
  , "foo"       ~> "2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae"
  , "alma"      ~> "cf43e029efe6476e1f7f84691f89c876818610c2eaeaeb881103790a48745b82"
  , "almakorte" ~> "7bbf139acb8cd3d267b949f6744e8c5f67cfe77e23342572a4246d4ccd64631d"
  , "almakorteszilvabanan1almakorteszilvabanan2almakorteszilvabanan3almakorteszilvabanan4almakorteszilvabanan5"
                ~> "b828bb6d1554136752c989e7d35070781f89d54162c9e0f4dfdc68f68103a88d"
  , "almakorteszilvabanan1almakorteszilvabanan2almakorteszilvabanan3almakorteszilvabanan4almakorteszilvabanan5almakorteszilvabanan6almakorteszilvabanan7"
                ~> "c285283c95d1a95042742af7222a977b17f61fb7227e8f47afe519197513f213"
  , "almakorteszilvabanan1almakorteszilvabanan2almakorteszilvabanan3almakorteszilvabanan4almakorteszilvabanan5almakorteszilvabanan6almakorteszilvabanan7almakorteszilvabanan8almakorteszilvabanan9"
                ~> "69868fcb61eb4d4e7e6ee92aae5806665b5efe960af47b28345a38885d4b57e0"
  , "almakorteszilvabanan1almakorteszilvabanan2almakorteszilvabanan3almakorteszilvabanan4almakorteszilvabanan5almakorteszilvabanan6almakorteszilvabanan7almakorteszilvabanan8almakorteszilvabanan9almakorteszilvabanan10almakorteszilvabanan11"
                ~> "c55ee176cb826d63671c02c5cfe8492564251735d5e745481cae408924d450f2"
  ]

sha2_384_vectors :: [(String,String)]
sha2_384_vectors =
  [ ""          ~> "38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b"
  , "a"         ~> "54a59b9f22b0b80880d8427e548b7c23abd873486e1f035dce9cd697e85175033caa88e6d57bc35efae0b5afd3145f31"
  , "foo"       ~> "98c11ffdfdd540676b1a137cb1a22b2a70350c9a44171d6b1180c6be5cbb2ee3f79d532c8a1dd9ef2e8e08e752a3babb"
  , "alma"      ~> "24c45c20efc2795d1737ecfd41f832a0215d2bea3bb86a3c57a27a7e4f1bcaba7cde5dc2ab8c3bea48690af5197c213b"
  , "almakorte" ~> "52ff3db08d878846ef3af064b13e5ba743e29ee72d0560a024bb45a72d5b866ab28df8ea03dfc3966d5a86c5171a0997"
  ]

sha2_512_vectors :: [(String,String)]
sha2_512_vectors =
  [ ""          ~> "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"
  , "a"         ~> "1f40fc92da241694750979ee6cf582f2d5d7d28e18335de05abc54d0560e0f5302860c652bf08d560252aa5e74210546f369fbbbce8c12cfc7957b2652fe9a75"
  , "foo"       ~> "f7fbba6e0636f890e56fbbf3283e524c6fa3204ae298382d624741d0dc6638326e282c41be5e4254d8820772c5518a2c5a8c0c7f7eda19594a7eb539453e1ed7"
  , "alma"      ~> "83b05d98186648cd5576ed158c9cf2174413b86d48720c3ffbe6452bc38a6527256ff3435eb1698f24efbc880c8ea870afe314f3004c71cfdd5f0e3c00e3979f"
  , "almakorte" ~> "826c1f003bf9835d84322150ca5d92b328465550e720fa2cc1c594957a962d089dcd581bf4e009c9c138faa18546879979fba58a091d38080af151a8ba6205d5"
  ]

--------------------------------------------------------------------------------
