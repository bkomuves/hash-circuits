
module TestVectors.Keccak.Vectors where

--------------------------------------------------------------------------------

data HashFun
  = SHA3_224
  | SHA3_256
  | SHA3_384
  | SHA3_512
  | Keccak_224
  | Keccak_256
  | Keccak_384
  | Keccak_512
  | SHAKE128 Int
  | SHAKE256 Int
  deriving (Eq,Show)

--------------------------------------------------------------------------------

testVectorsKeccak :: HashFun -> [(String,String)]
testVectorsKeccak hashfun = case hashfun of
  SHA3_224       -> sha3_224_vectors
  SHA3_256       -> sha3_256_vectors
  SHA3_384       -> sha3_384_vectors
  SHA3_512       -> sha3_512_vectors
  Keccak_224     -> keccak_224_vectors
  Keccak_256     -> keccak_256_vectors
  Keccak_384     -> keccak_384_vectors
  Keccak_512     -> keccak_512_vectors
  SHAKE128 10000 -> shake_128_10000
  SHAKE256 10000 -> shake_256_10000

--------------------------------------------------------------------------------

infix 1 ~>
(~>) :: a -> b -> (a,b)

--------------------------------------------------------------------------------

sha3_224_vectors :: [(String,String)]
sha3_224_vectors =
  [ ""          ~> "6b4e03423667dbb73b6e15454f0eb1abd4597f9a1b078e3f5b5a6bc7"
  , "a"         ~> "9e86ff69557ca95f405f081269685b38e3a819b309ee942f482b6a8b"
  , "foo"       ~> "f4f6779e153c391bbd29c95e72b0708e39d9166c7cea51d1f10ef58a"
  , "alma"      ~> "dcb3555bd4ff8fbe6b00e8fe93879726a33faa31dbcaf3922fa87717"
  , "almakorte" ~> "784812d81ae43fb0340db22b641a03adc4c5f6adf0d19ef2d70db264"
  ]

sha3_256_vectors :: [(String,String)]
sha3_256_vectors =
  [ ""          ~> "a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a"
  , "a"         ~> "80084bf2fba02475726feb2cab2d8215eab14bc6bdd8bfb2c8151257032ecd8b"
  , "foo"       ~> "76d3bc41c9f588f7fcd0d5bf4718f8f84b1c41b20882703100b9eb9413807c01"
  , "alma"      ~> "d9c83bcef3c447006da16ffddbfc99df116011a4ddd0c7d2cb69a3ad00a2ce0e"
  , "almakorte" ~> "6b0c346d292b302570d7e56d380bb6d3054fc6984573d62e67914b0cc321c0d9"
  , "almakorteszilvabanan1almakorteszilvabanan2almakorteszilvabanan3almakorteszilvabanan4almakorteszilvabanan5"
                ~> "d1bbbc3776561d2171f8d78fe47aa69fa6d099de313b72bc141ea5fcb458aa42"
  , "almakorteszilvabanan1almakorteszilvabanan2almakorteszilvabanan3almakorteszilvabanan4almakorteszilvabanan5almakorteszilvabanan6almakorteszilvabanan7"
                ~> "2b442459292b7459fecab8aeddb15a07dfe717500d4e031b3620ec5bd895ea41"
  , "almakorteszilvabanan1almakorteszilvabanan2almakorteszilvabanan3almakorteszilvabanan4almakorteszilvabanan5almakorteszilvabanan6almakorteszilvabanan7almakorteszilvabanan8almakorteszilvabanan9"
                ~> "b34de571ca5c426d7d6591323f55f1ab584bac6f807f6fad2591e37787f8d3d7"
  , "almakorteszilvabanan1almakorteszilvabanan2almakorteszilvabanan3almakorteszilvabanan4almakorteszilvabanan5almakorteszilvabanan6almakorteszilvabanan7almakorteszilvabanan8almakorteszilvabanan9almakorteszilvabanan10almakorteszilvabanan11"
                ~> "682d08a7e1dd6766edcd421e874ee572635b22247e81823363161cf7fa5e6fa9"
  ]

sha3_384_vectors :: [(String,String)]
sha3_384_vectors =
  [ ""          ~> "0c63a75b845e4f7d01107d852e4c2485c51a50aaaa94fc61995e71bbee983a2ac3713831264adb47fb6bd1e058d5f004"
  , "a"         ~> "1815f774f320491b48569efec794d249eeb59aae46d22bf77dafe25c5edc28d7ea44f93ee1234aa88f61c91912a4ccd9"
  , "foo"       ~> "665551928d13b7d84ee02734502b018d896a0fb87eed5adb4c87ba91bbd6489410e11b0fbcc06ed7d0ebad559e5d3bb5"
  , "alma"      ~> "329c18d20f7cabb774ded412f5b7935ec0c7f588c072c89418f98c3eb89c39b78dc461dc117f9cd541f82a49e1e6222a"
  , "almakorte" ~> "cf1d0ed3e020b7066882045f98774a262be026317d5c1db43a27ad55dd9c9208409f9b4167224b230adc807f982f3f75"
  ]

sha3_512_vectors :: [(String,String)]
sha3_512_vectors =
  [ ""          ~> "a69f73cca23a9ac5c8b567dc185a756e97c982164fe25859e0d1dcc1475c80a615b2123af1f5f94c11e3e9402c3ac558f500199d95b6d3e301758586281dcd26"
  , "a"         ~> "697f2d856172cb8309d6b8b97dac4de344b549d4dee61edfb4962d8698b7fa803f4f93ff24393586e28b5b957ac3d1d369420ce53332712f997bd336d09ab02a"
  , "foo"       ~> "4bca2b137edc580fe50a88983ef860ebaca36c857b1f492839d6d7392452a63c82cbebc68e3b70a2a1480b4bb5d437a7cba6ecf9d89f9ff3ccd14cd6146ea7e7"
  , "alma"      ~> "f0065734b99e7920159740c85a53e175015aefa51b7a4611b3717c9677764cfbb838c640a7cd96172bc7aed833bb7dafe068cb101a2c54d944de30ba6794208d"
  , "almakorte" ~> "59dd735ca11151ec14c983f409e954d3bae615d7fac0e5b6ba57d9c36186e35227ea485fc31481c027bd463f8122428220105b3aded835575c87734e4348b8db"
  ]

--------------------------------------------------------------------------------

keccak_224_vectors :: [(String,String)]
keccak_224_vectors =
  [ ""          ~> "f71837502ba8e10837bdd8d365adb85591895602fc552b48b7390abd"
  , "a"         ~> "7cf87d912ee7088d30ec23f8e7100d9319bff090618b439d3fe91308"
  , "foo"       ~> "daa94da7f6806bf5a4e0af60379d75c62cadd6be5427c16d01e76cca"
  , "alma"      ~> "1488bc5a66e1303515111142db260413a79f716a205eebef01714dcd"
  , "almakorte" ~> "68905c20c26638792204fdba164be140d75e333fc1626850d5108432"
  ]

keccak_256_vectors :: [(String,String)]
keccak_256_vectors =
  [ ""          ~> "c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"
  , "a"         ~> "3ac225168df54212a25c1c01fd35bebfea408fdac2e31ddd6f80a4bbf9a5f1cb"
  , "foo"       ~> "41b1a0649752af1b28b3dc29a1556eee781e4a4c3a1f7f53f90fa834de098c4d"
  , "alma"      ~> "4814bb924a6c4a34e6135ad6ee6bfb6e9c534244df8f9083a071d63b47277368"
  , "almakorte" ~> "9aef4dd52d487152ce8c69ffab9ae17d77f83cd6f7817c21ef329cf120dd40ff"
  ]

keccak_384_vectors :: [(String,String)]
keccak_384_vectors =
  [ ""          ~> "2c23146a63a29acf99e73b88f8c24eaa7dc60aa771780ccc006afbfa8fe2479b2dd2b21362337441ac12b515911957ff"
  , "a"         ~> "85e964c0843a7ee32e6b5889d50e130e6485cffc826a30167d1dc2b3a0cc79cba303501a1eeaba39915f13baab5abacf"
  , "foo"       ~> "19d3f8607d2c6519443ab70bf1f7c86e9da4fda7fbcba7bfae0cab6190d24606f48334a7382c60db479d49bfd9fa815c"
  , "alma"      ~> "adc16dd5e2784f7d93a523aca756e39140cc0404e448dbce1c2f56daad6e9ba399b935bece953676cc11e2739931de47"
  , "almakorte" ~> "7170e29feb017551134b04bf26ed88f567b93ced8b55e3105d5ccad4a208ab15dc42dc4fc8ccd64d4be93a450f8a5a3b"
  ]

keccak_512_vectors :: [(String,String)]
keccak_512_vectors =
  [ ""          ~> "0eab42de4c3ceb9235fc91acffe746b29c29a8c366b7c60e4e67c466f36a4304c00fa9caf9d87976ba469bcbe06713b435f091ef2769fb160cdab33d3670680e"
  , "a"         ~> "9c46dbec5d03f74352cc4a4da354b4e9796887eeb66ac292617692e765dbe400352559b16229f97b27614b51dbfbbb14613f2c10350435a8feaf53f73ba01c7c"
  , "foo"       ~> "1597842aac52bc9d13fe249d808afbf44da13524759477404c3592ee331173e89fe1cbf21a7e4360990d565fad4643cdb209d80fa41a91dea97e665022c92135"
  , "alma"      ~> "299372ba9aa09bc0a46e3347ce5115d3d3db5c6a609780140462fd78d3eba13ff1448a513f25cfbd7a7f331ead6b29fa57bcd15504d28a22eaaf12bd75a15754"
  , "almakorte" ~> "0d9e1abe0fac3a5754e78aa90bbb31606c9853a8754791424a194f408be6b7306a0c5ac686316ba6c38477b748dca353ce0ca475239c06d29d19530d3b4f61e8"
  ]

--------------------------------------------------------------------------------

-- 10000 bit = 1250 bytes of output
shake_128_10000 :: [(String,String)]
shake_128_10000 =
  [ "bar"    ~> "049d3001a053583fb27efab27ea80ffd934c478b9a0a7e12b0af4851bb7a0579520c339caf7975a703088c65ec1d57ca44c3abaad38e021ae3560d812023e6bef5d8841ed25d3991d1b9284c248b86ab82d25f89a117b7f85914fdb8e765976e120d5144f27d86facf84fdc9f1a19c7abd2156767f98102037d18aa82af02d18dbd1d1fb0ca5f3bd5db18015d83d44efe76fc1c6a7b3dd5396a6e2585007627bc76326ba555704ff35b0f3134d7dd46243a7e8aa0649d963401768689a7bc167014f67fb73f5dfa4c318760dea30fee6555a72e0a3ac3c0defdf41a505da4c948507f19b20c1a3c1022d9f40a0a37dcc904043224fa54a6f68f01887dd9a45b3d7bd870f6a3bfb1aef8fdd65e21cd1d7bbfb1a854c212d6698a60954eba97ba5ab2b0ff9260ba3ae7c6d9b301c3bdecf89d3f360c62ab97a2aff9566826fb0521f6ac163b7e800fe6e3a057ffdbeb12d8d5ab40d0b0c15e4b065074550ba228e5633f690d183927f7cc32058c2b50faaec89171e45cb8d62e3429ada75cdb8d20563a56d3a154a86f9d3ae0824197267ccc8668e754c5ab50856cdbb20037a70c83a09a78dca5026f9674e84091fc46a7ebd5c3c3535f383285663b85b789a64f52b6bc960013fda75015ae6051289de14ad5dca73f70bd1c404866d999df3e67bfba4415987531683c26cecacbf90f60b9baf8a7feda49b98fc468d846b4b67a2c77727ed99864ae32cec5b49d55c011590719f8192e48762c854a31c970a3d05bd65a5f1ac9dd3e4357e88e026cbf88cad18168685c3b8ee7eab6f1c2f0e26d5c0537420cdfbdf2498ebd88aa8dc5f6228bc9df913ec51d7940f56d3bca6b6b093ba7aabc6aea689e42106e8af0c0e040242b12fe39edf93c6b33ff62c72ff31367f2623dab9c4991abb4a762c0b07bb4297c342df477f195a0bff9c15003342a697e2a45192272f9314be178482ee8ccba658719f660337e572a576d9fd2be539f6304edc842a8160e687c488d1dff6071684b1c0af5b5df55253b6a8e717b293c48048108079a7c7921c5c02cd27b1c50bca2bfb9df4748d0e0618c33a9215864ea7f4545d2583e2c0420c9febdfeaff1b28797747a20ccfaa1780d30123223557a8a29fd7629f0d29dfb935836b97a145ad61c733a071b96bdce630a675302359045eb18f8cf4bfffd317d6312992405797f57a480e263775ffa5afd387c87bf5d47b270132ac78c88db5fd5266c8d63389ece3b482790813c32ebf215b406f30a0a8e4de366acc293a628083183ea4cf62cf91ca54ca4d6a43bc480cdd49fafca68e610861e32202c633f14480ea1e6c0ca87fbf15de539cd739e9e27acfec41d34929c73ca26f2a6fc119ba0411179b78d9184af5e7f15c9dbc1d680fe1bccb8c7da4abb4bfe1b2b3924428975a07fa1a785e33347e6c83371771e88983f7dd9246cac513e1c26cba7581788139a5bb18a748ad503c711010823d15f685a9fbe820ff50da70950421e646d003a82413685597c4efb3b8d2dfe838a859f8f02f8d33d5e0d02ca5d85f8ec624219335176808cce6c8734fe52df91830a49dda9e8ccb6767cf31ee3a8709922fc290b20bc573223ee07b073dcc960c7ce42637f67017541b715682716ccaae9f3545c4676804939f4962dcd308fa6047d17fedc2d8064488118f8f2ca197602720beb1536bb71477f14aba8b0a32d4fa06fd6de4db2568a9ed53ae2f5565ad115e906565ff0f3803f4d734185bd13a02e657ec"
  , "szilva" ~> "84bdd4e8ee3040db7e56aebf199a8be14696e133e61d39e1ece426a067042377ef437f12fe58977b91de73e2258515c98e9df213b16a64f2b41a4676188cc89b92db3796bd4436ee84f973fffdcdcb5f743cf02174a5e42c759a45060ed16d0e3a802403fe651db4b094fdb93c6e497c718cfa78ad01e39cfffc896b47f5af21040eeb88f8ee7c981771dab7f27c5a4a752ac5e6b4056452d6bfd5161daf3786b1f996459fe8a2bc59fd8bfb9ff38ed40f325bba6b02bb9f3ed5e340fd35b6b56b817696606f30f5f29cfd0c46bb206421c05e450618d5ee8913c6b6bf5d4be33a5599f9414931b50e0d657803354dc9c2b1aa1ed5bd2ce518012e4fcf8820ea6d00d41b93c6b90b10591d71fb8dcadb2db7d782c142a1a9a2f1fccad131099b0f26d0cd4b82749bd4154fef35c8454c1f15110018dbeae9478e2781f1cbc6ed44cfbb0f55e5fba3a1ab94aa36acb3aef06475653332800944a100ba4c2bfb45fdcaae8237771181cecc72bbbfee594b8921f03a78323ff9f118c55eb063b48e821626e0eed1dee8f22fa897da1f7d15565ca06ab76c00cc58661208d2fe36df81b73a94d70c8469485c376386fcce753e522f5b0824893f9f0aa494ad80eba16ce486950770b95b8020a74f5fa81d76e098ecbb1fcc05953d35fd3753ef4834c6249f69fdc86434181cd10cdb8c14f56f612d9fdc1f8fc9df4f7b031ce09e1bfdc789178459c007c7ea482a1478c4ca1f29fbaaece516e367d786806393b81facd93cbcae401eae8ce5cfad008923326f9aadc1316026934c4338c58b85c0edd68639afc805ae17713a1e4c915afc380a022ee124ffb1a477b886e022aabe15452bd38437921bc3250133bc6c0e5a72c9b24d0493e1d0532f627ebb3b761148fa8473325df9febf20b2c46a36c45359d291ecadf43a28590754ea8beeb1832795373bf3295bce71d181910b687c435ae5ac31a2afebd9aa7e4b2271af9b9902172ebda49e4987bce68f31baadb4c392a5fb214923faf584f5e60b75348578925bc647a4dc55b6856d200674fa5173bacd73b0fc57ba0a46eaca0be64f8a41784ae7db207f9462d3df1a38fa00e408b6852f946eaa39c0ffe278d560cae8f7720b0f4be9d96784bb63f496d8b8caef99d53b9f6c2d9094f5bea43071533dbd5e47eb1f0b79d9a01af4717f3520d4ab7e2481fd85e5be2408fc3621a024dddcf0a822d1d66041bb01603513f4839e03053e891227e5a7a99170c72ed143aa9b6276cfaf9bb21a1c74392b8e96d2ccee7e2803deb28d96fc3df5d193b627f7900a721c8785b891b85a608620ebb25fca73e1b030dc09057af8362e4808a761c349262eb3a95d0927d0c4edf6cdb9e07c33344bd1cf5e814b522641fe8dbb47a6a70aba765537f17c59d4f2ad127489ecdbd0f19a3d2f89b8ab4824407581d7a5739c13245b695ba653394124bf48e48116222dafaf9db9ed1378b901ff3ec0fb831c64ee04254355d7143644d871a0a361456a0ea6849e214dd2176fdd45029282c5dc4abd110ed3044ef47ad8662958f76f9ad9584958ccd097bb2e3fdcf8fdffe6fe084a283063b5b6f38fffda1cf611ec16f5a06194a6dfa40181512ae675eefdfff73c27e343aa37e1aba1fb4e79cc26e4504cb7f9512ca1fe4548f8f00c0f592432449d32f6c4adf1cccfd46e6ba14350dedbec0f67877b76a74345c15ef76fc4941634a3a70d8e623b4858b99e80ca79376c599112093fd628a0f9ff32132fb4"
  ]

shake_256_10000 :: [(String,String)]
shake_256_10000 =
  [ "bar"    ~> "a15163728ed24e1c7eddc03e92cb421940a0d1fe653bee3294358adf391d4da26adc110656392ff324f01dcdc9596196f1e66a3a071bb226e71a6f53ca2bf4ad248026ab403c3660982e57f43a783eb752586b62fc346df5b8739d7e5a5faa35a97ad63cecd566935218719042c26c58d3c8826c9cb1d15596f1b9aabd4efeb64a3e2daf2265bcb2708d31c30729f46b944d0b8c56a2865e22e8cc75d1e3eac7f05a607421abf6e45d8782f363347b3b8de7b8dc3087d2cb6903a2fdee44c9b7c2021df0d266c270a5335859f5a204da88c647c97630c9d591fcc7ed5072fa6bca0f450c238dcf363977b72c7e73b66955ab751b8b5fd371edaea28072de1f24f30030b204b4f5cd3e42c975aa85a5c464de9fc094f2aca078dcacd6a695ba1ccd367c54dc4414ea0bbf7e96da35c7f1e070cd94678d0720f223e2698f0b72892cb7156118a54b414e6e6cb1865c1378fd9c4045ae556f87a7cce5ab14fd1001d1766e7287efde87aaff37fbd0c4ce5730e32a2cc4ac62b57138f4a858f70f4d798aa14e59940ce943ce563caec64a61e9168b9181e6f500caa01142e7910449d25b33e021fcb780b8c91816dc185b4a8c84f20b27552246e260aee71d61c371321120fada43a8a56ddcb4c3bc9fcc6183ab5d82dd217f0f93bd60a4873a86728c4821ef24e5daf0c15f982986b125cca159ccf98a6b8cb2439f7d52f7eb245e56dc3a7f69b689a4c6bbe0636ac9925d55ed8aabec2d55f89747aa219ed2f78a7386708df78d6aebb1734c9741f42727483272932d5af18acdfee4016b0cd256f09b2f9d581fd60433e2878bc7d49ee53a9d8a215482af30fd06ef831a627dba8487f0403a98c54416757696de5fbece45fcc947555fcf56656ecfdded8e4e4da4151eb34367d98bcc97a205cff1beb8a04e74479a553bd7543a1acb8f896d8d33b1559dc7e1a77666cb298bc97ef217a2d3f6440a2a03097f91d8a3fe8efed411b75a81d4c478fea5a49a4eacff0ddfe676d87b37f975cb02e6835a9d017579706c4d42c6905b9371824e4c8178cc7735e106862704b36777faa14360de8c4f529b1284ef35dfbca56fb9b85f213aec44c3b34c809a0b1f10aea218772bd125ddfd926f43dde6610c4ee3f436e8e2decd67b48d70cd2d8176417a587987f93ae4811046ef14008aa9a2149cfb0ec8371b3a6b026f67972ed6397ce82230b1d0cc4ce9cb53321c43c6c2d094b99b08b7301200ee29b487ebd07456c6e318461c783fe47e2a79ee32815fbd7992ebe7bf31081fa508f30e22767dd87b0a753ff98260c8989e15157322773607449a5d48bcbb084bfc653dcbb61dbe2450c736bd7372dfb3320126a0656e555bcebfe440b2efd17c9a5bcd075bf4eefa14f67e71498ba663091ca818b7ed9567c2f456703d51e2887647d2fa1ffc9bc720cc39e25137b0bf85fea037c93222b5f8dadeeee2ace51b76971d729ff29b64d2573ef9172266924e0bdfb2be9a9d439fc359dfeb95968716c0c98dc0b471f0a371eff1a5a54d86c5dce899205dc7be6c6c9a7612275b8a64d19c9b6f0d4844e1dce17b2d7fffdf23cbe90abe78691bcc2c9333216bd96a090527984ca871251c5004a273213e0d2480d47130122fcfb656a4110203e0a792e09f61ee2480d45b0c7543c1170e7f02e36f908879744ef8332311c7c3d763e70dc5e918dfe98ce00ad2dab3c1421b968831591047c4f46a712241e63a5762322d5a7b26fbfc7b9e2d5d4f5271"
  , "szilva" ~> "125347be70737f448fd336699ff72f7e27762cd1050aecece72d5784ebc4c16ce61abd29fcd78069fd2476a70e87085c13a9f42a0b4c905efb2186a209678164ace5a14073c1a965d1cc083f90f75fb06d5ff3ff3dd104420058dfff2c7bf819acdbbff59b13e29dcad6b010b0e585a4d618f0ed48c45f01f7695bfcbacb34920a81c0cf0074021046217198994043711dea242d082af3bbd5d11c721bec24327eefad1bf3d8eb0bd32ca3eb285496f3b553c468ef9cd47f3fd5eefe1aab97260691e9c752c267ef2a84370b0fa7167609b62471146bb9672232cd39b98514e59a643abe05abf328b35f042272e3ee76f9131ba997b7858fb3b3d1c098353dfaf9337a3d771665f25124a9814c3c43d3325bc0b8d77c6f62b2975f16433e77ced58ab9ece2dc6a0013cbe7f0af2b79d9df41fa50df2698f26efc121e9babd390db174b7cb4fd4706b7746f871ab4bb6b7033aec05ade410e557adc50e69398514fbdf36df144db9b759377734af7f2463e1d1c67f324d9e490edaf55bed3009ca26fdda497c8004011f40173dc8f6ce5b7bab78da46ee21abe6f50068a35a0f369c059b48bf006a0a0f543191b9d8ec286ceaaaa3f84fddfb25fb2f8a93bd032cb3a237ef19e3696ecd1e3ed729b9365ea89d7fade99165d69525734c442f3d2a924cc67240a4b428c7b8c3c790fb538b70237f9839b0fe1404c2d04c3deab42746ab9b7325eada8ec04bf59b47f56c70332b2a8814a6e748c6f751f2a42b30d2df24cd027c0d5e96b06ea18e5937fd1fc7779fe05337dad43e48486036249c6c93e7be724866f5f11d00788a90cd28de8d6c8d6368dc47f43376c9cc9aaf5bceb01ff4bc2853e530b83e692277501a9a1639cd386fdbdf89ffde8f6d39ff89b2e360778d21d7ec907db95d243e97ed8bae73217567a994e685ae6229f86044d76f7b303a260bb8b94923feebb62173d55ea470bdddcb4c9fa532d5fa94ca7e26b304bfd17b3d852afd3de0519fa76f161f864e9e97e43bc3cce7b0e0df26aedf6385939885be7049b9ff08f298db030fce7b9d7380919068fe5d44853629b70c299f1529dfac8e780f8566129b3b3865944e50dc5f9170d848a6b7228bc41ab2b3d0d9a8641f2168dbc6f7f13f262f3b520c313fd2b8d3c5ebfd16b5b45198782f5cc9e9042a5eba2cd055a292f846064c1f317b085b5e07c03ce8163335d939f6d772f6d87c81dc1046a1b362e893256ff972ce8b699d278d8ac4e182391d630740f490c031b72fd5250c494995a0a9827740785de8b6b7be0891e4af490604a99bd4b9aae8c57173c08146a21e714fccb39c70bf14d42ee74eb80e3a7cc89113906833975b97503d0ea9b7c250d46411e5646bb2dbacf351d2c4f5b49933302af3a760a002768f455f15b039a6d87124335b8596a72ccc0ab748bf219e01f082ddcaf62bb16ccd051e0d19bf307a177016b7a294bf4713c8692bc00dd5802b49ce956339a5a67cca7bb897b323797605eafe462193d095b2eca7cf12fec785c36a115c684c041f54920665176820eb6dc896916d8bd265dfc9091a78161bda0a6a5575a12304578990d26e97f5f618c1fdd743bf4f8a548a0891c237ca0a613acd2b2ed14312f2a2ceca80395c539e778dd7e3caf763928051d6fc8029a066ade9e9796c7174a72c1773c4590fe104cdc7eca791cd873e79f4618c9fe01aee9db6253392db13d570e9eed312a4ac7c6eebdaa09476a1bcba558d0d3d4f9b7a45b"
  ]

--------------------------------------------------------------------------------
