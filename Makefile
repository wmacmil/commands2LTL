all:

	gf -make -output-format=haskell --haskell=gadt BasicEng.gf 
	# ghc --make -XGADTs -o query QuerySystem.hs
