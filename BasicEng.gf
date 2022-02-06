concrete BasicEng of Basic = open
  SyntaxEng,
  SymbolicEng,
  (C = ConstructorsEng),
  ParadigmsEng,
  ConjunctionEng,
  -- ExtraEng,
  ExtendEng,
  Prelude in {



lincat
  Commands = Text  ;
  [Commands] = Text ;
  Command = Imp ;
  [Command] = [Imp] ;
  Action = VP ;

lin
  -- OneCommand   : Command -> Commands ; 
  -- CompoundCommand : [Command] -> Command ;
  -- SimpleCom      : Action   -> Command ;

  OneCommand c = mkText (mkPhr (mkUtt c)) fullStopPunct;
  CompoundCommand = ConjImp and_Conj ;
  SimpleCom a = mkImp a ;

  Go    = mkVP (mkV "go")    ;
  Stop  = mkVP (mkV "stop")  ;
  Turn  = mkVP (mkV "turn")  ;

  BaseCommand = BaseImp ;
  ConsCommand = ConsImp ;

  ConsCommands x xs = ss (x.s ++ xs.s) ;
  BaseCommands x = x ;

}
