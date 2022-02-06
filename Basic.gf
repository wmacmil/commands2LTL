abstract Basic = {

flags startcat = ListCommands ;

cat
  Commands  ;
  [Commands]{1} ;
  Command ;
  [Command]{2} ;
  Action  ;


fun
  OneCommand   : Command -> Commands ; 
  CompoundCommand : [Command] -> Command ;
  SimpleCom      : Action   -> Command ;
  -- now if we want to only change it to one then it might mess up the 

  Go : Action ;
  Stop : Action ;
  Turn : Action ;

}
