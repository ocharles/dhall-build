let bash = derive "test" "mkdir -p $out" 
in derive "foo" ''
  ${bash}/bin/bash 
''
