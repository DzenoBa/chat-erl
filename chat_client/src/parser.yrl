Nonterminals line.

Terminals '/connect' '/join' '/nick' '/disconnect' '/leave' channel
   name. 

Endsymbol '$end'.

Rootsymbol line.

line -> '/leave' : leave.

line -> '/disconnect' : disconnect.

line -> '/connect' : connect.

line -> '/join' channel   : {join, ff('$2')}.

line -> '/leave' channel   : {leave, ff('$2')}.

line -> '/nick' : nick.

line -> '/nick' name : {nick, ff('$2')}.

Erlang code.

ff({_,_,Content}) -> Content.
