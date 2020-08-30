Definitions.

Name = [a-zA-Z][0-9a-zA-Z_]*

Channel = #[0-9a-zA-Z_]*

Whites  = \s+

Connect = /connect

Join    = /join 

Nick    = /nick 

Disconnect = /disconnect

Leave = /leave

Digit   = [0-9]

Rules.

{Name} :
  {token,{name,TokenLine,TokenChars}}.

{Channel} :
  {token,{channel,TokenLine,TokenChars}}.  

{Connect} :
  {token,{'/connect',TokenLine,TokenChars}}.

{Join} : 
  {token,{'/join',TokenLine,TokenChars}}.

{Nick} : 
  {token,{'/nick',TokenLine,TokenChars}}.

{Disconnect} : 
  {token,{'/disconnect',TokenLine,TokenChars}}.

{Leave} : 
  {token,{'/leave',TokenLine,TokenChars}}.

{Whites} : skip_token.

{Digit} :
  {token, {digit, TokenLine, TokenChars}}.

\. :
  {token, {dot, TokenLine, TokenChars}}.

\{ :
  {token, {'{', TokenLine, TokenChars}}.

\} :
  {token, {'}', TokenLine, TokenChars}}.

\, :
  {token, {',', TokenLine, TokenChars}}.

\@ :
  {token, {'@', TokenLine, TokenChars}}.


Erlang code.
