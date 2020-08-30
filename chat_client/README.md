# Chat-erl - client
A chat client.

## Prerequisites
- Erlang/OTP 23
- CMake
- Rebar3
- WxWidgets

## Getting started
```bash
rebar3 compile
rebar3 shell
```
To quit the app type in `q().`

## Commands
**Note**: *Don't forget to start the server before using the commands in the chat window.*
- `/connect` - Connect to the server.
- `/disconnect` - Disconnect from the server.
- `/join #<name>`- Join a channel.
- `/leave` - Leave the channel.
- `/nick` - Returns current nick name.
- `/nick <name>` - Set nick name.

## References
- [wx](https://erlang.org/doc/man/wx.html) - A API of wxWidgets. 
- [WebSockets](https://en.wikipedia.org/wiki/WebSocket) - Wikipedia.
- [Gun](https://ninenines.eu/docs/en/gun/2.0/guide/) - A HTTP & WebSocket client.
- [jsone](https://github.com/sile/jsone) - A library for encoding & decoding JSON.
- [leex](https://erlang.org/doc/man/leex.html) - Lexical analyzer generator.
- [yecc](https://erlang.org/doc/man/yecc.html) - Parser generator.
