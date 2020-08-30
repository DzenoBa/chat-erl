# Chat-erl - server

A simple chat server.

## Prerequisites
- Erlang/OTP 23
- CMake
- Rebar3

## Getting started
```bash
rebar3 compile
rebar3 shell
```
To exist the server type in `q().`

## Tests
The run the test write:
```bash
rebar3 eunit
```

## API
- WebSocket URL: localhost:8080/chat

## Joining or leaving a channel
```json
{
    "type": "join|leave",
    "payload": {
        "name": "<channel-name>"
    }
}
```

### Sending messages
```json
{
    "type": "message",
    "payload": {
        "channel": "<channel-name>", 
        "text": "<message>",
        "user": "<your-name>", 
    }
}
```

### Receiving a message
```json
{
    "type":"message",
    "text":"<message>",
    "user":"<user>",
    "time":"2020-01-01T01:01:01"
}
```

### Request response
```json
{
    "type":"ok|error",
    "payload": "<request-json|error-reason>",
    "time":"2020-01-01T01:01:01"
}
```

## References
- [WebSockets](https://en.wikipedia.org/wiki/WebSocket) - Wikipedia.
- [Cowboy](https://ninenines.eu/) - A HTTP server.
- [Jiffy](https://github.com/davisp/jiffy) - JSON parser.
- [Jesse](https://github.com/for-GET/jesse) - JSON validator.
- [pg](https://erlang.org/doc/man/pg.html) - Process group module (introduced in OTP 23.0).
