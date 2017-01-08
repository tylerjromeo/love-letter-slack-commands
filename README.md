# Love Letter Slack Commands

A game engine and interface for playing [Love Letter](https://www.alderac.com/tempest/love-letter/) through a Slack.

The rules of Love letter are [here](https://www.alderac.com/tempest/files/2012/09/Love_Letter_Rules_Final.pdf)

## Playing

A Procfile for heroku is included. Once the service is hosted it can be added to your slack instance as a [custom command](https://my.slack.com/services/new/slash-commands)

The available commands are:

| command | description |
|---------|-------------|
|`/love help`| get this help|
|`/love start [player names]`| start a new game|
|`/love quit`| end the game in the channel|
|`/love hand`| see your hand|
|`/love status`| see all available information on the board|
|`/love play [card name] [?target] [?guess]`| play a card. (play can be omitted)|
|`/love discard`| equivalent to `/love play`|

## Building

### Sbt

Use the included sbt script to get into sbt:
```
$ ./sbt
```

### Compile

```
> compile
```

### Test
```
> test
```

### Run
To start service
```
> re-start
```
To stop service
```
> re-stop
```

service binds to port 8080 by default. Set the environment variable `PORT` to bind to a different one.


