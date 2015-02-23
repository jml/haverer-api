# API design

This is a rough draft of the API for haverer.

## Goals

1. Mostly trusted people write bots that play loveletter
2. Untrusted people write bots that play loveletter
3. Maybe a pretty website for playing loveletter interactively

## Constraints

* Relatively easy to write a client in Haskell, Python, and Clojure
* Cannot rely on clients being coordinated through any means other than the
  server

## Assumptions

* Level of traffic is going to be vanishingly small, so scaling considerations
  can wait
* Initial users will be particularly patient and understand, and so we do not
  need explicit versioning yet

## Technology

RESTful API using HTTP 1.1.

Probably JSON for resource serialization.

Probably OAuth2 for authentication.


## Unanswered questions

* How to do authentication?
* How to allow users to register?
* Exact structure of responses
  * Depends on wire format
* Correct error code for errors
* How should old games be handled?
* How can we make polling as efficient as possible?


## Use cases

### Kick off a game

```
POST /games HTTP/1.1

Players (Open Int | Closed [PlayerId])
TurnTimeLimit Seconds
```

```
HTTP/1.1 201 Created
Location: /games/<id>
```

#### Errors

* Invalid number of players
* Unknown player ids
* Too long a time limit
* Non-positive time limit

Could possibly error if user not in list of closed playerids.

### What's in a game

`GET /game/<id> HTTP/1.1`

* Are we still waiting for people?
* Who's playing?
* What are the scores?
* Who won?
* Where's the current round?
* Date created

### Changing the game

Before the game has started, you can PATCH the time limit, and also the player
set.

### What's in a round?

`GET /game/<id>/round/<id> HTTP/1.1`

* Whose turn is it?
* What are everyone's discards?
* Who is still in the round? / What is the play order?
* Who is protected by the Priestess?
* How many cards are left?
* Is it over? Who won? Who survived? What was the burn card?

### Is it my turn?

`GET /game/<id>/round/<id>/player/<id> HTTP/1.1`

If that's you:
* hand

otherwise:
* discards
* active
* protected

Note this method might show you that you've busted out. You'll still need to
`POST` to the round for play to proceed.

#### Errors

* 404 No such player

### It's my turn!

`POST /game/<id>/round/<id> HTTP/1.1`

`Play Card`

```
HTTP/1.1 201 Created
/game/<id>/round/<id>/turn/<id>
```

#### Errors

* 400 Invalid combination
* 403 Not your turn
* 403 Inactive player
* 404 No such player
* 405 Round over
* 408 Request Timeout

### What happened

`GET /game/<id>/round/<id>/turns HTTP/1.1`

```
HTTP/1.1 200 Created
[Result]
```

### Who am I

`GET /me HTTP/1.1`

```
303 /users/<id>
```

## Notes

Because we can't rely on anyone doing anything in time, we'll need timeouts.
The simplest way to set this is a maximum time between your turn starting and
you deciding what to do with it.

A possibly nicer way is to have two timers, one timer that lasts until you
have polled to discover that it is indeed your turn, and then a second timer
starting from there until you play your turn.
