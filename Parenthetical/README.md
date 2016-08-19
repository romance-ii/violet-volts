Parenthetical
=============

The WebGL- and WebSockets-based client program for Romance II game servers.

## What is Romance II?

*Romance* is a server platform (and, with this release, we're
beginning to provide client software and libraries as well) for
massively multiplayer online rôle-playing games (MMORPG). Romance
provides a(n increasing) number of facilities for open, persistent
multi-user worlds that make it easier for game designers and
programmers to not kill one another.

That is: the design of Romance is such that the design of the game is
meant to be very, very well-isolated from the game engine code itself.

In addition, Romance is being designed to be very modular, allowing
multiple games to be developed while sharing the cost/effort of
maintaining the infrastructural code.

## What is Parenthetical?

The Parenthetical WebGL client is a game-play client
application. It's written in Parenscript and uses GLGE for its
WebGL implementation, and a combination of WebSockets and XML/JSON
AJAX requests for communication with the server(s).

It is maintained as a part of the infrastructure of the
Romance project.

## Major External Dependencies

 * GLGE WebGL library
 * GNU Make
 * Parenscript
 * Quicklisp and various libraries, including Buildapp

Also, primary development of Parenthetical is being performed using
only Firefox at present, although other browsers are being
ocassionally tested and we do intend to support as many as plausible.

