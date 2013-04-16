-----
title: screen
description: Some notes on using the screen shell session software
date: 2012-10-26
-----

`screen` is a useful and powerful program that allows you to create and
manage multiple virtual sessions, independently of how or where you're
logged into a machine.


Running a Screen Session
------------------------

To create a screen session, simply type `screen` and you will get a splash
screen (at which you press space).

Whilst in screen, your terminal will not beep, but instead will flash
visually when it receives a beep command from an application.

Recovering sessions
-------------------

If you lose (or detach from) a single `screen` session, you can bring it
back up with:

```bash
screen -r
```

If you had multiple `screen` sessions running, this command will bring up a list
of them.

### Naming sessions
You can name `screen` sessions to keep track of them:

```bash
screen -S [my session]
```


Detaching from Sessions
-----------------------

You can detach yourself from a running `screen` session via the key-command
`^a-d` (`ctrl-a-d`).

To reattach a session (the `-d` switch detaches the session from other
terminals, if necessary):

```bash
screen -rd [my session]
```

or alternatively

```bash
screen -rx [another session]
```

which will allow you to attach to a screen session without detaching it from
any other terminals - multi-screen mode!
