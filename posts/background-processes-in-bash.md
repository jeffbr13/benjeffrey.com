-----
title: Background Processes in Bash
description: How to deal with background processes in Bash
date: 2012-10-26
-----

Mostly stolen from <http://www.tardis.ed.ac.uk/wiki/Tardis_Beginner_Tutorials/8>,
if I remember correctly...


Initializing a Background Process
---------------------------------

If you append `&` to a command, it will run as a background process.
For instance, try this in your shell:

```bash
top &
```

To move a running process to the background, hit `^z` (`ctrl-z`).
This will freeze it, and return you to the prompt - then you can either
type `bg` to allow it to run in the background, without your input, or
`fg` to bring it back to the foreground. Now try bringing `top` back to
foreground by typing:

```bash
fg
```

or allow it to run in the background by typing:

```bash
bg
```


Listing, Killing and Re-"nice"-ing Processes
--------------------------------------------

Linux has a threads and priorities system which can be controlled (to a
degree) by the user.

To see who is connected to the same machine you are, type `w` - this lists
all usernames, where they are connected from if they are logged in from
the internet or another machine, when they logged in, how long
they've been idle, how much cpu time they are using on average, and what
they are currently running in foreground.

To list the processes you have running you can simply type `ps`. You will
likely only see two: `bash`, and `ps` itself. A more informative `ps`
command is:

```bash
ps auxf | less
```

This lists all processes, with users
listed with their processes, regardless of tty, and with a process
hierarchy display (which shows graphically which processes are children
of which). The output is rather long, showing both userland and system
processes.

The two most useful things to be found on the process list are:

1. the "PID" -- the four or five digit number on the left (used to reference
    individual processes)
2. the "niced" priority of a process -- telling the kernel how much CPU time
    it should be given relative to other processes

For a non-root user, priorities range from 0 to 19. A priority of 0
demands the process' maximum fair share of CPU time available, whereas 19
is the lowest priority, meaning it will use only idle CPU time that no
other process wants.

It *is* possible for the root user to create processes with negative
priorities, where processes aggressively grab CPU time whether they need
it or not. Also, only the root user can increase the priority of a
process, although any user can create a process with a priority between
0 and 19, and then reduce the priority (i.e. increase the priority
number) while it is running. Note that the "niced" priority assigned to a process is only taken as a guide to the kernel, and the
kernel will assign a lower real priority based on system load etc. You can
see both the real and niced priority in `top`. It seems
that nobody knows exactly why it's called "nice", but it may be to do with making the process behave "nicely", by not hogging
unnecessary resources. You can make your own processes "nicer" with the
`renice` command:

```bash
renice +10 [process PID]
```

You can set the nice of a process when you run it with the command:

```bash
nice [command]
```


Advanced Process Manipulation
-----------------------------

We are going to:

* create a resource hogging process
* stop it
* make it play nice
* kill it before it can complete.

First, run top in a new window. Now go
back to your original shell window - it's time to create a resource hogging
process that will never end:

```bash
grep sillystringthatwontbefound /dev/urandom
# search the system random device for a match of "sillystringthatwontbefound"
```

You will see in `top` that it's using considerably more CPU time than any
other process (unless you're unlucky and the machine is heavily loaded at
the moment), and you will see the kernel changing its priority constantly
to balance its demands with those of the rest of the system, while the
"nice" column remains at zero.

Now we will make our resource hog play a little nicer. Let's stop our
process by pressing `^z` (`ctrl-z`). Put it in the background by typing
`bg` and see it spring back up on the top list. Now we can take control
of the process, so let's tame it a bit. We'll need the PID of the
process, so copy it from the list in top - you can copy by just selecting
the text, and paste it when you need to by right- clicking in the window.
Now we know the PID, we can change its priority:

```bash
renice +10 [PID of our process]
```

Now have a look at the listing in top
again - our nice priority has been increased, but if the system is as
busy when you try it as when i was writing this, you'll find the actual
priority doesn't change at all because it's a higher number to start with
than your nice priority. Your nice priority is just saying to the kernel
that your process doesn't need to go over a certain priority. So let's
force our resource hog to absolute minimum priority:

```bash
renice +19 [PID of our process]
```

Now we see the process' priority has
dropped to 19 and if any process at all wants more CPU time our grep will
step out of the way.

Now to finish off, let's kill the process. We could just bring it back to
foreground and interrupt it by pressing `^c`, but killing it is pretty
much the same. This is what you want to do if a process stops responding,
or behaves in a way you don't want it to and you can't stop it in a more
friendly way. Try it now - simply type: `kill [PID of process]` It should
now have disappeared from the processes list. Sometimes uncooperative
processes don't die with just the standard TERM signal, in which case you
want to send it a KILL signal by typing: `kill -s 9 [PID of process]`
It's worth reading man kill as it explains different signals - you will
see that kill can be used to send other signals to processes, such as HUP
which is used fairly often. Now you know how to control processes you
run!
