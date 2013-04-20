-----
title: Linux Processes
description: Manipulating processes from the shell.
date: 2012-10-26
-----

**Note**: Mostly stolen from a [Tardis][] [beginner
tutorial](http://www.tardis.ed.ac.uk/wiki/Tardis_Beginner_Tutorials/8).


Background & Foreground Processes
---------------------------------

Appending `&` to a command $c$ will run it as a background process:

```bash
{c} &
```

To move a running process to the background, hit <kbd>ctrl-z</kbd>. This
will freeze it, and return you to the prompt - then you can either type
`bg` to allow it to run in the background, without your input, or `fg` to
bring it back to the foreground. You can bring your frozen command $c$
back to the foreground by typing:

```bash
fg
```

or allow it to run in the background by typing:

```bash
bg
```


Listing Processes
-----------------

To list the processes you have running you can simply type `ps`. You will
likely only see two: `bash`, and `ps` itself. A more informative `ps`
command is:

```bash
ps auxf | less
```

This lists all processes, both userland and system, with users shown
alongside their processes, regardless of tty. A process hierarchy display
shows graphically which processes are children of which. The output is
rather long, showing both userland and system processes.

The two most useful things to be found on the process list are:

* the "PID" -- the 4- or 5-digit number used to reference
    individual processes
* the "nice" -- telling the kernel how much CPU time the individual
    process should be given relative to other processes


Process Manipulation
--------------------

### Killing a process

To kill an uncooperative process, we can do one of two things:

* bring the process to the foreground with `fg` and interrupt it
    by pressing <kbd>ctrl-c</kbd>
* `kill` the process

To kill a process, type:

```bash
kill [PID of process]
```

It should dissapear from the processes list.

Sometimes uncooperative processes don't die with just the standard TERM
signal, in which case you want to send it a KILL signal by typing:

```bash
kill -s 9 [PID of process]
```

It's worth reading `man kill` as it explains different signals - you will
see that kill can be used to send other signals to processes, such as HUP
which is used fairly often.


### Prioritizing processes with `nice`

To start a command $c$ with a specific "nice" priority $n$, run

```bash
nice -n {n} {c}
```

The "nice" priority assigned to a process is only taken as a guide,
saying to the kernel that your process doesn't need to go over a priority
of $n$. The kernel assigns real priorities based on system
load and other factors.

The default nice $n$ of a process $p$ is 10.

For non-root users, the nice $n$ of a process must be $0 < n < 19$. The
highest priority is 0, demanding the process's maximum fair share of CPU
time available. 19 is the lowest priority, meaning it will use only idle
CPU time that no other process wants.

The root user can create processes with priorities < 0, which
aggressively grab CPU time whether they need it or not.


#### Prioritizing already-running processes

Say we want to tame a resource-hogging process $p$.

First, we need the process to be running in the background, so we can
take control of it. If we have the the PID of the process $p$, we can
decrease its nice $n$ with:

```bash
renice ±{n} {PID p}
```

If you look at the listing in `top` again, $p$'s nice priority should
have changed to $n$. If the system is still busy, it's because the actual
priority hasn't changed -- it's often a higher number to start with than
your requested/nice priority.


To force the process $p$ to absolute minimum priority, type:

```bash
renice +19 {PID p}
```

This drops $p$'s nice priority to 19, so that if another process
wants more CPU time, then $p$ will step out of the way.

Only the root user can increase the nice $n$ of a process $p$, although
any user can create a process where $0 < n < 19$, or reduce the priority
(by increasing $n$) while it's running.


<!-- links -->

[Tardis]: http://www.tardis.ed.ac.uk
