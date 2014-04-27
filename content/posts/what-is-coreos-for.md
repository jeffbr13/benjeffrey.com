---
title: What is CoreOS for?
date: 2014-04-27
---

CoreOS is an operating system which attempts to minimise

1) the overhead of unneeded services
2) complexity and potential surprises (variability) of the environment

by providing the minimum possible familiar (Linux) environment for software to run in.

These properties are very useful for:

1) Running software in isolated environments (containers/virtual machines), where much functionality can provided directly by the environment host's services and software.
2) Automating software deployment, since complex and unknown environments will need equally sophisticated software to handle them.
