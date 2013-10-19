---
title: Jane Street Interview Reparations
description: I brainfarted during a programming telephone interview. These are the programs I should have wrote.
date: 2013-10-18
---

So today I had a phone interview with the 'market makers' Jane Street, for
a summer internship position. They are a financial trading company
who use ML as their main programming language and have not only open-sourced
a complete asynchronous library, but also their own take on the OCaml standard
library! So these guys obviously lean heavily on the functional side of
programming.

So yeah, I should perhaps have prepared better for their call!
The interviewer said that I could work in any language I wanted to, and my
challenge would be to implement a queue. I chose
Python as my weapon, as I had been using it at work just a little earlier
in the day.

This was a mistake. The first task was to come up with type signatures
for the core queue functions, `push` and `pop`. And without any such thing
existing in Python, I tried to lean on my rusty Haskell knowledge to come
up with `push :: a -> Queue a -> Queue a` and `pop :: Queue a -> a -> Queue a`.
The reader familiar with this type notation will note that **they are both wrong**.

I feel it only went (slightly) downhill from there. Following tasks (my
mistakes in brackets) were to:

- implement this queue functionally, using 2 lists to amortize lookup complexity
    (I decided to write this as a Python class... *hurr-durr*)
- implement a `peek' function (I mostly copypasted from my `Queue.pop`
    implementation)
- ensure that the queue followed FIFO ordering (mine didn't!)

To make up for these errors upon errors, here are a couple of functional
queue implementations in Haskell:

```haskell
-- Naïve implementation of a queue a list, so pops are O(n) complexity.

newtype Queue a = Queue [a]
    deriving (Show, Read)

push :: a -> Queue a -> Queue a
push x (Queue xs) = Queue (x:xs)

pop :: Queue a -> (a, Queue a)
pop (Queue xs) = (last xs, Queue (init xs))

peek :: Queue a -> (a, Queue a)
peek (Queue xs) = (last xs, Queue xs)
```

The naïve implementation also doesn't handle empty lists, but it's early now.
I spent a little more time on my attempt using two lists to amortize complexity:

```haskell
-- Two-list queue implementation.
-- Pushes are cons'd ont the front of the `in` list.
-- Pops the head of the `out` list.
-- Reverses and sets the `in` list as the `out` list if the `out` list is empty.

data Queue a = Queue {in_l :: [a],
                      out_l :: [a]}
    deriving (Eq, Read, Show)

push :: a -> Queue a -> Queue a
push x q                            --  (in) (out)
    | length (out_l q) == 0     = Queue ([]) ([x])
    | otherwise                 = Queue (x:(in_l q)) (out_l q)

pop :: Queue a -> (Maybe a, Queue a)
pop (Queue [] []) = (Nothing, Queue [] [])
pop (Queue xs []) = pop (Queue [] (reverse xs))
pop (Queue xs ys) = ((Just (head ys)), Queue xs (tail ys))

peek :: Queue a -> (Maybe a, Queue a)
peek (Queue [] []) = (Nothing, Queue [] [])
peek (Queue xs []) = peek (Queue [] (reverse xs))
peek (Queue xs ys) = ((Just (head ys)), Queue xs ys)
```

Further room for improvement? I could perhaps rewrite `pop` using `peek`,
but *sleep*.
