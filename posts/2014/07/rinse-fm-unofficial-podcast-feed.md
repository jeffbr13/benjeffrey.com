---
title: Rinse FM unofficial podcast feed
date: 2014-07-01
tags: project
---

[Rinse FM](http://rinse.fm/)'s website CMS doesn't enclose the podcast media files in it's podcast RSS feeds, making it impossible to subscribe with podcatchers.

I was using [\@levelsio's Rinse FM podcast](https://levels.io/rinse-fm-podcast/) with great success for a long time. Unfortunately, it seems to break whenever there's a format-change or any erroneous data-entry on Rinse FM's side. I've [tweeted \@levelsio when it's broken in the past](https://twitter.com/jeffbr13/status/447019136934486016), but it seems unfair to bug him to keep it maintained, and he's not open-sourced it yet so I can't go in and fix the software myself.

So I built my own unofficial Rinse FM podcast feed! I built it with Python 3 and Flask, it's open-source under [the Mozilla Public License (2.0)](https://www.mozilla.org/MPL/2.0/), and the feeds are available at <http://rinse.benjeffrey.net>. Enjoy!

- [Rinse FM unofficial podcast feed](http://rinse.benjeffrey.net)
- [rinse-rss source code](https://github.com/jeffbr13/rinse-rss)
