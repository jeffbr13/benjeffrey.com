---
title: An Update to rinse-rss
date: 2015-04-13
---

Banger banger banger! I’m happy to announce that I’ve finally  updated [`rinse-rss`](http://rinse.benjeffrey.net "Rinse FM unofficial podcast feed"), my unofficial RSS feed generator for [Rinse FM](http://rinse.fm), to version `0.3.2-beezledub`, and you can now subscribe to recurring show-feeds without them dying on you later in the week!

[Riiiiiiiiiiiinse...](http://rinse.benjeffrey.net "Rinse FM unofficial podcast feed")


## Technical Details

- a separate [Celery](http://www.celeryproject.org) worker asynchronously scrapes Rinse FM every so-often, rather than it happening during HTTP request processing
- the scraped data is now stored in Postgres using [SQLAlchemy](http://www.sqlalchemy.org)
- everything is containerised, and run using [Docker Compose](http://docs.docker.com/compose/)


<iframe width="1280" height="720" src="https://www.youtube-nocookie.com/embed/rao9gviQQf0?rel=0&amp;showinfo=0" frameborder="0" allowfullscreen></iframe>
