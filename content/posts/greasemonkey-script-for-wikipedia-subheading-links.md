---
title: Greasemonkey script to add link anchors to Wikipedia page subheadings
date: 2014-04-29
---

Linking to specific sections of a Wikipedia article can be mildly annoying, requiring you to either

A) inspect the page source and find the subsection heading's `id` attribute, or
B) scroll back up to the TOC and get the link from there.

Instead, in the spirit of hacker laziness, here's a Greasemonkey script which wraps Wikipedia page subsection headings in links:

```javascript
// https://benjeffrey.com/greasemonkey-script-for-wikipedia-subheading-links
// Available under the MPL 2.0
// ==UserScript==
// @key value
// @name        Wikipedia Subheading Links
// @namespace   com.benjeffrey.wikipedia-subheading-links
// @description Add links to subheadings on Wikipedia pages.
// @include http://*.wikipedia.org/*
// @include https://*.wikipedia.org/*
// ==/UserScript==
contentsItems = document.getElementById('toc').getElementsByTagName('li');
for (var i = 0; i < contentsItems.length; i++) {
    subheadingHash = contentsItems[i].getElementsByTagName('a')[0].hash.slice(1);
    subheading = document.getElementById(subheadingHash);
    subheading.innerHTML = ('<a href="#' + subheadingHash + '">' + subheading.innerHTML + ' #</a>');
}
```

Apologies for the *ahem* MVP-like quality of the JavaScript...

[Greasemonkey script to add link anchors to Wikipedia page subheadings (Gist)](https://gist.github.com/jeffbr13/11387443).
