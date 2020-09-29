---
title: "Shuffletron, a Common Lisp Music Player for the terminal"
date: 2018-09-11T16:36:23+02:00
tags: ["libraries",]
draft: false
---

[Shuffletron](https://github.com/ahefner/shuffletron/) is a nice music
player for the terminal written in Common Lisp, "based on search and
tagging", that seduced me with its attention to details. Moreover, its
author was very responsive to fix a couple issues.

The first time you launch it, it will ask for a music repository and
will propose to scan it for id3 tags with the `scanid3` command. It
is optional, but it allows to print colored information:

![](https://github.com/ahefner/shuffletron/raw/master/img-search.png)

The basic commands to know are the following:

- search with `/` followed by your search terms. You'll notice that
  your prompt changed from `library` to `xy matches`. You can **refine
  the results** by searching again. To enter a new query we have to go
  back to the library, with as many successive "enters" as needed.

- to play songs: `play`. We can select which songs to play, using their index:
    - comma-separated indexes of songs: `1,3,10`
    - a selection with a dash and an optional end: `1-10`, `0-`
    - a combination of the two: `1,3-10`.

- there are the obvious `pause`, `shuffle`, `skip`, `next`, `seek`, `repeat`,…
- `now` to show the currently playing song.


There is also a **queue**, **id3 tags management**, **profiles** to
use an alternate library (`./shuffletron --help`), and even an **alarm
clock** feature which allows to program music with something like:

```
  alarm at 7:45 am      # \"at\" is optional and doesn't change the meaning
  alarm 7:45 am
  alarm 9 pm
  alarm 7               # If AM/PM not specified, assumes AM
  alarm in 5 minutes    # Relative alarm times, in minutes or hours
  alarm in 10m          # minutes, minute, mins, min, , m are synonyms
  alarm in 7 hours      # hours, hour, hr, h are synonyms
  alarm in 8h
  alarm in 7:29         # h:mm format - seven hours, twenty-nine minutes
  alarm reset           # off/never/delete/disable/cancel/clear/reset
```

I can see a use for a pomodoro-like technic :)

I'll list the complete set of commands below (available
[on the sources](https://github.com/ahefner/shuffletron/blob/master/src/help.lisp)),
but first a note on installation.


## Installation

Shuffletron doesn't provide executables
([yet ?](https://github.com/ahefner/shuffletron/issues/6)). The
procedure is now documented in the readme so you just have to

    make shuffletron-bin  # sbcl
    sudo make install
    ./shuffletron

This last line calls a script and it is actually important to use it,
to link dependencies and to use `rlwrap`. There is room for
improvement here.

To read Flac and Ogg files, you need those system dependencies:

    apt install libflac-dev
    apt install libvorbis-dev

Finally, scanning my library failed for the first time, because of
badly manually encoded ogg files coming from youtube. The mixalot
library prefered to fail instead of showing error messages. If you
encounter a similar problem, see
[this PR](https://github.com/ahefner/mixalot/pull/7).

## All commands

In the application, type `help`, and `help commands` to get this list:

```
Command list:

  /[query]       Search library for [query].
  show           Print search matches, highlighting songs in queue.
  back           Undo last search.
  [songs]        Play list of songs.
  all            Play all songs in selection (equivalent to \"0-\")
  +[songs]       Append list of songs to queue.
  pre[songs]     Prepend list of songs to queue.
  random         Play a random song from the current selection.
  random QUERY   Play a random song matching QUERY
  shuffle SONGS  Play songs in random order.

  queue          Print queue contents and current song playing.
  shuffle        Randomize order of songs in queue.
  clear          Clear the queue (current song continues playing)
  loop           Toggle loop mode (loop through songs in queue)
  qdrop          Remove last song from queue
  qdrop RANGES   Remove songs from queue
  qtag TAGS      Apply tags to all songs in queue
  fromqueue      Transfer queue to selection
  toqueue        Replace queue with selection

  now            Print name of song currently playing.
  play           Resume playing
  stop           Stop playing (current song pushed to head of queue)
  pause          Toggle paused/unpaused.
  skip           Skip currently playing song. If looping is enabled, this
                 song won't played again.
  next           Advance to next song. If looping is enabled, the current
                 song will be enqueued.
  repeat N       Add N repetitions of currently playing song to head of queue.
  seek TIME      Seek to time (in [h:]m:ss format, or a number in seconds)
  seek +TIME     Seek forward
  seek -TIME     Seek backward
  startat TIME   Always start playback at a given time (to skip long intros)

  tag            List tags of currently playing song.
  tag TAGS       Add one or more textual tags to the current song.
  untag TAGS     Remove the given tags from the currently playing song.
  tagged TAGS    Search for files having any of specified tags.
  tags           List all tags (and # occurrences) within current query.
  killtag TAGS   Remove all occurances of the given tags
  tagall TAGS    Apply tags to all selected songs
  untagall TAGS  Remove given tags from all selected songs

  time           Print current time
  alarm          Set alarm (see \"help alarms\")

  scanid3        Scan new files for ID3 tags
  prescan        Toggle file prescanning (useful if file IO is slow)
  exit           Exit the program.

  help [topic]   Help
```

## See also

- [mpd](https://github.com/stassats/mpd), an interface to Music Player
  Daemon in CL.

other music players:

- [cmus](https://cmus.github.io/)
- Emacs' media players [http://wikemacs.org/wiki/Media_player](http://wikemacs.org/wiki/Media_player)
