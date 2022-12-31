# Snake

A Scala clone of Snake II as played on the Nokia 3310, for fun and nostalgia.
- Fun: I've tried to use the minimum possible amount of Swing code,
  and do pixel-by-pixel rendering instead.
- Nostalgia: I've tried to be visually accurate to capture the feel of
  the original game. Some of the behaviour (but none of the code) has
  been modelled by observing https://helpfulsheep.com/snake/.


## Demo

https://user-images.githubusercontent.com/7769582/210119719-a4196f70-2dd9-40ed-870c-867d72d0489d.mov


## Run

Install [scala-cli](https://scala-cli.virtuslab.org/install).
Then:

```
scala-cli Snake.scala
```

## Controls

| Command      | Description        |
|--------------|--------------------|
| **arrows**   | movement           |
| **1-9**      | change speed/level |
| **spacebar** | pause/resume       |
