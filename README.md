# gemgems

At some point to be a game about matching gems.

For now, just run elm-reactor to try.

Quick and dirty code (c) by first time Elm user.

## To dos
* Major changes
  * Implement score
  * Replace hexagon SVGs with "real" gem icons
* Notes on current topics
  * remove Nop again (does not have the desired effect)
  * stop increasing counter when model not dirty (generates useless model updates)
  * split DragEnd into
    * TouchDragEnd, to handle touch drag end position and get the target hex, and
    * DragEnd, receiving the target hex as previously.
  * make new TochDragEnd safer by calculating the distance from the center, and only accept the drop if the drop location is within the radius that does not overlap any other gems. (the used Hexagon library seems to have an issue in certain cases in border areas.)
 