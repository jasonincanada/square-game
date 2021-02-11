## Install notes

To install locally on linux, install Stack first: https://docs.haskellstack.org/en/stable/README/#how-to-install

Then clone and build this repo locally:

```bash
git clone https://github.com/jasonincanada/square-game.git
cd square-game
stack build
```

Load board #1 with:

```bash
stack exec square-game-exe 1
```

The rest of the board data must be extracted before it can be rendered:

```bash
cd generation/squares
unzip n8-squares.zip
```

Now you can load any of the valid boards by their ordinal number from 1 to 18656.


On Ubuntu 20 Desktop I had to bring the following dependencies up to date before it would build without errors:

```bash
sudo apt install libglvnd-dev freeglut3-dev
```

